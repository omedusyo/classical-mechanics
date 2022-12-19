module Effect.Free (List, concat) where

-- ===List===
data List a =
    Nil
  | Cons a (List a)

concat :: forall a . List a -> List a -> List a
concat xs ys =
  case xs of
    Nil -> ys
    Cons x xs' -> Cons x (concat xs' ys)

listFlatMap :: forall a b . (a -> List b) -> List a -> List b
listFlatMap f xs =
  case xs of
    Nil -> Nil
    Cons x xs' -> concat (f x) (listFlatMap f xs')

-- ===Maybe===
data Option a =
    Nothing
  | Some a 

optionFlatMap :: forall a b . (a -> Option b) -> Option a -> Option b
optionFlatMap f ma =
  case ma of
    Nothing -> Nothing
    Some a -> f a

-- ===Stateful===
newtype Stateful s a =
  Stateful (s -> { state :: s, value :: a })

statefulFlatMap :: forall s a b . (a -> Stateful s b) -> Stateful s a -> Stateful s b
statefulFlatMap f (Stateful stateful) =
  Stateful (\s ->
    let
        stateful' = stateful s
        Stateful stateful'' = f stateful'.value
     in
        stateful'' stateful'.state
  )

statefulPure :: forall s a . a -> Stateful s a
statefulPure a =
  Stateful (\s -> { state: s, value: a })

-- ===Writer===
newtype Writer m a = Writer { action :: m, value :: a }

writerFlatMap :: forall m a b . Monoid m => (a -> Writer m b) -> Writer m a -> Writer m b
writerFlatMap f (Writer wr) =
  let
      Writer wr' = f wr.value
   in
   Writer { action: combine wr.action wr'.action , value: wr'.value }

writerPure :: forall m a . Monoid m => a -> Writer m a
writerPure a =
  Writer { action: neutral, value: a }

-- ===Monoid===
class Monoid m where
  combine :: m -> m -> m
  neutral :: m

instance listMonoid :: Monoid (List a) where
  combine = concat
  neutral = Nil

-- ===Monad===
class Monad m where
  flatMap :: forall a b . (a -> m b) -> m a -> m b
  pure :: forall a . a -> m a

instance listMonad :: Monad List where
  flatMap = listFlatMap
  pure x = Cons x Nil

instance optionMonad :: Monad Option where
  flatMap = optionFlatMap
  pure x = Some x

instance statefulMonad :: Monad (Stateful s) where
  flatMap = statefulFlatMap
  pure = statefulPure

instance writerMonad :: Monoid m => Monad (Writer m) where
  flatMap = writerFlatMap
  pure = writerPure

-- ===Functor===
class Functor f where
  map :: forall a b . (a -> b) -> f a -> f b

-- instance monadToFunctor :: Monad m => Functor m where
--   map f fa =
--     flatMap (\a -> pure (f a)) fa


-- ===Monoidal===
class Monoidal f where
  map2 :: forall a0 a1 b . (a0 -> a1 -> b) -> f a0 -> f a1 -> f b

instance monadToMonoidal :: Monad f => Monoidal f where
  map2 f fa fb =
    flatMap (\a -> flatMap (\b -> pure (f a b)) fb) fa

-- ===Free Concrete===
data WriteReadCmd :: Type -> Type
data WriteReadCmd a =
    WriteThen String ({} -> WriteReadCmd a)
  | ReadThen (String -> WriteReadCmd a)
  | WriteReadPure a
    
writeReadFlatMap :: forall a b . (a -> WriteReadCmd b) -> WriteReadCmd a -> WriteReadCmd b
writeReadFlatMap f cmd =
  case cmd of
    WriteThen outputStr k ->
      WriteThen outputStr (\out -> writeReadFlatMap f (k out))
    ReadThen k ->
      ReadThen (\inputStr -> writeReadFlatMap f (k inputStr))
    WriteReadPure a ->
      f a

instance Monad WriteReadCmd where
  flatMap = writeReadFlatMap
  pure = WriteReadPure

example0 :: WriteReadCmd String
example0 =
  WriteThen "hello" (\{} ->
    WriteThen "yo" (\{} ->
      ReadThen (\input ->
        WriteReadPure input
      )
    )
  )

-- ===Free Monad over a Polynomial Functor===
data Command :: (Type -> Type) -> Type -> Type
data Command f a =
    AtomicCommandThen (f (Command f a))
  | Pure a 

commandFlatMap :: forall (f :: Type -> Type) a b . Functor f => (a -> Command f b) -> Command f a -> Command f b
commandFlatMap f cmd =
  case cmd of
    AtomicCommandThen faExpr ->
      AtomicCommandThen (map (commandFlatMap f) faExpr)
    Pure a ->
      f a

instance freeCommandsOverFunctor :: Functor f => Monad (Command f) where
  flatMap = commandFlatMap
  pure = Pure

-- ===Example===
data WriteReadAtomicCmd :: Type -> Type
data WriteReadAtomicCmd cmd =
    AtomicWriteThen String ({} -> cmd)
  | AtomicReadThen (String -> cmd)

writeReadAtomicMap :: forall a b . (a -> b) -> WriteReadAtomicCmd a -> WriteReadAtomicCmd b
writeReadAtomicMap f op =
    case op of
      AtomicWriteThen outputStr k -> AtomicWriteThen outputStr (\unit -> f (k unit))
      AtomicReadThen k -> AtomicReadThen (\inputStr -> f (k inputStr))

-- TODO: Why does this give overlappign type class error?
instance writeReadAtomicCmdFunctor :: Functor WriteReadAtomicCmd where
  map = writeReadAtomicMap



example1 :: Command WriteReadAtomicCmd String
example1 =
  AtomicCommandThen (AtomicWriteThen "hello" (\{} ->
    AtomicCommandThen (AtomicWriteThen "yo" (\{} ->
      AtomicCommandThen (AtomicReadThen (\input ->
        Pure input
      ))
    ))
  ))


liftF :: forall (f :: Type -> Type) a . Functor f => f a -> Command f a
liftF fa =
  AtomicCommandThen (map (\a -> Pure a) fa)

write :: String -> Command WriteReadAtomicCmd {}
-- write outputStr = AtomicCommandThen (AtomicWriteThen outputStr Pure)
write outputStr = liftF (AtomicWriteThen outputStr (\out -> out))

read :: Command WriteReadAtomicCmd String
-- read = AtomicCommandThen (AtomicReadThen Pure)
read = liftF (AtomicReadThen (\out -> out))


bind :: forall (m :: Type -> Type) a b . Monad m => m a -> (a -> m b) -> m b
bind ma f = flatMap f ma

example2 :: Command WriteReadAtomicCmd String
example2 = do
  _ <- write "hello"
  _ <- write "yo"
  input <- read
  Pure input

