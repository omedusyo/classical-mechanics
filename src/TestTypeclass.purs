module TestTypeclass where

data Pair :: Type -> Type -> Type
data Pair a b = Pair a b

class MyFunctor f where
  myMap :: forall a b . (a -> b) -> f a -> f b

class MyTuplable f where
  myPair :: forall a b . f a -> f b -> f (Pair a b)

class MyMonad f where
  myPure :: forall a . a -> f a
  myAndThen :: forall a b . (a -> f b) -> f a -> f b

data MyList a =
    Empty
  | Cons a (MyList a)

concat :: forall a . MyList a -> MyList a -> MyList a
concat xs ys =
  case xs of
    Empty -> ys
    Cons x xs' -> Cons x (concat xs' ys)

instance listFunctor :: MyFunctor MyList where
  myMap f xs =
    case xs of
      Empty -> Empty
      Cons x xs' -> Cons (f x) (myMap f xs')

instance listMonad :: MyMonad MyList where
  myPure x = Cons x Empty

  myAndThen f xs =
    case xs of
      Empty -> Empty
      Cons x xs' -> concat (f x) (myAndThen f xs')

instance applicativeFromMonad :: MyMonad f => MyTuplable f where
  myPair fa fb =
    myAndThen (\a -> myAndThen (\b -> myPure (Pair a b)) fb) fa

