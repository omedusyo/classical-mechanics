module AsyncActor where

import Effect (Effect)

-- TODO: Should distinguish between sync/async?
-- Is there even a point of having a ReaderCapability? If we can send stuff,
-- we can simulate reading.
data ReaderCapability output
-- TODO: How does this reading work really?
-- readSync :: forall output . ReaderCapability output -> Effect output

-- readAsync :: forall output . ReaderCapability output -> (output -> Effect Unit) -> Effect Unit


-- ===Actor===

data SenderCapability msg

type Behavior msg privateModel =
  { init :: Effect privateModel
  -- Hmm... in general if we want to allow `become`,
  --        this output type won't do. We basically want behavior???
  --        Hmm, this `becoming` concept really complicates stuff.
  
  -- We atleast wish to be able to send messages to ourselves.
  , update :: SenderCapability msg -> msg -> privateModel -> Effect privateModel
  }

-- TODO: How to model `become`?
-- spawn :: forall msg privateModel . Behavior msg privateModel -> Eff (SenderCapability msg)

-- restrict :: (msgPublic -> msgPrivate ) -> SenderCapability msgPrivate -> SenderCapability msgPublic

-- asyncSend :: forall msg . msg -> SenderCapability msg -> Effect Unit

-- selfToSender :: Self msg -> SenderCapability msg

-- data Self msg privateModel
-- become :: Self publicMsg privateMsg0 privateMsg0 -> 



-- TODO:
--   The output is produced only on demand by the consumer?
--   Or is it forcibly emitted to everyone who is subscribed?

-- In Elm the `view` is like our `output`.
-- Seems like there the html it's definitely consumed on demand every frame.

-- Maybe both give rise to separate conceptions of automata?
-- Why is reading problematic? Sending seems fine... it's just async send.

-- Am I just doing a bad actor model? Actors with public output?

-- Let's just model what's in Elm.
--   Async sending of messages, just like in Actor Model.
--   And on demand reading of output. This is hidden in Elm.
--   Also in Elm we have exactly one big-ass actor. Very strange.

-- Hmm... the output 
-- output :: privateModel -> publicOutput
