module SimpleEmitter
  ( Emitter
  , createEmitter
  , subscribe
  , unsubscribe
  , emit
  ) where

import Prelude

import Effect (Effect)
import Effect.Ref (Ref, read, new, modify_)
import Data.Foldable (sequence_)
import Data.Map (Map, empty, insert, lookup, delete)
import Data.Maybe (Maybe(..))

newtype Emitter k = Emitter (Ref (Map k (Effect Unit)))

createEmitter :: forall k. Effect (Emitter k)
createEmitter = Emitter <$> new empty

subscribe :: forall k. Ord k => k -> Effect Unit -> Emitter k -> Effect Unit
subscribe k f (Emitter ref) = do
  listeners <- read ref
  flip modify_ ref case lookup k listeners of
    Nothing -> insert k f
    Just f' -> insert k $ sequence_ [ f', f ]

unsubscribe :: forall k. Ord k => k -> Emitter k -> Effect Unit
unsubscribe k (Emitter ref) = flip modify_ ref $ delete k

emit :: forall k. Ord k => k -> Emitter k -> Effect Unit
emit k (Emitter ref) = do
  listeners <- read ref
  case lookup k listeners of
    Nothing -> pure unit
    Just f -> f
