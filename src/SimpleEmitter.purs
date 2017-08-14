module SimpleEmitter
  ( Emitter
  , emitter
  , subscribe
  , unsubscribe
  , emit
  ) where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Ref (REF, Ref, readRef, newRef, modifyRef)
import Data.Foldable (sequence_)
import Data.Map (Map, empty, insert, lookup, delete)
import Data.Maybe (Maybe(..))



newtype Emitter e k = Emitter (Ref (Map k (Eff e Unit)))



emitter :: forall e k. Eff (ref :: REF | e) (Emitter (ref :: REF | e) k)
emitter = map Emitter $ newRef empty



subscribe :: forall e k. Ord k => k -> Eff (ref :: REF | e) Unit -> Emitter (ref :: REF | e) k -> Eff (ref :: REF | e) Unit
subscribe k f (Emitter ref) = do
  listeners <- readRef ref
  case lookup k listeners of
    Nothing -> modifyRef ref $ insert k f
    Just f' -> modifyRef ref $ insert k $ sequence_ [ f', f ]



unsubscribe :: forall e k. Ord k => k -> Emitter (ref :: REF | e) k -> Eff (ref :: REF | e) Unit
unsubscribe k (Emitter ref) = modifyRef ref $ delete k



emit :: forall e k. Ord k => k -> Emitter (ref :: REF | e) k -> Eff (ref :: REF | e) Unit
emit k (Emitter ref) = do
  listeners <- readRef ref
  case lookup k listeners of
    Nothing -> pure unit
    Just f -> f
