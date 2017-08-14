module Test.Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Ref (REF)
import SimpleEmitter (emit, emitter, subscribe, unsubscribe)



data Event
  = Create
  | Update
  | Destroy

derive instance eqEvent :: Eq Event
derive instance ordEvent :: Ord Event



main :: forall e. Eff (console :: CONSOLE, ref :: REF | e) Unit
main = do
  e <- emitter
  subscribe Create log1 e
  emit Create e
  log "======================"
  subscribe Update log2 e
  emit Update e
  log "======================"
  subscribe Update log3 e
  emit Update e
  log "======================"
  emit Destroy e
  log "======================"
  unsubscribe Create e
  unsubscribe Update e
  unsubscribe Destroy e
  emit Create e
  log "======================"
  emit Update e
  log "======================"



log1 :: forall e. Eff (console :: CONSOLE | e) Unit
log1 = log "log 1"



log2 :: forall e. Eff (console :: CONSOLE | e) Unit
log2 = log "log 2"



log3 :: forall e. Eff (console :: CONSOLE | e) Unit
log3 = log "log 3"
