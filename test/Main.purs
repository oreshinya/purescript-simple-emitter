module Test.Main where

import Prelude

import Effect (Effect)
import Effect.Console (log)
import SimpleEmitter (emit, createEmitter, subscribe, unsubscribe)

data Event
  = Create
  | Update
  | Destroy

derive instance eqEvent :: Eq Event
derive instance ordEvent :: Ord Event

main :: Effect Unit
main = do
  e <- createEmitter
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

log1 :: Effect Unit
log1 = log "log 1"

log2 :: Effect Unit
log2 = log "log 2"

log3 :: Effect Unit
log3 = log "log 3"
