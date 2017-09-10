module Test.Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception
import Factorio

-- TODO: extend

main :: forall e. Eff (exception :: EXCEPTION | e) Unit
main = do
  --- Calculation of number of factories
  -- one with productivity modules
  if factories ironGearWheel 2.8 1.0 == 2.0
      then pure unit
      else throw "Number of factories not calculated correctly"

  -- one without productivity modules
  if factories pipe 2.5 1.0 == 1.0
      then pure unit
      else throw "Number of factories not calculated correctly"

  -- Number of processes in factories with productivity modules
  if processes 28.0 ironGearWheel == 20.0
      then pure unit
      else throw "Number of processes to create 28 iron gear wheels not calculated correctly"

  -- Speed with productivity modules
  if factorySpeedWithProductivity Assembler == 0.5
      then pure unit
      else throw "Factory speed with productivity modules not calculated correctly"

  if factorySpeedWithProductivity RocketSilo == 0.4
      then pure unit
      else throw "Factory speed with productivity modules not calculated correctly"

