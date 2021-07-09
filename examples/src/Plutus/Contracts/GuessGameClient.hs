module Plutus.Contracts.GuessGameClient where

import Plutus.Contract.GuessGameContract (machineInstance)
import qualified Language.Plutus.Contract.StateMachine as SM

client :: SM.StateMachineClient GuessingGameState GuessingGameInput
client = SM.mkStateMachineClient machineInstance
-- | End of the boilerplate code for the State Machine