module Plutus.Contracts.GuessGameSMLibrary where 

import           Ledger                                hiding (to,initialise)
import           Ledger.Typed.Tx (tyTxOutData,TypedScriptTxOut(..))
import           Language.Plutus.Contract.StateMachine (AsSMContractError (..), OnChainState, State (..), Void)
import qualified Language.Plutus.Contract.StateMachine as SM
import Plutus.Contracts.GuessGameDomain

--- Another module
getCurrentStateSM :: SM.StateMachineClient GuessingGameState GuessingGameInput -> Contract GuessingGameSchema SM.SMContractError (Maybe GuessingGameState)
getCurrentStateSM client = do
    currentState <- SM.getOnChainState client
    case currentState of
        Just ((TypedScriptTxOut{tyTxOutData=x}, _), _) -> pure (Just x)
        _ -> pure Nothing

runContractStep :: SM.StateMachineClient GuessingGameState GuessingGameInput -> GuessingGameInput -> Contract GuessingGameSchema SM.SMContractError ()
runContractStep client input =
    do
    r <- SM.runStep client input
    case r of
            SM.TransitionFailure i                 -> logError ("Invalid State for" <> show input ++ ". State:" <> show i)
            SM.TransitionSuccess s                 -> logInfo ("Successful transaction to state: " <> show s)

initialiseSM :: SM.StateMachineClient GuessingGameState GuessingGameInput -> GuessingGameState -> Value -> Contract GuessingGameSchema SM.SMContractError Bool
initialiseSM client initState value = do
    currentState <- SM.getOnChainState client
    case currentState of
        Nothing -> do
            SM.runInitialise client initState value
            logInfo @String $ "No previous state found, initialising SM with state: " <> show initState ++ "and with value: " <> show value
            pure True
        _ -> do
            logWarn @String "The State Machine was already initialised! Impossible to continue with this operation"
            pure False