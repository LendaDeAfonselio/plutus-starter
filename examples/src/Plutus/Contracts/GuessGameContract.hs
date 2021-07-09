module Plutus.Contracts.GuessGameContract where

-- | This contract was generated automatically using a scribble protocol as source:
import           Control.Lens 
import           Control.Monad                         (void, forever) 
import           Data.Aeson                            (FromJSON, ToJSON) 
import           Control.Monad.Error.Lens              (catching, throwing, throwing_) 
import           GHC.Generics                          (Generic) 
import qualified Language.PlutusTx                     as PlutusTx 
import           Language.PlutusTx.Prelude             
import           Ledger                                hiding (to,initialise)
import qualified Ledger.Ada                            as Ada  
import qualified Ledger.Value                          as V
import           Ledger.Constraints                    (TxConstraints)  
import qualified Ledger.Constraints                    as Constraints 
import qualified Ledger.Typed.Scripts                  as Scripts  
import           Ledger.Typed.Tx (tyTxOutData,TypedScriptTxOut(..))
import           Language.Plutus.Contract.StateMachine (AsSMContractError (..), OnChainState, State (..), Void) 
import qualified Language.Plutus.Contract.StateMachine as SM 
import           Data.Text                (Text) 
import qualified Data.Text                as T 
import qualified Data.ByteString.Char8     as C
import           Language.Plutus.Contract 
import           Playground.Contract
import qualified Prelude
import           GuessGameBusiness
import           GuessGameSMLibrary
import           GuessGameDomain


-- | Beginning of the boilerplate code for the State Machine
-- | TODO: Do not forget to add extra conditions to the transitions that are related to business logic if necessary
{-# INLINABLE transition #-}
transition :: State GuessingGameState -> GuessingGameInput -> Maybe (TxConstraints Void Void, State GuessingGameState)
transition State{stateData=oldData,stateValue} input = case (oldData, input) of
    (LockState s ,GuessInput) -> Just (mempty, State{stateData = LockState s, stateValue = Ada.lovelaceValueOf 0})
    (LockState _ ,CancelGameInput) -> Just(mempty, State{stateData = CancelGameState, stateValue = mempty})
    _ -> Nothing

{-# INLINABLE machine #-}
machine :: SM.StateMachine GuessingGameState GuessingGameInput
machine = SM.mkStateMachine transition isFinal where
    isFinal CancelGameState = True
    isFinal _       = False

{-# INLINABLE mkValidator #-} 
mkValidator :: Scripts.ValidatorType (SM.StateMachine GuessingGameState GuessingGameInput)
mkValidator = SM.mkValidator machine

scriptInstance :: Scripts.ScriptInstance (SM.StateMachine GuessingGameState GuessingGameInput)
scriptInstance = Scripts.validator @(SM.StateMachine GuessingGameState GuessingGameInput)
    $$(PlutusTx.compile [|| mkValidator ||])
    $$(PlutusTx.compile [|| wrap ||])
    where
        wrap = Scripts.wrapValidator @GuessingGameState @GuessingGameInput

machineInstance :: SM.StateMachineInstance GuessingGameState GuessingGameInput
machineInstance = SM.StateMachineInstance machine scriptInstance

client :: SM.StateMachineClient GuessingGameState GuessingGameInput
client = SM.mkStateMachineClient machineInstance
-- | End of the boilerplate code for the State Machine

contractAddress :: Ledger.Address
contractAddress = Ledger.scriptAddress (Scripts.validatorScript scriptInstance)

lock :: AsGuessingGameError e => Contract GuessingGameSchema e ()
lock = do
    -- | Receiving the parameters for this endpoint
    LockParams param1 param2 <- mapError (review _GuessingGameError) $ endpoint @"lock"

    business <- lockBusiness param1 param2
    case business of 
        Left (stg1,val2) -> do
            res <- mapError (review _GuessingGameSMError) $  initialiseSM client (LockState stg1) val2
            case res of
             True -> do
                  outcome <- mapError (review _GuessingGameError) $ fundsAtAddressLeq contractAddress (Ada.lovelaceValueOf 0)
                  logInfo @String "Someone has guessed the correct word... Closing the game"
                  void $ mapError (review _GuessingGameSMError) $ runContractStep client CancelGameInput
             _ ->  logWarn @String "Previous lock observed as such this lock did not have any effect"
        Right (BusinessError t) -> logWarn @Text t

guess :: AsGuessingGameError e => Contract GuessingGameSchema e ()
guess = do
    -- | Receiving the parameters for this endpoint
    (param1) <- mapError (review _GuessingGameError) $ endpoint @"guess"
    x <- guessBusiness param1
    case x of 
        Left () -> do
            void $ mapError (review _GuessingGameSMError) $ runContractStep client GuessInput
        Right (BusinessError t) -> logWarn @Text t

contract :: AsGuessingGameError e => Contract GuessingGameSchema e ()
contract = lock `select` guess

endpoints :: AsGuessingGameError e => Contract GuessingGameSchema e ()
endpoints = forever contract

PlutusTx.makeLift ''GuessingGameState
PlutusTx.makeLift ''GuessingGameInput
PlutusTx.makeLift ''HashedString
PlutusTx.makeIsData ''GuessingGameState
PlutusTx.makeIsData ''GuessingGameInput
mkSchemaDefinitions ''GuessingGameSchema