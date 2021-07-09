module Plutus.Contracts.GuessGameDomain where

import           Control.Lens
import           Data.Aeson                            (FromJSON, ToJSON)
import           Language.Plutus.Contract.StateMachine (AsSMContractError (..), OnChainState, State (..), Void)
import qualified Language.Plutus.Contract.StateMachine as SM

-- | Parameters for the "lock" endpoint
data LockParams = LockParams
    {amount     :: Value, 
    secretWord :: String
    }
    deriving stock (Prelude.Eq, Prelude.Show, Generic)
    deriving anyclass (FromJSON, ToJSON, IotsType, ToSchema, ToArgument)

-- | Contract schema: Endpoint and the parameters they receive
type GuessingGameSchema = 
    BlockchainActions
        .\/ Endpoint "guess" (String)
        .\/ Endpoint "lock" LockParams

newtype HashedString = HashedString ByteString deriving newtype PlutusTx.IsData
 deriving (Eq,Show)

-- PlutusTx.makeLift ''HashedString
-- needs to be in the contract module

-- | Declaration of the possible states for the State Machine:
data GuessingGameState =
    CancelGameState
    | LockState HashedString
    | GuessState
    deriving stock (Show, Generic)

-- | Declaration of the inputs that will be used for the transitions of the State Machine
data GuessingGameInput =
    CancelGameInput
    | LockInput
    | GuessInput
    deriving stock (Show, Generic)

-- | Declaration of the errors that will be used throughout this contract
data GuessingGameError =
    GuessingGameContractError ContractError
    | GuessingGameSMError SM.SMContractError
    | Error Text
    | StoppedUnexpectedly
    deriving stock (Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

makeClassyPrisms ''GuessingGameError

instance AsSMContractError GuessingGameError where
    _SMContractError = _GuessingGameSMError

instance AsContractError GuessingGameError where
    _ContractError = _GuessingGameContractError

instance AsGuessingGameError Text where
    _GuessingGameError = prism' (T.pack . show) (const Nothing)