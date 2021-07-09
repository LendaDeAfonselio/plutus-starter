module Plutus.Contracts.GuessingGameLogic
  ( lock
  , guess
  , closeGame
  ) where

import           Control.Lens
import           Control.Monad                         (void)
import           GHC.Generics                          (Generic)
import           Language.PlutusTx.Prelude
import qualified Ledger.Ada                            as Ada
import qualified Ledger.Value                          as V
import qualified Data.Text                as T
import qualified Data.ByteString.Char8    as C
import qualified Plutus.Contract.GuessGameDomain         as G
import           Plutus.Contract.GuessGameClient
hashString :: String -> HashedString
hashString = HashedString . sha2_256 . C.pack

zeroLovelace :: Value
zeroLovelace = Ada.lovelaceValueOf 0

lock :: G.AsError e => String -> Value -> Contract G.Schema e (Either (HashedString, V.Value) G.Error)
lock str val = pure $
  if  val `V.leq` zeroLovelace 
    then Right $ Error $ T.pack $ "The prize must be greater than 0; got " <> show val
    else Left (hashString str, val)

guess :: G.AsError e => String -> Contract G.Schema e (Either (HashedString, V.Value) G.Error)
guess str = do
    secret <- mapError (review _GuessingGameSMError) $ G.getCurrentStateSM G.client
    if secret == hashString str
      then do
        logInfo @String "Congratulations, you won!"
        pure $ Left (hashString str, zeroLovelace)
      else pure $ Right $ Error "Incorrect guess, try again"

closeGame :: G.AsError e => Contract G.Schema e (Either (HashedString, V.Value) G.Error)
closeGame = do 
  logInfo @String "Closing the game"
  pure $ Left (hashString "Game over", zeroLovelace)

