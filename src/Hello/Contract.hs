{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# Language OverloadedStrings #-}
{-# LANGUAGE DerivingStrategies #-}
module Hello.Contract where

import Codec.Serialise (serialise)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Short as BSS
import Hello.Shared (validatorHash, wrap)
import qualified Plutus.V1.Ledger.Scripts as Scripts
import Plutus.V2.Ledger.Api (ScriptContext)
import PlutusTx
import PlutusTx.Prelude
import Cardano.Api
import Plutus.V1.Ledger.Api (POSIXTime, Address(..))
import qualified Plutus.V2.Ledger.Contexts as Contexts
import qualified Plutus.V2.Ledger.Api as PlutusV2
import Plutus.V2.Ledger.Api (Credential(..))
import Plutus.V1.Ledger.Interval
import Cardano.Api.Shelley (PlutusScript(..))
import Plutus.V2.Ledger.Api (TxInfo)
import Plutus.V2.Ledger.Contexts (findDatum)


{-# INLINEABLE mustFindScriptDatum #-}
mustFindScriptDatum :: (UnsafeFromData d) => PlutusV2.TxOut -> TxInfo -> d
mustFindScriptDatum o info = case PlutusV2.txOutDatum o of
  -- inline datum 
  PlutusV2.OutputDatum (PlutusV2.Datum dat) -> PlutusTx.unsafeFromBuiltinData dat
  -- datum hash
  PlutusV2.OutputDatumHash dh -> case findDatum dh info of
    Just (PlutusV2.Datum dat) -> PlutusTx.unsafeFromBuiltinData dat
    _ -> error ()
  _ -> error ()

newtype HelloDatum = HelloDatum Integer
PlutusTx.unstableMakeIsData ''HelloDatum

newtype HelloRedeemer = HelloRedeemer Integer
PlutusTx.unstableMakeIsData ''HelloRedeemer

run :: HelloDatum -> HelloRedeemer -> ScriptContext -> Bool
run (HelloDatum datum) (HelloRedeemer redeemer) _ = redeemer < datum

runWithoutContext :: HelloDatum -> HelloRedeemer -> Bool
runWithoutContext (HelloDatum datum) (HelloRedeemer redeemer) = redeemer < datum

-- Entry

-- mkValidator :: HelloRedeemer -> HelloDatum -> ScriptContext -> () 
-- mkValidator = 

wrappedWithoutContext :: BuiltinData -> BuiltinData -> BuiltinData -> ()
wrappedWithoutContext datum redeemer _ = check $ runWithoutContext (unsafeFromBuiltinData datum) (unsafeFromBuiltinData redeemer)

wrapped :: BuiltinData -> BuiltinData -> BuiltinData -> ()
wrapped = wrap run

validator :: Scripts.Validator
-- validator = Scripts.mkValidatorScript $$(PlutusTx.compile [||wrapped||])
validator = Scripts.mkValidatorScript $$(PlutusTx.compile [||wrappedWithoutContext||])

-- serialized :: PlutusScript PlutusScriptV1
-- serialized = PlutusScriptSerialised . BSS.toShort . BSL.toStrict . serialise $ validator

hash :: Scripts.ValidatorHash
hash = validatorHash validator

----------------------------------------------

data MathBountyDatum = MBD 
                     { mbBounty    :: Integer
                     , mbDeadline :: POSIXTime }  

PlutusTx.makeIsDataIndexed ''MathBountyDatum [('MBD,0)]        -- At compile time write an instance of this data type (MyWonderFullRedeemer) on the IsData typeclass

{-# INLINABLE mathBountyValidator  #-}
mathBountyValidator :: MathBountyDatum -> Integer -> ScriptContext -> Bool
mathBountyValidator datum x sContext = traceIfFalse "Wrong guess!" ((mbBounty datum) == x*x) &&
                                       traceIfFalse "Deadline passed!" deadlineReached
       where
            somedatum :: MathBountyDatum 
            somedatum = mustFindScriptDatum @MathBountyDatum (head (PlutusV2.txInfoOutputs info)) info

            info :: Contexts.TxInfo
            info = Contexts.scriptContextTxInfo sContext

            deadlineReached :: Bool
            deadlineReached = contains (to $ mbDeadline datum) $ PlutusV2.txInfoValidRange info
                           -- (to $ mbDeadline datum) `contains` (txInfoValidRange info)






bountyWrapped :: BuiltinData -> BuiltinData -> BuiltinData -> ()
bountyWrapped = wrap mathBountyValidator

bountyValidator :: Scripts.Validator
bountyValidator = Scripts.mkValidatorScript $$(PlutusTx.compile [||bountyWrapped||])                 

bountyValHash :: Scripts.ValidatorHash
bountyValHash = validatorHash bountyValidator                      

bountyAddress :: Plutus.V1.Ledger.Api.Address
bountyAddress = Address (ScriptCredential bountyValHash) Nothing                -- New function to derive the address, included in the Utils library
--bountyAddress = scriptAddress valiator 

serialized :: PlutusScript PlutusScriptV1
serialized = PlutusScriptSerialised . BSS.toShort . BSL.toStrict . serialise $ bountyValidator