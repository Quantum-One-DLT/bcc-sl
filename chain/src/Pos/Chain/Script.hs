{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE RecordWildCards #-}

-- | A wrapper over Zerepoch (the scripting language used in transactions).

module Pos.Chain.Script
       ( Script(..)
       , ZerepochError(..)
       ) where

import           Universum

import qualified Formatting.Buildable as Buildable

import           Pos.Core.Common (Script (..), ScriptVersion)


-- | The type for errors that can appear when validating a script-protected
-- transaction.
data ZerepochError
    -- | A script has a version that we don't know how to execute.
    = ZerepochUnknownVersion ScriptVersion
    -- | A script couldn't be deserialized.
    | ZerepochDecodingFailure Text
    -- | The script evaluator refused to execute the program (e.g. it
    -- doesn't typecheck, or the evaluator has run out of petrol).
    | ZerepochExecutionFailure Text
    -- | The script evaluator threw an __exception__ (e.g. with 'error').
    | ZerepochException Text
    -- | Everything typechecks and executes just fine but the result of
    -- evaluation is @failure@ and so the transaction is invalid.
    | ZerepochReturnedFalse
    deriving (Eq, Show, Generic, NFData)

instance Buildable ZerepochError where
    build (ZerepochUnknownVersion v) =
        "unknown script version: " <> Buildable.build v
    build (ZerepochDecodingFailure s) =
        "script decoding failure: " <> Buildable.build s
    build (ZerepochExecutionFailure s) =
        "script execution failure: " <> Buildable.build s
    build (ZerepochException s) =
        "Zerepoch threw an exception: " <> Buildable.build s
    build ZerepochReturnedFalse =
        "script execution resulted in 'failure'"

