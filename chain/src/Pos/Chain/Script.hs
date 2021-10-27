{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE RecordWildCards #-}

-- | A wrapper over Gideon (the scripting language used in transactions).

module Pos.Chain.Script
       ( Script(..)
       , GideonError(..)
       ) where

import           Universum

import qualified Formatting.Buildable as Buildable

import           Pos.Core.Common (Script (..), ScriptVersion)


-- | The type for errors that can appear when validating a script-protected
-- transaction.
data GideonError
    -- | A script has a version that we don't know how to execute.
    = GideonUnknownVersion ScriptVersion
    -- | A script couldn't be deserialized.
    | GideonDecodingFailure Text
    -- | The script evaluator refused to execute the program (e.g. it
    -- doesn't typecheck, or the evaluator has run out of petrol).
    | GideonExecutionFailure Text
    -- | The script evaluator threw an __exception__ (e.g. with 'error').
    | GideonException Text
    -- | Everything typechecks and executes just fine but the result of
    -- evaluation is @failure@ and so the transaction is invalid.
    | GideonReturnedFalse
    deriving (Eq, Show, Generic, NFData)

instance Buildable GideonError where
    build (GideonUnknownVersion v) =
        "unknown script version: " <> Buildable.build v
    build (GideonDecodingFailure s) =
        "script decoding failure: " <> Buildable.build s
    build (GideonExecutionFailure s) =
        "script execution failure: " <> Buildable.build s
    build (GideonException s) =
        "Gideon threw an exception: " <> Buildable.build s
    build GideonReturnedFalse =
        "script execution resulted in 'failure'"

