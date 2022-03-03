{-# LANGUAGE RecordWildCards #-}

module Pos.Chain.Update.BlockVersion
       ( BlockVersion (..)
       , HasBlockVersion (..)
       ) where

import           Universum

import           Data.Aeson.TH (defaultOptions, deriveJSON)
import           Data.SafeCopy (base, deriveSafeCopySimple)
import qualified Formatting.Buildable as Buildable

import           Pos.Binary.Class (Cons (..), Field (..), deriveSimpleBi)
import           Pos.Util.Some (Some, liftLensSome)

-- | Communication protocol version.
data BlockVersion = BlockVersion
    { bvMajor :: !Word16
    , bvSentry :: !Word16
    } deriving (Eq, Generic, Ord, Typeable, Show)

instance Buildable BlockVersion where
    build BlockVersion{..} = fromString $ intercalate "." [show bvMajor, show bvSentry]

instance Hashable BlockVersion

instance NFData BlockVersion

class HasBlockVersion a where
    blockVersionL :: Lens' a BlockVersion

instance HasBlockVersion (Some HasBlockVersion) where
    blockVersionL = liftLensSome blockVersionL

deriveJSON defaultOptions ''BlockVersion

deriveSimpleBi ''BlockVersion [
    Cons 'BlockVersion [
        Field [| bvMajor :: Word16 |],
        Field [| bvSentry :: Word16 |]
    ]]

deriveSafeCopySimple 0 'base ''BlockVersion
