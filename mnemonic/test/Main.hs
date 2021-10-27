module Main where

import           Universum

import           Test.Hspec

import qualified Bcc.MnemonicSpec


main :: IO ()
main = hspec $ do
    Bcc.MnemonicSpec.spec
