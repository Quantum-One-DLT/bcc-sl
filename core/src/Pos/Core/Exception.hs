{-# LANGUAGE ExistentialQuantification #-}

-- | Exceptions hierarchy in bcc-sl.

module Pos.Core.Exception
       ( BccException (..)
       , bccExceptionToException
       , bccExceptionFromException

       , BccFatalError (..)
       , reportFatalError
       , assertionFailed
       ) where

import           Control.Exception.Safe (Exception (..))
import           Data.Typeable (cast)
import           Formatting (bprint, stext, (%))
import qualified Formatting.Buildable
import           Pos.Util.Wlog (WithLogger, logError)
import           Serokell.Util (Color (Red), colorize)
import qualified Text.Show
import           Universum

-- | Root of exceptions in bcc-sl.
data BccException =
    forall e. (Buildable e, Exception e) =>
              BccException e
    deriving (Typeable)

instance Show BccException where
    show (BccException e) = toString . pretty $ e

instance Exception BccException

instance Buildable BccException where
    build (BccException e) = Formatting.Buildable.build e

-- | Helper to define sub-exception of BccException.
bccExceptionToException :: (Buildable e, Exception e) => e -> SomeException
bccExceptionToException = toException . BccException

-- | Helper to define sub-exception of BccException.
bccExceptionFromException :: Exception e => SomeException -> Maybe e
bccExceptionFromException x = do
    BccException a <- fromException x
    cast a


-- | Error indicating that something really bad happened. Should be
-- used when serious assertions fail (local equivalent of
-- 'panic'). 'panic' is still alright to use, but preferably in pure
-- environment.
data BccFatalError =
    BccFatalError !Text
    deriving (Typeable, Show)

instance Buildable BccFatalError where
    build (BccFatalError msg) =
        bprint ("Bcc fatal error: "%stext) msg

instance Exception BccFatalError where
    toException = bccExceptionToException
    fromException = bccExceptionFromException
    displayException = toString . pretty

-- | Print red message about fatal error and throw exception.
reportFatalError
    :: (WithLogger m, MonadThrow m)
    => Text -> m a
reportFatalError msg = do
    logError $ colorize Red msg
    throwM $ BccFatalError msg

-- | Report 'BccFatalError' for failed assertions.
assertionFailed :: (WithLogger m, MonadThrow m) => Text -> m a
assertionFailed msg =
    reportFatalError $ "assertion failed: " <> msg
