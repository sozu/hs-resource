{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.Resource.Logger where

import System.IO
import Control.Applicative
import Control.Monad.IO.Class
import Data.Maybe (maybe)
import Data.IORef
import Data.ByteString (ByteString)
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import System.Log.FastLogger
import Control.Monad.Logger
import Control.Exception.Safe
import Data.Resource.Resource

-- ----------------------------------------------------------------
-- Shared functions used internally.
-- ----------------------------------------------------------------

log' :: (MonadIO m)
     => Contexts cs
     -> String
     -> LogLevel
     -> String
     -> m ()
log' contexts tag level s = do
    let (CBase logger) = baseOf contexts
    liftIO $ sequence $ map (\l -> l tag level format) logger
    return ()
    where
        nl = case nativeNewline of
                LF -> "\n"
                CRLF -> "\r\n"
        format :: FormattedTime -> LogStr
        format t = mconcat $ toLogStr t : toLogStr (" [" :: ByteString) : toLogStr (show level) : toLogStr ("] " :: ByteString) : [toLogStr $ s ++ nl]

logQ :: String
     -> LogLevel
     -> ExpQ
logQ tag level = [| \cxt s -> log' cxt tag level $ s ++ "@ " ++ $(qLocation >>= return . formatLoc >>= lift) |]
    where
        -- log message ... @ Moddule.Name (filename : start-end)
        formatLoc loc = loc_module loc ++ " (" ++ loc_filename loc ++ " : " ++ show (loc_start loc) ++ "-" ++ show (loc_end loc) ++ ")"

-- ----------------------------------------------------------------
-- Logging functions with a tag.
-- ----------------------------------------------------------------

-- | Log debug level message with a tag.
logD' :: (With cs, MonadIO m)
      => String -- ^ Tag.
      -> String -- ^ Message.
      -> m () -- ^ No result.
logD' t = log' ?cxt t LevelDebug

-- | Log info level message with a tag.
logI' :: (With cs, MonadIO m)
      => String -- ^ Tag.
      -> String -- ^ Message.
      -> m () -- ^ No result.
logI' t = log' ?cxt t LevelInfo

-- | Log warn level message with a tag.
logW' :: (With cs, MonadIO m)
      => String -- ^ Tag.
      -> String -- ^ Message.
      -> m () -- ^ No result
logW' t = log' ?cxt t LevelWarn

-- | Log error level message with a tag.
logE' :: (With cs, MonadIO m)
      => String -- ^ Tag.
      -> String -- ^ Message.
      -> m () -- ^ No result.
logE' t = log' ?cxt t LevelError

-- ----------------------------------------------------------------
-- Logging functions with no tag.
-- ----------------------------------------------------------------

-- | Log debug level message.
logD :: (With cs, MonadIO m)
     => String -- ^ Message.
     -> m () -- ^ No result.
logD = logD' ""

-- | Log info level message.
logI :: (With cs, MonadIO m)
     => String -- ^ Message.
     -> m () -- ^ No result.
logI = logI' ""

-- | Log warn level message.
logW :: (With cs, MonadIO m)
     => String -- ^ Message.
     -> m () -- ^ No result.
logW = logW' ""

-- | Log error level message.
logE :: (With cs, MonadIO m)
     => String -- ^ Message.
     -> m () -- ^ No result.
logE = logE' ""

-- ----------------------------------------------------------------
-- Logging functions with a tag accepting contexts as an explicit argument.
-- ----------------------------------------------------------------

-- | Log debug level message with a tag.
logCD' :: (MonadIO m)
       => Contexts cs -- ^ Resource contexts.
       -> String -- ^ Tag.
       -> String -- ^ Message.
       -> m () -- ^ No result.
logCD' c t = log' c t LevelDebug

-- | Log info level message with a tag.
logCI' :: (MonadIO m)
       => Contexts cs -- ^ Resource contexts.
       -> String -- ^ Tag.
       -> String -- ^ Message.
       -> m () -- ^ No result.
logCI' c t = log' c t LevelInfo

-- | Log warn level message with a tag.
logCW' :: (MonadIO m)
       => Contexts cs -- ^ Resource contexts.
       -> String -- ^ Tag.
       -> String -- ^ Message.
       -> m () -- ^ No result
logCW' c t = log' c t LevelWarn

-- | Log error level message with a tag.
logCE' :: (MonadIO m)
       => Contexts cs -- ^ Resource contexts.
       -> String -- ^ Tag.
       -> String -- ^ Message.
       -> m () -- ^ No result.
logCE' c t = log' c t LevelError

-- ----------------------------------------------------------------
-- Logging functions with no tag accepting contexts as an explicit argument.
-- ----------------------------------------------------------------

-- | Log debug level message.
logCD :: (MonadIO m)
      => Contexts cs -- ^ Resource contexts.
      -> String -- ^ Message.
      -> m () -- ^ No result.
logCD c = logCD' c ""

-- | Log info level message.
logCI :: (MonadIO m)
      => Contexts cs -- ^ Resource contexts.
      -> String -- ^ Message.
      -> m () -- ^ No result.
logCI c = logCI' c ""

-- | Log warn level message.
logCW :: (MonadIO m)
      => Contexts cs -- ^ Resource contexts.
      -> String -- ^ Message.
      -> m () -- ^ No result.
logCW c = logCW' c ""

-- | Log error level message.
logCE :: (MonadIO m)
      => Contexts cs -- ^ Resource contexts.
      -> String -- ^ Message.
      -> m () -- ^ No result.
logCE c = logCE' c ""

{-| TH logging function logs message followed by source location information obtained by @qLocation@.

    Each function returns an expression of a function accepting contexts and a message string.
    The usage is as follows.
    > $(logQD' "tag") contexts "log message..."
    > $(logQD) contexts "log message..."
-}

-- ----------------------------------------------------------------
-- TH Logging functions with a tag.
-- ----------------------------------------------------------------

-- | Retuens an expression of function to log debug level message with given tag.
logQD' :: String -- ^ Tag.
       -> ExpQ -- ^ Expression.
logQD' t = logQ t LevelDebug

-- | Retuens an expression of function to log info level message with given tag.
logQI' :: String -- ^ Tag.
       -> ExpQ -- ^ Expression.
logQI' t = logQ t LevelInfo

-- | Retuens an expression of function to log warn level message with given tag.
logQW' :: String -- ^ Tag.
       -> ExpQ -- ^ Expression.
logQW' t = logQ t LevelWarn

-- | Retuens an expression of function to log error level message with given tag.
logQE' :: String -- ^ Tag.
       -> ExpQ -- ^ Expression.
logQE' t = logQ t LevelError

-- ----------------------------------------------------------------
-- TH Logging functions with no tag.
-- ----------------------------------------------------------------

-- | Retuens an expression of function to log debug level message.
logQD :: ExpQ -- ^ Expression.
logQD = logQ "" LevelDebug

-- | Retuens an expression of function to log info level message.
logQI :: ExpQ -- ^ Expression.
logQI = logQ "" LevelInfo

-- | Retuens an expression of function to log warn level message.
logQW :: ExpQ -- ^ Expression.
logQW = logQ "" LevelWarn

-- | Retuens an expression of function to log error level message.
logQE :: ExpQ -- ^ Expression.
logQE = logQ "" LevelError