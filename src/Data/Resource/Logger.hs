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

log' :: (With cs, MonadIO m)
     => String
     -> LogLevel
     -> String
     -> m ()
log' = log'' ?cxt

log'' :: (MonadIO m)
      => Contexts cs
      -> String
      -> LogLevel
      -> String
      -> m ()
log'' contexts tag level s = do
    let (CBase logger) = baseOf contexts
    liftIO $ sequence $ map (\l -> l tag level format) logger
    return ()
    where
        format :: FormattedTime -> LogStr
        format t = mconcat $ toLogStr t : toLogStr (" [" :: ByteString) : toLogStr (show level) : toLogStr ("] " :: ByteString) : [toLogStr s]

logD' :: (With cs, MonadIO m)
      => String
      -> String
      -> m ()
logD' t = log' t LevelDebug

logI' :: (With cs, MonadIO m)
      => String
      -> String
      -> m ()
logI' t = log' t LevelInfo

logW' :: (With cs, MonadIO m)
      => String
      -> String
      -> m ()
logW' t = log' t LevelWarn

logE' :: (With cs, MonadIO m)
      => String
      -> String
      -> m ()
logE' t = log' t LevelError

logD :: (With cs, MonadIO m)
     => String
     -> m ()
logD = log' "" LevelDebug

logI :: (With cs, MonadIO m)
     => String
     -> m ()
logI = log' "" LevelInfo

logW :: (With cs, MonadIO m)
     => String
     -> m ()
logW = log' "" LevelWarn

logE :: (With cs, MonadIO m)
     => String
     -> m ()
logE = log' "" LevelError

logQ :: String
     -> LogLevel
     -> ExpQ
logQ tag level = [| \cxt s -> log'' cxt tag level $ s ++ "(" ++ $(qLocation >>= return . show >>= lift) ++ ")" |]

logQD' :: String
       -> ExpQ
logQD' t = logQ t LevelDebug

logQI' :: String
       -> ExpQ
logQI' t = logQ t LevelInfo

logQW' :: String
       -> ExpQ
logQW' t = logQ t LevelWarn

logQE' :: String
       -> ExpQ
logQE' t = logQ t LevelError

logQD :: ExpQ
logQD = logQ "" LevelDebug

logQI :: ExpQ
logQI = logQ "" LevelInfo

logQW :: ExpQ
logQW = logQ "" LevelWarn

logQE :: ExpQ
logQE = logQ "" LevelError