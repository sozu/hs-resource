module Data.Resource (
    LoggingResource
    , newLoggingResource
    , newLogger, newLoggers
    , Logger
    , LoggerSettings
    , GetContextLogger
    , anyTag
    , denyTag
    , ResourceContext(..)
    , Resource(..)
    , Resources(..)
    , (@+)
    , ResourceOf(..)
    , ContextTypes(..)
    , Contexts(..)
    , closeAll, failAll
    , ContextResources(..)
    , ContextOf(..)
    , Refs
    , SelectContexts(..)
    , With
    , WithContext(..)
    , withContext'
    , with
    , logD, logI, logW, logE
    , logD', logI', logW', logE'
    , logCD, logCI, logCW, logCE
    , logCD', logCI', logCW', logCE'
    , logQD, logQI, logQW, logQE
    , logQD', logQI', logQW', logQE'
    , LogType(..), defaultBufSize
    , LogLevel(..)
) where

import Data.Resource.Resource
import Data.Resource.Logger
import System.Log.FastLogger (LogType(..), defaultBufSize)
import Control.Monad.Logger (LogLevel(..))