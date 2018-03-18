module Data.Resource (
    LoggingResource
    , newLoggingResource
    , Logger
    , anyTag
    , denyTag
    , ResourceContext(..)
    , Resource(..)
    , Resources(..)
    , ResourceOf(..)
    , ContextTypes(..)
    , Contexts(..)
    , ContextResources(..)
    , Refs
    , SelectContexts(..)
    , With
    , WithContext(..)
    , with
    , logD, logI, logW, logE
    , logD', logI', logW', logE'
    , logCD, logCI, logCW, logCE
    , logCD', logCI', logCW', logCE'
    , logQD, logQI, logQW, logQE
    , logQD', logQI', logQW', logQE'
) where

import Data.Resource.Resource
import Data.Resource.Logger