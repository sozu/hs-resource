{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Resource.LoggerSpec where

import Test.Hspec
import Control.Monad.IO.Class
import Control.Monad.Logger
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C8
import System.Log.FastLogger
import Data.IORef
import Data.Resource.Resource
import Data.Resource.Logger
import Control.Exception.Safe

logIORef :: IORef [B.ByteString] -> (LogStr -> IO())
logIORef ref = \s -> modifyIORef ref (fromLogStr s:)

getContexts :: LogLevel
            -> [String]
            -> IORef [B.ByteString]
            -> IO (Contexts '[], IORef [B.ByteString])
getContexts level tags ref = do
    r <- newLoggingResource [(tags, level, LogCallback (logIORef ref) (return ()), Just "")] >>= newIORef
    (, ref) <$> generateContexts (r `RCons` RNil)

removeNL :: Char
         -> B.ByteString
removeNL '\r' = ""
removeNL '\n' = ""
removeNL c = C8.singleton c

spec :: Spec
spec = do
    describe "Loggers" $ do
        it "Output debug log" $ do
            (cxt, buf) <- newIORef [] >>= getContexts LevelDebug anyTag
            log' cxt "" LevelDebug "1"
            log' cxt "" LevelInfo "2"
            log' cxt "" LevelWarn "3"
            log' cxt "" LevelError "4"
            res <- readIORef buf
            map (C8.concatMap removeNL) res `shouldBe` reverse [" [LevelDebug] 1", " [LevelInfo] 2", " [LevelWarn] 3", " [LevelError] 4"]

        it "Output info log" $ do
            (cxt, buf) <- newIORef [] >>= getContexts LevelInfo anyTag
            log' cxt "" LevelDebug "1"
            log' cxt "" LevelInfo "2"
            log' cxt "" LevelWarn "3"
            log' cxt "" LevelError "4"
            res <- readIORef buf
            map (C8.concatMap removeNL) res `shouldBe` reverse [" [LevelInfo] 2", " [LevelWarn] 3", " [LevelError] 4"]

        it "Output warn log" $ do
            (cxt, buf) <- newIORef [] >>= getContexts LevelWarn anyTag
            log' cxt "" LevelDebug "1"
            log' cxt "" LevelInfo "2"
            log' cxt "" LevelWarn "3"
            log' cxt "" LevelError "4"
            res <- readIORef buf
            map (C8.concatMap removeNL) res `shouldBe` reverse [" [LevelWarn] 3", " [LevelError] 4"]

        it "Output error log" $ do
            (cxt, buf) <- newIORef [] >>= getContexts LevelError anyTag
            log' cxt "" LevelDebug "1"
            log' cxt "" LevelInfo "2"
            log' cxt "" LevelWarn "3"
            log' cxt "" LevelError "4"
            res <- readIORef buf
            map (C8.concatMap removeNL) res `shouldBe` reverse [" [LevelError] 4"]

        it "Output debug log with tag" $ do
            (cxt, buf) <- newIORef [] >>= getContexts LevelDebug ["a"]
            log' cxt "" LevelDebug "1"
            log' cxt "a" LevelDebug "2"
            log' cxt "ab" LevelDebug "3"
            log' cxt "b" LevelDebug "4"
            res <- readIORef buf
            map (C8.concatMap removeNL) res `shouldBe` reverse [" [LevelDebug] 1", " [LevelDebug] 2", " [LevelDebug] 3"]

        it "Output error log with tag" $ do
            (cxt, buf) <- newIORef [] >>= getContexts LevelError ["a"]
            log' cxt "" LevelWarn "1"
            log' cxt "a" LevelWarn "2"
            log' cxt "a" LevelError "3"
            log' cxt "b" LevelError "4"
            res <- readIORef buf
            map (C8.concatMap removeNL) res `shouldBe` reverse [" [LevelError] 3"]