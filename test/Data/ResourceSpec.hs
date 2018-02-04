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

module Data.ResourceSpec where

import Test.Hspec
import Control.Monad.IO.Class
import Data.IORef
import Data.Resource
import Control.Exception.Safe

data A = A Int deriving (Eq, Show)
data B = B Int deriving (Eq, Show)
data C = C Int deriving (Eq, Show)

data CA = CA Int Bool deriving (Eq, Show)
data CB = CB Int Bool deriving (Eq, Show)
data CC = CC Int Bool deriving (Eq, Show)

instance Resource A where
    type ContextType A = CA
    newContext (A v) = liftIO $ newIORef (CA v False)
instance Resource B where
    type ContextType B = CB
    newContext (B v) = liftIO $ newIORef (CB v False)
instance Resource C where
    type ContextType C = CC
    newContext (C v) = liftIO $ newIORef (CC v False)

instance ResourceContext CA where
    type ResourceType CA = A
    closeContext (CA v _) b = return $ CA v b
    execContext c f = f
instance ResourceContext CB where
    type ResourceType CB = B
    closeContext (CB v _) b = return $ CB v b
    execContext c f = f
instance ResourceContext CC where
    type ResourceType CC = C
    closeContext (CC v _) b = return $ CC v b
    execContext c f = f

spec :: Spec
spec = do
    describe "Resources" $ do
        it "Generate and access" $ do
            ra <- newIORef (A 1)
            rb <- newIORef (B 2)
            rc <- newIORef (C 3)
            let resources = ra `RCons` rb `RCons` rc `RCons` RNil
            readIORef (resourceOf @B resources) >>= (`shouldBe` (B 2))

    describe "Contexts" $ do
        it "Generate and access" $ do
            ca <- newIORef $ CA 1 False
            cb <- newIORef $ CB 2 False
            cc <- newIORef $ CC 3 False
            let contexts = ca `CCons` cb `CCons` cc `CCons` CNil
            readIORef (contextOf @CB contexts) >>= (`shouldBe` CB 2 False)

        it "Close all contexts" $ do
            ca <- newIORef $ CA 1 False
            cb <- newIORef $ CB 2 False
            cc <- newIORef $ CC 3 False
            let contexts = ca `CCons` cb `CCons` cc `CCons` CNil
            closeAll True contexts
            readIORef (contextOf @CA contexts) >>= (`shouldBe` CA 1 True)
            readIORef (contextOf @CB contexts) >>= (`shouldBe` CB 2 True)
            readIORef (contextOf @CC contexts) >>= (`shouldBe` CC 3 True)

        it "Generate contexts of all resources" $ do
            ra <- newIORef (A 1)
            rb <- newIORef (B 2)
            rc <- newIORef (C 3)
            let resources = ra `RCons` rb `RCons` rc `RCons` RNil
            contexts <- generateContexts @(Refs '[CA, CB, CC]) resources
            case contexts of
                ca `CCons` cb `CCons` cc `CCons` CNil
                    -> do
                        readIORef ca >>= (`shouldBe` CA 1 False)
                        readIORef cb >>= (`shouldBe` CB 2 False)
                        readIORef cc >>= (`shouldBe` CC 3 False)

        it "Generate contexts of selected types" $ do
            ra <- newIORef (A 1)
            rb <- newIORef (B 2)
            rc <- newIORef (C 3)
            let resources = ra `RCons` rb `RCons` rc `RCons` RNil
            contexts <- generateContexts @(Refs '[CB, CC]) resources
            case contexts of
                cb `CCons` cc `CCons` CNil
                    -> do
                        readIORef cb >>= (`shouldBe` CB 2 False)
                        readIORef cc >>= (`shouldBe` CC 3 False)

    describe "Contextual function invocation" $ do
        it "Use context" $ do
            ra <- newIORef (A 1)
            rb <- newIORef (B 2)
            rc <- newIORef (C 3)
            let resources = ra `RCons` rb `RCons` rc `RCons` RNil
            (v, _) <- withContext @'[CA, CC] resources contextualFunc
            v `shouldBe` 4

        it "Modify context" $ do
            ra <- newIORef (A 1)
            rb <- newIORef (B 2)
            rc <- newIORef (C 3)
            let resources = ra `RCons` rb `RCons` rc `RCons` RNil
            (v, contexts) <- withContext @'[CA, CC] resources contextualFunc2
            v `shouldBe` 4
            case contexts of
                ca `CCons` cc `CCons` CNil
                    -> do
                        readIORef ca >>= (`shouldBe` CA 2 True)
                        readIORef cc >>= (`shouldBe` CC 6 True)

        it "Raise error" $ do
            ra <- newIORef (A 1)
            rb <- newIORef (B 2)
            rc <- newIORef (C 3)
            let resources = ra `RCons` rb `RCons` rc `RCons` RNil
            withContext @'[CA, CC] resources (throwIO TestException) `shouldThrow` anyException

data TestException = TestException deriving (Show)

instance Exception TestException

contextualFunc :: (With '[CA, CC])
               => IO Int
contextualFunc = do
    ca <- readIORef $ contextOf @CA ?cxt
    cc <- readIORef $ contextOf @CC ?cxt
    let fa = case ca of CA v _ -> v
    let fc = case cc of CC v _ -> v
    return $ fa + fc

contextualFunc2 :: (With '[CA, CC])
                => IO Int
contextualFunc2 = do
    let ra = contextOf @CA ?cxt
    let rc = contextOf @CC ?cxt
    (CA va ba) <- readIORef ra
    (CC vc bc) <- readIORef rc
    writeIORef ra (CA (va * 2) ba)
    writeIORef rc (CC (vc * 2) bc)
    return $ va + vc