{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}
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
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Resource.Resource where

import GHC.Exts
import GHC.TypeLits
import System.IO
import Data.IORef
import Data.Proxy
import Data.Time
import qualified Data.List as L
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Control
import Control.Exception.Safe
import System.Log.FastLogger
import Control.Monad.Logger

{-| Loggers.

    Logger is a type of resource context which consists of a list of functions outputting log messages.
    When logging function defined in @Data.Resource.Logger@ is executed, the message will be logged by all of the functions.

    Logger is unique resource in point that logging function itself is always available without registering the resource explicitly.
    By default, logger has no functions thus any execution of logging function outputs nothing.
    Once logging resource is registered, it can be used in any generated context even if @Logger@ is not specified.
    In case multiple logging resources are registered, the former one has higher priority.
-}

-- ------------------------------------------------------------
-- Logger
-- ------------------------------------------------------------

-- | Resource type for the logger.
data LoggingResource = LoggingResource Logger

-- | Creates resource for the logger.
newLoggingResource :: [([String], LogLevel, LogType, Maybe TimeFormat)] -- ^ Logging configuration.
                   -> IO LoggingResource -- ^ Resource for the logger.
newLoggingResource ts = LoggingResource <$> mapM newLogger ts

-- | Logger type holding functions to output log messages.
type Logger = [String -> LogLevel -> (FormattedTime -> LogStr) -> IO ()]

-- | Generate a logging function from its configuration.
newLogger :: ([String], LogLevel, LogType, Maybe TimeFormat) -- ^ Logging configuration.
          -> IO (String -> LogLevel -> (FormattedTime -> LogStr) -> IO ()) -- ^ Logging function.
newLogger (tags, level, t, tf) = do
    getTime <- newTimeCache $ maybe "%Y/%m/%d %H:%M:%S" id tf
    (fl, cleanup) <- newTimedFastLogger getTime t
    return $ \tag l s -> if l < level && (tag == "" || any (matchTag tag) tags)
                            then return ()
                            else fl s `catchAny` \e -> hPutStrLn stderr (show e)
    where
        matchTag tag t = L.isPrefixOf t tag

-- | Predefined tag list which allows output of any kind of tag.
anyTag :: [String]
anyTag = [""]

-- | Predefined tag list which denies output of any kind of tag.
denyTag :: [String]
denyTag = []

-- | Declares a method to get the logger having the highest priority from resources.
class GetContextLogger (rs :: [*]) where
    getContextLogger :: (MonadIO m, MonadBaseControl IO m)
                     => Resources rs -- ^ Resources.
                     -> m Logger -- ^ The logger of the highest priority.

instance GetContextLogger '[] where
    getContextLogger _ = return []

instance {-# OVERLAPPING #-} GetContextLogger (IORef LoggingResource ': rs) where
    getContextLogger (r `RCons` rs) = do
        loggerRef <- newContext r
        liftIO $ readIORef loggerRef

instance {-# OVERLAPPABLE #-} (GetContextLogger rs) => GetContextLogger (r ': rs) where
    getContextLogger (r `RCons` rs) = getContextLogger rs

-- | Declares a method to get the logger having the highest priority from resource contexts.
class GetLogger cs where
    getLogger :: (MonadIO m)
              => Contexts cs -- ^ Resource contexts.
              -> m Logger -- ^ The logger of the highest priority.

instance GetLogger '[] where
    getLogger (CBase logger) = return logger

instance {-# OVERLAPPING #-} GetLogger (IORef Logger ': cs) where
    getLogger (c `CCons` _) = liftIO $ readIORef c

instance {-# OVERLAPPABLE #-} (GetLogger cs) => GetLogger (c ': cs) where
    getLogger (_ `CCons` cs) = getLogger cs

instance Resource LoggingResource where
    type ContextType LoggingResource = Logger

    --newContext :: (MonadIO m, MonadBaseControl IO m) => IORef LoggingResource -> m (IORef Logger)
    newContext r = do
        LoggingResource logger <- liftIO $ readIORef r
        liftIO $ newIORef logger

instance ResourceContext Logger where
    type ResourceType Logger = LoggingResource

    --closeContext :: (MonadIO m, MonadBaseControl IO m) => Logger -> Bool -> m Logger
    closeContext c b = return c

    --execContext :: (MonadIO m, MonadBaseControl IO m) => IORef Logger -> m a -> m a 
    execContext c f = f

-- ------------------------------------------------------------
-- Resources
-- ------------------------------------------------------------

-- | Represents a context used in IO action accessing resource.
-- Every context is generated by the resource and closed at the end of the action.
class (Resource (ResourceType c), ContextType (ResourceType c) ~ c) => ResourceContext c where
    -- | The type of resource which generates this context.
    type ResourceType c :: *

    -- | This method is invoked when the IO action finished or some exception is thrown.
    -- Instance type should implement releasing operation of resource handles if @execContext@ does not do it.
    closeContext :: (MonadIO m, MonadBaseControl IO m)
                 => c -- ^ This context.
                 -> Bool -- ^ Denotes whether the action finished without exception.
                 -> m c -- ^ Closed context.

    -- | Executes an IO action and returns the result.
    execContext :: (MonadIO m, MonadBaseControl IO m)
                => IORef c -- ^ Reference to this context.
                -> m a -- ^ An IO action.
                -> m a -- ^ The result of the action.

-- | Represents a resource to be managed in the application.
-- Every resource is used through a context generated in each execution of IO action.
class (ResourceContext (ContextType r), ResourceType (ContextType r) ~ r) => Resource r where
    -- | The type of context.
    type ContextType r :: *

    initialize :: (MonadIO m, MonadBaseControl IO m)
               => r
               -> m (IORef r)
    initialize = liftIO . newIORef

    -- | Generate a context. The context is contained in IORef, thus, it can be modified in IO action.
    newContext :: (MonadIO m, MonadBaseControl IO m)
               => IORef r -- ^ Referencee to this resource.
               -> m (IORef (ContextType r)) -- ^ Generate context contained in IORef.

-- | Hetero typed list of resource references.
data Resources (rs :: [*]) where
    -- | Data constructor of an empty list.
    RNil :: Resources '[]

    -- | Data constructor to prepend a resource reference to a list.
    RCons :: (Resource r)
          => IORef r -- ^ A resource reference to prepend.
          -> Resources rs -- ^ List of resource references.
          -> Resources (IORef r ': rs) -- ^ Prepended list.

infixr 5 `RCons`

type family ConsResources a b :: [*] where
    ConsResources (IORef r) (IORef q) = '[IORef r, IORef q]
    ConsResources (IORef r) (Resources rs) = IORef r ': rs
    ConsResources r (Resources rs) = IORef r ': rs
    ConsResources r (IORef q) = '[IORef r, IORef q]
    ConsResources (IORef r) q = '[IORef r, IORef q]
    ConsResources r q = '[IORef r, IORef q]

class ResourceCons r qs where
    (.+) :: r -> qs -> IO (Resources (ConsResources r qs))

instance {-# OVERLAPPING #-} (Resource r, Resource q) => ResourceCons (IORef r) (IORef q) where
    r .+ q = return $ r `RCons` q `RCons` RNil
instance {-# OVERLAPS #-} (Resource r, Resource q, ConsResources r (IORef q) ~ '[IORef r, IORef q]) => ResourceCons r (IORef q) where
    r .+ q = do
        rr <- initialize r :: IO (IORef r)
        return $ rr `RCons` q `RCons` RNil
instance {-# OVERLAPS #-} (Resource r, Resource q, ConsResources (IORef r) q ~ '[IORef r, IORef q]) => ResourceCons (IORef r) q where
    r .+ q = do
        qr <- initialize q
        return $ r `RCons` qr `RCons` RNil
instance {-# OVERLAPPABLE #-} (Resource r, Resource q, ConsResources r q ~ '[IORef r, IORef q]) => ResourceCons r q where
    r .+ q = do
        rr <- initialize r
        qr <- initialize q
        return $ rr `RCons` qr `RCons` RNil
instance {-# OVERLAPS #-} (Resource r) => ResourceCons (IORef r) (Resources rs) where
    r .+ rs = return $ r `RCons` rs
instance {-# OVERLAPPABLE #-} (Resource r, ConsResources r (Resources rs) ~ (IORef r ': rs)) => ResourceCons r (Resources rs) where
    r .+ rs = do
        rr <- initialize r
        return $ rr `RCons` rs

infixr 5 .+

-- | Declares a method to get a resource reference by its type.
-- Type should by given by type application or type signature.
-- > resourceOf @ResourceType resources
class ResourceOf r rs where
    resourceOf :: Resources rs -- ^ List of resource references.
               -> IORef r -- ^ A reference of the resource.

instance ResourceOf r (IORef r ': rs) where
    resourceOf (v `RCons` vs) = v

instance {-# OVERLAPPABLE #-} (ResourceOf r rs) => ResourceOf r (x ': rs) where
    resourceOf (v `RCons` vs) = resourceOf vs

-- | Returns context types from resource types.
type family ContextTypes (rs :: [*]) :: [*] where
    ContextTypes '[] = '[]
    ContextTypes (IORef r ': rs) = ContextType r ': ContextTypes rs

-- ------------------------------------------------------------
-- Contexts
-- ------------------------------------------------------------

-- | Hetero typed list of context references.
data Contexts (cs :: [*]) where
    -- | Data constructor of an empty list.
    CBase :: Logger
          -> Contexts '[]

    -- | Data constructor to prepend a context reference to a list.
    CCons :: (ResourceContext c)
          => IORef c -- ^ A context reference to prepend.
          -> Contexts cs -- ^ List of context references.
          -> Contexts (IORef c ': cs) -- ^ Prepended list.

infixr 5 `CCons`

-- Gets base context from contexts.
baseOf :: Contexts cs -- ^ Contexts.
       -> Contexts '[] -- ^ Base context.
baseOf b@(CBase _) = b
baseOf (_ `CCons` cs) = baseOf cs

-- | Execute a function in the environment where contexts can be obtained from an implicit param.
-- The function is called after every @execContext@ of each context is executed.
execContexts :: forall m cs' cs a. (MonadIO m, MonadBaseControl IO m)
             => Contexts (Refs cs') -- ^ List of all context references available in the function.
             -> Contexts cs -- ^ Partial list of contexts. This argument is used for recursion.
             -> (With cs' => m a) -- ^ A function.
             -> m a -- ^ Result of the function.
execContexts contexts (CBase _) f = let ?cxt = contexts in f
execContexts contexts (c `CCons` cxts) f = do
    execContext c $ execContexts contexts cxts f

-- | Close all contexts by executing their @closeContext@ in order.
-- Every modification done in @closeContext@ affects each context reference.
closeAll :: (MonadIO m, MonadBaseControl IO m)
         => Bool -- ^ Status denoting whether some error happened in operations under the contexts.
         -> Contexts cs -- ^ List of context references.
         -> m () -- ^ Returns nothing.
closeAll b (CBase _) = return ()
closeAll b (v `CCons` vs) = do
    c <- liftIO $ readIORef v
    closeContext c b >>= liftIO . writeIORef v
    closeAll b vs

-- | Declares a method to get a context reference by its type.
-- Type should by given by type application or type signature.
-- > contextOf @ContextType contexts
class ContextOf c cs where
    contextOf :: Contexts cs -> IORef c

instance ContextOf c (IORef c ': cs) where
    contextOf (v `CCons` vs) = v

instance {-# OVERLAPPABLE #-} (cs ~ (x ': cs'), ContextOf c cs') => ContextOf c cs where
    contextOf (v `CCons` vs) = contextOf vs

-- | Declares a method to generate contexts for resources.
class ContextResources (cs :: [*]) rs where
    -- | Generates contexts for resources.
    generateContexts :: (MonadIO m, MonadBaseControl IO m)
                     => Resources rs -- ^ Resources.
                     -> m (Contexts cs) -- ^ Contexts generated from all resources.

instance (GetContextLogger rs) => ContextResources '[] rs where
    generateContexts resources = CBase <$> getContextLogger resources

instance (ResourceContext c, ResourceOf (ResourceType c) rs, ContextResources cs rs) => ContextResources (IORef c ': cs) rs where
    generateContexts resources = do
        cxt <- newContext (resourceOf @(ResourceType c) resources)
        others <- generateContexts @cs resources
        return $ cxt `CCons` others

-- | Converts every item in type level list by applying @IORef@.
type family Refs (as :: [*]) = (rs :: [*]) | rs -> as
type instance Refs '[] = '[]
type instance Refs (a ': as) = IORef a ': Refs as

-- | Declares a method to select contexts of specified types from their superset.
class SelectContexts (ds :: [*]) (cs :: [*]) (cs' :: [*]) where
    -- | Select contexts of specified types.
    selectContexts :: Contexts cs' -- ^ Superset of contexts.
                   -> Contexts cs -- ^ Subset of contexts reduced on each recursion.
                   -> Contexts ds -- ^ Selected contexts.

instance {-# OVERLAPPING #-} SelectContexts '[] (c ': cs) cs' where
    selectContexts contexts _ = baseOf contexts

instance {-# OVERLAPPING #-} (SelectContexts ds cs' cs') => SelectContexts (IORef c ': ds) (IORef c ': cs) cs' where
    selectContexts contexts (v `CCons` vs) = v `CCons` selectContexts @ds contexts contexts

instance {-# OVERLAPPABLE #-} (SelectContexts ds cs cs') => SelectContexts ds (c ': cs) cs' where
    selectContexts contexts (v `CCons` vs) = selectContexts @ds contexts vs

-- | Constraint supplying an implicit parameter to a function.
-- In the function annotated this constraint, context reference can be obtained by @?cxt@ which is @Context cs@ variable.
--
-- > func :: (With '[C1, C2]) => String -> IO String
-- > func s = do
-- >     c1 <- readIORef $ contextOf @C1 ?cxt
-- >     c2 <- readIORef $ contextOf @C2 ?cxt
-- >     return $ anotherFunc c1 c2 s
type With (cs :: [*]) = (?cxt :: Contexts (Refs cs))

-- | Declares a method executing function with resource contexts.
class WithContext cs w where
    -- | Executes a function with contexts.
    withContext :: forall m a. (MonadIO m, MonadMask m, MonadBaseControl IO m)
                => w -- ^ Object resource contexts can be obtained from.
                -> (With cs => m a) -- ^ Action using contexts.
                -> m (a, Contexts (Refs cs)) -- ^ Result of the action and modified contexts.

-- | An instance of WithContext which generates contexts from resources.
-- @withContext@ of this instance will close the genrated contexts correctly.
instance {-# INCOHERENT #-} (ContextResources (Refs cs) rs) => WithContext cs (Resources rs) where
    withContext resources f = do
        bracketOnError (generateContexts @(Refs cs) resources)
                       (closeAll False)
                       (\c -> do
                            r <- execContexts c c f
                            closeAll True c
                            return (r, c)
                        )

instance {-# INCOHERENT #-} (SelectContexts (Refs ds) cs cs) => WithContext ds (Contexts cs) where
    withContext contexts f = let ?cxt = selectContexts @(Refs ds) contexts contexts in f >>= return . (, ?cxt)

-- | This function behaves as @withContext@ but returns just the result of ths action.
withContext' :: forall cs w m a. (WithContext cs w, MonadIO m, MonadMask m, MonadBaseControl IO m)
             => w -- ^ Object resource contexts can be obtained from.
             -> (With cs => m a) -- ^ Action using contexts.
             -> m a -- ^ Result of the action.
withContext' w f = fst <$> withContext w f

-- | Execute a function using contexts propagated from another function having @With@ constraint.
-- Use this to call a function in another function having implicit contexts of other types.
-- Among type variables, @ds@ is context types required by callee, @cs@ is context types of caller.
-- @ds@ must be the subset of @cs@ otherwise compilation is rejected.
--
-- > func1 :: (With '[C1, C2, C3]) => String -> IO String
-- > func1 s = do
-- >     s2 <- with @'[C2] func2
-- >     return $ s ++ s2
-- >
-- > func2 :: (With '[C2]) => IO String
-- >     c2 <- readIORef $ contextOf @C2 ?cxt
-- >     return $ show c2
with :: forall ds cs m a. (With cs, SelectContexts (Refs ds) (Refs cs) (Refs cs), MonadIO m, MonadMask m, MonadBaseControl IO m)
     => (With ds => m a) -- ^ A function using contexts of @ds@.
     -> m a -- ^ Result of the function.
with f = let ?cxt = selectContexts @(Refs ds) ?cxt ?cxt in f
