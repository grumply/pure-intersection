{-# LANGUAGE CPP, PatternSynonyms, MultiParamTypeClasses, RecordWildCards,
   TypeFamilies, DeriveGeneric, DeriveAnyClass, OverloadedStrings, 
   FlexibleContexts, ViewPatterns #-}
module Pure.Intersection where

import Pure hiding (features,children,Action)

import Pure.Data.Lifted
import Pure.Data.Cond
import Pure.Data.JSON
import Pure.Data.Prop as P

import Control.Arrow ((&&&))
import Control.Monad
import Data.Coerce
import Data.Foldable (for_,traverse_)
import Data.Function ((&))
import Data.IORef
import Data.Maybe
import GHC.Generics as G

#ifdef __GHCJS__
import GHCJS.Marshal.Internal
import JavaScript.Object.Internal as JS (Object(..),create,setProp)
#endif

pattern Observer :: Observer -> Observer
pattern Observer io = io

data Observer = Observer_
  { as         :: Features -> [View] -> View
  , features   :: Features
  , children   :: [View]
  , root       :: JSV
  , rootMargin :: Txt
  , threshold  :: [Double]
  , action     :: [Intersection] -> IO ()
  } deriving (Generic)

instance Default Observer where
  def = (G.to gdef) { as = \fs cs -> Div & Features fs & Children cs, root = nullJSV }

toOptions :: Observer -> IO JSV
toOptions o = do
#ifdef __GHCJS__
  obj <- JS.create
  ts <- toJSValListOf (filter (\x -> x >= 0 && x <= 1) (threshold o))
  JS.setProp "threshold" ts obj
  unless (isNull (root o)) $ JS.setProp "root" (root o) obj
  unless (rootMargin o == "") $ JS.setProp "rootMargin" (pToJSVal $ rootMargin o) obj
  pure (coerce obj)
#else
  pure ()
#endif

data Intersection = Intersection
  { bounds       :: Rect
  , ratio        :: Double
  , intersection :: Rect
  , intersecting :: Bool
  , rootBounds   :: Rect
  , target       :: JSV
  , time         :: Double
  }

instance Pure Observer where
  view =
    Component $ \self -> 
      let
        handleRef (Node ref) = modifyM_ self $ \o _ -> return ((Nothing,ref),run o)
        run o = do
          (_,ref) <- get self
          options <- toOptions o
          (obs,rel) <- observer (action o) options
          observe obs ref
          modify_ self $ \_ _ -> (Just rel,ref)
      in 
        def
          { construct = return (Nothing,nullJSV)
          , receive = \new (mrel,ref) -> traverse_ id mrel >> run new >> return (mrel,ref)
          , unmounted = get self >>= \(mrel,_) -> traverse_ id mrel
          , render  = \Observer_ {..} _ -> as (features & Lifecycle (HostRef handleRef)) children
          }

mkIntersection :: JSV -> Intersection
mkIntersection jsv = 
#ifdef __GHCJS__
  fromMaybe (error "Pure.Observer.mkObserverEventObject: fromMaybe got Nothing") $ do
    bcr <- fmap mkRect $ jsv .# "boundingClientRect"
    ip  <- jsv .# "intersectionRatio"
    ir  <- fmap mkRect $ jsv .# "intersectionRect"
    ii  <- jsv .# "isIntersecting"
    rb  <- fmap mkRect $ jsv .# "rootBounds"
    tr  <- jsv .# "target"
    tm  <- jsv .# "time"
    pure $ Intersection bcr ip ir ii rb tr tm
#else
  Intersection def 0 def Nothing def def 0
#endif


#ifdef __GHCJS__

foreign import javascript unsafe
  "$r = new IntersectionObserver($1,$2)" observer_js :: Callback (JSV -> IO ()) -> JSV -> IO JSV

foreign import javascript unsafe
  "$1.observe($2)" observe_js :: JSV -> JSV -> IO ()

#endif

observer :: ([Intersection] -> IO ()) -> JSV -> IO (JSV,IO ())
observer f options = do
#ifdef __GHCJS__
  cb <- syncCallback1 ContinueAsync $ \jsv -> do
    Just (map mkIntersection -> is) <- fromJSValListOf jsv
    f is
  obs <- observer_js cb (pFromJSVal options)
  pure (obs,releaseCallback cb)
#else
  pure ((),return ())
#endif

observe :: JSV -> JSV -> IO ()
observe obs e = do
#ifdef __GHCJS__
  observe_js obs e
#else
  pure ()
#endif

data Rect = Rect
  { rLeft :: Double
  , rTop :: Double
  , rRight :: Double
  , rBottom :: Double
  , rWidth :: Double
  , rHeight :: Double
  }

instance Default Rect where 
  def = Rect 0 0 0 0 0 0

mkRect :: JSV -> Rect
mkRect o =
#ifdef __GHCJS__
  fromMaybe (error "Pure.Observer.mkRect: fromMaybe got Nothing") $ do
    rLeft   <- o .# "left"
    rTop    <- o .# "top"
    rRight  <- o .# "right"
    rBottom <- o .# "bottom"
    rWidth  <- o .# "width"
    rHeight <- o .# "height"
    return Rect {..}
#else
  def
#endif

data As = As_
pattern As :: HasProp As a => Prop As a -> a -> a
pattern As p a <- (getProp As_ &&& id -> (p,a)) where
    As p a = P.setProp As_ p a

data Root = Root_
pattern Root :: HasProp Root a => Prop Root a -> a -> a
pattern Root p a <- (getProp Root_ &&& id -> (p,a)) where
  Root p a = P.setProp Root_ p a

data RootMargin = RootMargin_
pattern RootMargin :: HasProp RootMargin a => Prop RootMargin a -> a -> a
pattern RootMargin p a <- (getProp RootMargin_ &&& id -> (p,a)) where
  RootMargin p a = P.setProp RootMargin_ p a

data Threshold = Threshold_
pattern Threshold :: HasProp Threshold a => Prop Threshold a -> a -> a
pattern Threshold p a <- (getProp Threshold_ &&& id -> (p,a)) where
  Threshold p a = P.setProp Threshold_ p a

data Action = Action_
pattern Action :: HasProp Action a => Prop Action a -> a -> a
pattern Action p a <- (getProp Action_ &&& id -> (p,a)) where
  Action p a = P.setProp Action_ p a

instance HasProp As Observer where
    type Prop As Observer = Features -> [View] -> View
    getProp _ = as
    setProp _ a o = o { as = a }

instance HasProp Root Observer where
  type Prop Root Observer = JSV
  getProp _ = root
  setProp _ r o = o { root = r }

instance HasProp RootMargin Observer where
  type Prop RootMargin Observer = Txt
  getProp _ = rootMargin
  setProp _ rm o = o { rootMargin = rm }

instance HasProp Threshold Observer where
  type Prop Threshold Observer = [Double]
  getProp _ = threshold
  setProp _ ts o = o { threshold = ts }

instance HasProp Action Observer where
  type Prop Action Observer = [Intersection] -> IO ()
  getProp _ = action
  setProp _ a o = o { action = a }

instance HasFeatures Observer where
    getFeatures = features
    setFeatures as o = o { features = as }

instance HasChildren Observer where
    getChildren = children
    setChildren cs o = o { children = cs }

