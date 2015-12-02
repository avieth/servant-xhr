{-|
Module      : Servant.XHR.Path
Description : Definitions for constructing paths from servant types.
Copyright   : (c) Alexander Vieth, 2015
Licence     : BSD3
Maintainer  : aovieth@gmail.com
Stability   : experimental
Portability : non-portable (GHC only)
-}

{-# LANGUAGE AutoDeriveTypeable #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Servant.XHR.Path where

import GHC.TypeLits
import Data.Proxy
import qualified Data.Text as T
import Servant.API
import Web.HttpApiData

-- | Value-level representation of the capture parts of a path.
--   The type parameter indicates the named, types parts of the path.
data XHRServantPath (path :: [(Symbol, *)]) where
    XHRServantPathNil :: XHRServantPath '[]
    XHRServantPathCons
        :: ( ToHttpApiData t
           , KnownSymbol name
           )
        => Proxy name
        -> t
        -> XHRServantPath rest
        -> XHRServantPath ( '(name, t) ': rest )

-- | Drop a capture part of a path
type family XHRServantPathDrop (name :: Symbol) (ty :: *) (path :: [(Symbol, *)]) :: [(Symbol, *)] where
    XHRServantPathDrop name ty ( '(name, ty) ': rest ) = rest
    XHRServantPathDrop name ty ( '(name', ty') ': rest ) = '( name', ty' ) ': XHRServantPathDrop name ty rest

-- | Shows that a given capture part is in a path, with proofs showing that its
--   value can be extracted from an XHRServantPath, and that an XHRServantPath
--   can be shrunk to exclude it.
class
    ( ToHttpApiData ty
    ) => InXHRServantPath (name :: Symbol) (ty :: *) (path :: [(Symbol, *)])
  where
    xhrServantPathGetValue :: Proxy name -> Proxy ty -> XHRServantPath path -> ty
    xhrServantPathDrop
        :: Proxy name
        -> Proxy ty
        -> XHRServantPath path
        -> XHRServantPath (XHRServantPathDrop name ty path)

instance {-# OVERLAPS #-}
    ( ToHttpApiData ty
    ) => InXHRServantPath name ty ( '(name, ty) ': rest )
  where
    xhrServantPathGetValue _ _ path = case path of
        XHRServantPathCons _ t _ -> t
    xhrServantPathDrop _ _ path = case path of
        XHRServantPathCons _ _ rest -> rest

instance {-# OVERLAPS #-}
    ( InXHRServantPath name ty rest
    ,   XHRServantPathDrop name ty ( '(name', ty') ': rest )
      ~ ( '(name', ty') ': XHRServantPathDrop name ty rest )
    ) => InXHRServantPath name ty ( '(name', ty') ': rest )
  where
    xhrServantPathGetValue proxyName proxyTy path = case path of
        XHRServantPathCons _ _ rest -> xhrServantPathGetValue proxyName proxyTy rest
    xhrServantPathDrop proxyName proxyTy path = case path of
        XHRServantPathCons proxyName' proxyTy' rest -> XHRServantPathCons proxyName' proxyTy' (xhrServantPathDrop proxyName proxyTy rest)

-- | Use XHRServantPath to make the path string of a servant route.
makeXHRServantPath
    :: forall servantRoute path .
       ( MakeXHRServantPath servantRoute path )
    => Proxy servantRoute
    -> XHRServantPath path
    -> T.Text
makeXHRServantPath proxyRoute = T.intercalate "/" . makeXHRServantPathParts proxyRoute

-- | Shows that a given path provides all the details for the path of a servant
--   route.
class MakeXHRServantPath servantRoute path where
    makeXHRServantPathParts :: Proxy servantRoute -> XHRServantPath path -> [T.Text]

instance {-# OVERLAPS #-}
    (
    ) => MakeXHRServantPath servantRoute '[]
  where
    makeXHRServantPathParts _ _ = []

instance {-# OVERLAPS #-}
    ( InXHRServantPath name t (p ': ps)
    , MakeXHRServantPath servantRoute (XHRServantPathDrop name t (p ': ps))
    ) => MakeXHRServantPath ( Capture name t :> servantRoute ) (p ': ps)
  where
    makeXHRServantPathParts _ path =
          toUrlPiece (xhrServantPathGetValue (Proxy :: Proxy name) (Proxy :: Proxy t) path)
        : makeXHRServantPathParts (Proxy :: Proxy servantRoute) (xhrServantPathDrop (Proxy :: Proxy name) (Proxy :: Proxy t) path)



instance {-# OVERLAPS #-}
    ( KnownSymbol name
    , MakeXHRServantPath servantRoute '[]
    ) => MakeXHRServantPath ( name :> servantRoute ) '[]
  where
    makeXHRServantPathParts _ path
        = T.pack (symbolVal (Proxy :: Proxy name))
        : makeXHRServantPathParts (Proxy :: Proxy servantRoute) path

instance {-# OVERLAPS #-}
    ( KnownSymbol name
    , MakeXHRServantPath servantRoute (p ': ps)
    ) => MakeXHRServantPath ( name :> servantRoute ) (p ': ps)
  where
    makeXHRServantPathParts _ path
        = T.pack (symbolVal (Proxy :: Proxy name))
        : makeXHRServantPathParts (Proxy :: Proxy servantRoute) path



instance {-# OVERLAPS #-}
    ( MakeXHRServantPath servantRoute '[]
    ) => MakeXHRServantPath ( anything :> servantRoute ) '[]
  where
    makeXHRServantPathParts _ path = makeXHRServantPathParts (Proxy :: Proxy servantRoute) path

instance {-# OVERLAPS #-}
    ( MakeXHRServantPath servantRoute (p ': ps)
    ) => MakeXHRServantPath ( anything :> servantRoute ) (p ': ps)
  where
    makeXHRServantPathParts _ path = makeXHRServantPathParts (Proxy :: Proxy servantRoute) path

-- type Example = Capture "foo" Bool :> "user" :> Capture "bar" Int :> Get '[] ()
--
-- example :: Proxy Example
-- example = Proxy
--
-- exampleD = XHRServantPathCons (Proxy :: Proxy "bar") (42 :: Int)
--          . XHRServantPathCons (Proxy :: Proxy "foo") False
--          $ XHRServantPathNil
--
-- makeXHRRequestPath example exampleD = "false/user/42"
