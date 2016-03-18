{-|
Module      : Servant.Xhr.Path
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

module Servant.Xhr.Path where

import GHC.TypeLits
import Data.Proxy
import qualified Data.Text as T
import Servant.API
import Web.HttpApiData

-- | Value-level representation of the capture parts of a path.
--   The type parameter indicates the named, types parts of the path.
data XhrServantPath (path :: [(Symbol, *)]) where
    XhrServantPathNil :: XhrServantPath '[]
    XhrServantPathCons
        :: ( ToHttpApiData t
           , KnownSymbol name
           )
        => Proxy name
        -> t
        -> XhrServantPath rest
        -> XhrServantPath ( '(name, t) ': rest )

-- | Drop a capture part of a path
type family XhrServantPathDrop (name :: Symbol) (ty :: *) (path :: [(Symbol, *)]) :: [(Symbol, *)] where
    XhrServantPathDrop name ty ( '(name, ty) ': rest ) = rest
    XhrServantPathDrop name ty ( '(name', ty') ': rest ) = '( name', ty' ) ': XhrServantPathDrop name ty rest

-- | Shows that a given capture part is in a path, with proofs showing that its
--   value can be extracted from an XhrServantPath, and that an XhrServantPath
--   can be shrunk to exclude it.
class
    ( ToHttpApiData ty
    ) => InXhrServantPath (name :: Symbol) (ty :: *) (path :: [(Symbol, *)])
  where
    xhrServantPathGetValue :: Proxy name -> Proxy ty -> XhrServantPath path -> ty
    xhrServantPathDrop
        :: Proxy name
        -> Proxy ty
        -> XhrServantPath path
        -> XhrServantPath (XhrServantPathDrop name ty path)

instance {-# OVERLAPS #-}
    ( ToHttpApiData ty
    ) => InXhrServantPath name ty ( '(name, ty) ': rest )
  where
    xhrServantPathGetValue _ _ path = case path of
        XhrServantPathCons _ t _ -> t
    xhrServantPathDrop _ _ path = case path of
        XhrServantPathCons _ _ rest -> rest

instance {-# OVERLAPS #-}
    ( InXhrServantPath name ty rest
    ,   XhrServantPathDrop name ty ( '(name', ty') ': rest )
      ~ ( '(name', ty') ': XhrServantPathDrop name ty rest )
    ) => InXhrServantPath name ty ( '(name', ty') ': rest )
  where
    xhrServantPathGetValue proxyName proxyTy path = case path of
        XhrServantPathCons _ _ rest -> xhrServantPathGetValue proxyName proxyTy rest
    xhrServantPathDrop proxyName proxyTy path = case path of
        XhrServantPathCons proxyName' proxyTy' rest -> XhrServantPathCons proxyName' proxyTy' (xhrServantPathDrop proxyName proxyTy rest)

-- | Use XhrServantPath to make the path string of a servant route.
makeXhrServantPath
    :: forall servantRoute path .
       ( MakeXhrServantPath servantRoute path )
    => Proxy servantRoute
    -> XhrServantPath path
    -> T.Text
makeXhrServantPath proxyRoute = T.intercalate "/" . makeXhrServantPathParts proxyRoute

-- | Shows that a given path provides all the details for the path of a servant
--   route.
class MakeXhrServantPath servantRoute path where
    makeXhrServantPathParts :: Proxy servantRoute -> XhrServantPath path -> [T.Text]

instance {-# OVERLAPS #-}
    (
    ) => MakeXhrServantPath servantRoute '[]
  where
    makeXhrServantPathParts _ _ = []

instance {-# OVERLAPS #-}
    ( InXhrServantPath name t (p ': ps)
    , MakeXhrServantPath servantRoute (XhrServantPathDrop name t (p ': ps))
    ) => MakeXhrServantPath ( Capture name t :> servantRoute ) (p ': ps)
  where
    makeXhrServantPathParts _ path =
          toUrlPiece (xhrServantPathGetValue (Proxy :: Proxy name) (Proxy :: Proxy t) path)
        : makeXhrServantPathParts (Proxy :: Proxy servantRoute) (xhrServantPathDrop (Proxy :: Proxy name) (Proxy :: Proxy t) path)



instance {-# OVERLAPS #-}
    ( KnownSymbol name
    , MakeXhrServantPath servantRoute '[]
    ) => MakeXhrServantPath ( name :> servantRoute ) '[]
  where
    makeXhrServantPathParts _ path
        = T.pack (symbolVal (Proxy :: Proxy name))
        : makeXhrServantPathParts (Proxy :: Proxy servantRoute) path

instance {-# OVERLAPS #-}
    ( KnownSymbol name
    , MakeXhrServantPath servantRoute (p ': ps)
    ) => MakeXhrServantPath ( name :> servantRoute ) (p ': ps)
  where
    makeXhrServantPathParts _ path
        = T.pack (symbolVal (Proxy :: Proxy name))
        : makeXhrServantPathParts (Proxy :: Proxy servantRoute) path



instance {-# OVERLAPS #-}
    ( MakeXhrServantPath servantRoute '[]
    ) => MakeXhrServantPath ( anything :> servantRoute ) '[]
  where
    makeXhrServantPathParts _ path = makeXhrServantPathParts (Proxy :: Proxy servantRoute) path

instance {-# OVERLAPS #-}
    ( MakeXhrServantPath servantRoute (p ': ps)
    ) => MakeXhrServantPath ( anything :> servantRoute ) (p ': ps)
  where
    makeXhrServantPathParts _ path = makeXhrServantPathParts (Proxy :: Proxy servantRoute) path

-- type Example = Capture "foo" Bool :> "user" :> Capture "bar" Int :> Get '[] ()
--
-- example :: Proxy Example
-- example = Proxy
--
-- exampleD = XhrServantPathCons (Proxy :: Proxy "bar") (42 :: Int)
--          . XhrServantPathCons (Proxy :: Proxy "foo") False
--          $ XhrServantPathNil
--
-- makeXhrRequestPath example exampleD = "false/user/42"
