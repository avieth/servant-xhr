{-|
Module      : Servant.XHR.Headers
Description : Definitions for constructing headers from servant types.
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

module Servant.XHR.Headers where

import GHC.TypeLits
import Data.Proxy
import qualified Data.Text as T
import Data.Text.Encoding
import Servant.API
import Servant.XHR.Body
import Web.HttpApiData

-- | Value-level representation of the capture parts of a path.
--   The type parameter indicates the named, types parts of the path.
data XHRServantHeaders (headers :: [(Symbol, *)]) where
    XHRServantHeadersNil :: XHRServantHeaders '[]
    XHRServantHeadersCons
        :: ( ToHttpApiData t
           , KnownSymbol name
           )
        => Proxy name
        -> t
        -> XHRServantHeaders rest
        -> XHRServantHeaders ( '(name, t) ': rest )

-- | Drop a capture part of a path
type family XHRServantHeadersDrop (name :: Symbol) (ty :: *) (path :: [(Symbol, *)]) :: [(Symbol, *)] where
    XHRServantHeadersDrop name ty ( '(name, ty) ': rest ) = rest
    XHRServantHeadersDrop name ty ( '(name', ty') ': rest ) = '( name', ty' ) ': XHRServantHeadersDrop name ty rest

-- | Shows that a given capture part is in a path, with proofs showing that its
--   value can be extracted from an XHRServantPath, and that an XHRServantPath
--   can be shrunk to exclude it.
class
    ( ToHttpApiData ty
    ) => InXHRServantHeaders (name :: Symbol) (ty :: *) (headers :: [(Symbol, *)])
  where
    xhrServantHeadersGetValue :: Proxy name -> Proxy ty -> XHRServantHeaders headers -> ty
    xhrServantHeadersDrop
        :: Proxy name
        -> Proxy ty
        -> XHRServantHeaders headers
        -> XHRServantHeaders (XHRServantHeadersDrop name ty headers)

instance {-# OVERLAPS #-}
    ( ToHttpApiData ty
    ) => InXHRServantHeaders name ty ( '(name, ty) ': rest )
  where
    xhrServantHeadersGetValue _ _ path = case path of
        XHRServantHeadersCons _ t _ -> t
    xhrServantHeadersDrop _ _ path = case path of
        XHRServantHeadersCons _ _ rest -> rest

instance {-# OVERLAPS #-}
    ( InXHRServantHeaders name ty rest
    ,   XHRServantHeadersDrop name ty ( '(name', ty') ': rest )
      ~ ( '(name', ty') ': XHRServantHeadersDrop name ty rest )
    ) => InXHRServantHeaders name ty ( '(name', ty') ': rest )
  where
    xhrServantHeadersGetValue proxyName proxyTy path = case path of
        XHRServantHeadersCons _ _ rest -> xhrServantHeadersGetValue proxyName proxyTy rest
    xhrServantHeadersDrop proxyName proxyTy path = case path of
        XHRServantHeadersCons proxyName' proxyTy' rest -> XHRServantHeadersCons proxyName' proxyTy' (xhrServantHeadersDrop proxyName proxyTy rest)

-- | Shows that a given path provides all the details for the path of a servant
--   route.
class MakeXHRServantHeaders servantRoute headers where
    makeXHRServantHeaders
        :: Proxy servantRoute
        -> XHRServantHeaders headers
        -> [(T.Text, T.Text)]

-- | This one must only match in case servantRoute is the end of a route (not
--   a :> type). In that case, we demand that there be no headers remaining.
instance {-# OVERLAPS #-}
    ( headers ~ '[]
    ) => MakeXHRServantHeaders servantRoute headers
  where
    makeXHRServantHeaders _ _ = []

instance {-# OVERLAPS #-}
    ( InXHRServantHeaders name t headers
    , MakeXHRServantHeaders servantRoute (XHRServantHeadersDrop name t headers)
    , KnownSymbol name
    ) => MakeXHRServantHeaders ( Header name t :> servantRoute ) headers
  where
    makeXHRServantHeaders _ headers = ( decodeUtf8 (toHeader (symbolVal (Proxy :: Proxy name)))
                                      , decodeUtf8 (toHeader (xhrServantHeadersGetValue (Proxy :: Proxy name) (Proxy :: Proxy t) headers))
                                      )
                                    : makeXHRServantHeaders (Proxy :: Proxy servantRoute) (xhrServantHeadersDrop (Proxy :: Proxy name) (Proxy :: Proxy t) headers)

instance {-# OVERLAPS #-}
    ( MakeXHRServantHeaders servantRoute headers
    ) => MakeXHRServantHeaders ( anything :> servantRoute ) headers
  where
    makeXHRServantHeaders _ headers = makeXHRServantHeaders (Proxy :: Proxy servantRoute) headers

class AddXHRContentTypeHeader contentType body where
    addXHRContentTypeHeader
        :: XHRServantBody contentType body
        -> [(T.Text, T.Text)]
        -> [(T.Text, T.Text)]

instance AddXHRContentTypeHeader contentType EmptyBody where
    addXHRContentTypeHeader _ = id

instance
    ( Accept contentType
    ) => AddXHRContentTypeHeader contentType (NonEmptyBody body)
  where
    addXHRContentTypeHeader (XHRServantBodyNonEmpty ctype _) =
        (:) ("Content-Type", T.pack (show (contentType ctype)))

-- type Example = Header "auth" T.Text :> "user" :> Header "count" Int :> Capture "bar" Int :> Get '[JSON] ()
--
-- example :: Proxy Example
-- example = Proxy
--
-- exampleD = XHRServantHeadersCons (Proxy :: Proxy "count") (42 :: Int)
--          . XHRServantHeadersCons (Proxy :: Proxy "auth") ("1234" :: T.Text)
--          $ XHRServantHeadersNil
--
-- makeXHRRequestPath example exampleD = [("auth", "1234"), ("count", 42"), ("accept", "application/json")]
