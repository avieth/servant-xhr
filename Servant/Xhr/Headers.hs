{-|
Module      : Servant.Xhr.Headers
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

module Servant.Xhr.Headers where

import GHC.TypeLits
import Data.Proxy
import qualified Data.Text as T
import Data.Text.Encoding
import Servant.API
import Servant.Xhr.Body
import Web.HttpApiData

-- | Value-level representation of the capture parts of a path.
--   The type parameter indicates the named, types parts of the path.
data XhrServantHeaders (headers :: [(Symbol, *)]) where
    XhrServantHeadersNil :: XhrServantHeaders '[]
    XhrServantHeadersCons
        :: ( ToHttpApiData t
           , KnownSymbol name
           )
        => Proxy name
        -> t
        -> XhrServantHeaders rest
        -> XhrServantHeaders ( '(name, t) ': rest )

-- | Drop a capture part of a path
type family XhrServantHeadersDrop (name :: Symbol) (ty :: *) (path :: [(Symbol, *)]) :: [(Symbol, *)] where
    XhrServantHeadersDrop name ty ( '(name, ty) ': rest ) = rest
    XhrServantHeadersDrop name ty ( '(name', ty') ': rest ) = '( name', ty' ) ': XhrServantHeadersDrop name ty rest

-- | Shows that a given capture part is in a path, with proofs showing that its
--   value can be extracted from an XhrServantPath, and that an XhrServantPath
--   can be shrunk to exclude it.
class
    ( ToHttpApiData ty
    ) => InXhrServantHeaders (name :: Symbol) (ty :: *) (headers :: [(Symbol, *)])
  where
    xhrServantHeadersGetValue :: Proxy name -> Proxy ty -> XhrServantHeaders headers -> ty
    xhrServantHeadersDrop
        :: Proxy name
        -> Proxy ty
        -> XhrServantHeaders headers
        -> XhrServantHeaders (XhrServantHeadersDrop name ty headers)

instance {-# OVERLAPS #-}
    ( ToHttpApiData ty
    ) => InXhrServantHeaders name ty ( '(name, ty) ': rest )
  where
    xhrServantHeadersGetValue _ _ path = case path of
        XhrServantHeadersCons _ t _ -> t
    xhrServantHeadersDrop _ _ path = case path of
        XhrServantHeadersCons _ _ rest -> rest

instance {-# OVERLAPS #-}
    ( InXhrServantHeaders name ty rest
    ,   XhrServantHeadersDrop name ty ( '(name', ty') ': rest )
      ~ ( '(name', ty') ': XhrServantHeadersDrop name ty rest )
    ) => InXhrServantHeaders name ty ( '(name', ty') ': rest )
  where
    xhrServantHeadersGetValue proxyName proxyTy path = case path of
        XhrServantHeadersCons _ _ rest -> xhrServantHeadersGetValue proxyName proxyTy rest
    xhrServantHeadersDrop proxyName proxyTy path = case path of
        XhrServantHeadersCons proxyName' proxyTy' rest -> XhrServantHeadersCons proxyName' proxyTy' (xhrServantHeadersDrop proxyName proxyTy rest)

-- | Shows that a given path provides all the details for the path of a servant
--   route.
class MakeXhrServantHeaders servantRoute headers where
    makeXhrServantHeaders
        :: Proxy servantRoute
        -> XhrServantHeaders headers
        -> [(T.Text, T.Text)]

-- | This one must only match in case servantRoute is the end of a route (not
--   a :> type). In that case, we demand that there be no headers remaining.
instance {-# OVERLAPS #-}
    ( headers ~ '[]
    ) => MakeXhrServantHeaders servantRoute headers
  where
    makeXhrServantHeaders _ _ = []

instance {-# OVERLAPS #-}
    ( InXhrServantHeaders name t headers
    , MakeXhrServantHeaders servantRoute (XhrServantHeadersDrop name t headers)
    , KnownSymbol name
    ) => MakeXhrServantHeaders ( Header name t :> servantRoute ) headers
  where
    makeXhrServantHeaders _ headers = ( decodeUtf8 (toHeader (symbolVal (Proxy :: Proxy name)))
                                      , decodeUtf8 (toHeader (xhrServantHeadersGetValue (Proxy :: Proxy name) (Proxy :: Proxy t) headers))
                                      )
                                    : makeXhrServantHeaders (Proxy :: Proxy servantRoute) (xhrServantHeadersDrop (Proxy :: Proxy name) (Proxy :: Proxy t) headers)

instance {-# OVERLAPS #-}
    ( MakeXhrServantHeaders servantRoute headers
    ) => MakeXhrServantHeaders ( anything :> servantRoute ) headers
  where
    makeXhrServantHeaders _ headers = makeXhrServantHeaders (Proxy :: Proxy servantRoute) headers

class AddXhrContentTypeHeader contentType body where
    addXhrContentTypeHeader
        :: XhrServantBody contentType body
        -> [(T.Text, T.Text)]
        -> [(T.Text, T.Text)]

instance AddXhrContentTypeHeader contentType EmptyBody where
    addXhrContentTypeHeader _ = id

instance
    ( Accept contentType
    ) => AddXhrContentTypeHeader contentType (NonEmptyBody body)
  where
    addXhrContentTypeHeader (XhrServantBodyNonEmpty ctype _) =
        (:) ("Content-Type", T.pack (show (contentType ctype)))

-- type Example = Header "auth" T.Text :> "user" :> Header "count" Int :> Capture "bar" Int :> Get '[JSON] ()
--
-- example :: Proxy Example
-- example = Proxy
--
-- exampleD = XhrServantHeadersCons (Proxy :: Proxy "count") (42 :: Int)
--          . XhrServantHeadersCons (Proxy :: Proxy "auth") ("1234" :: T.Text)
--          $ XhrServantHeadersNil
--
-- makeXhrRequestPath example exampleD = [("auth", "1234"), ("count", 42"), ("accept", "application/json")]
