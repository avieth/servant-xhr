{-|
Module      : 
Description : 
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

module Servant.XHR.RequestData where


import GHC.TypeLits
import Data.Proxy
import qualified Data.Text as T
import Data.Monoid (mconcat)
--import Servant
--import Reactive.DOM.XHR
import Servant.API

-- The idea:
--
--   type ServantXHRRequest
--
--   header
--       :: Header name t
--       -> ServantXHRRequest method headers url body
--       -> ServantXHRRequest method ( '(name, t) ': headers ) url body
--
--   queryParam
--       :: QueryParam name t
--       -> ServantXHRRequest method headers url body
--       -> ServantXHRRequest method headers ( QueryParamet name t ': url ) body
--
--   body
--       :: body
--       -> ServantXHRRequest method headers url NoBody
--       -> ServantXHRRequest method headers url body
--
--   method
--       :: Method method
--       -> ServantXHRRequest NoMethod headers url body
--       -> ServantXHRRequest method headers url body
--
-- but such that we can't overwrite a previous one.
-- Request are then constructed using normal function composition.
--
--
--   myRequest = header "auth" token
--             . header "foo" bar
--             . queryParam "user" me
--             . queryParam "location" here
--             . url someUrl
--             . body theData
--             . method GET
--             . xhrRequest 
--
-- And that's not even servant-related.
-- Servant comes in to infer the URL and method, some headers
-- (content-types), and then then ensure that exactly the required capture,
-- query, body, headers are provided.

type family ServantHeaders route :: [*] where
    ServantHeaders (Header sym a :> sublayout) = Header sym a ': ServantHeaders sublayout
    ServantHeaders (Header sym a) = '[Header sym a]
    ServantHeaders (something :> sublayout) = ServantHeaders sublayout
    ServantHeaders x = '[]

type family ServantHeaderName header :: Symbol where
    ServantHeaderName (Header name t) = name

type family ServantHeaderType header :: * where
    ServantHeaderType (Header name t) = t

type family ServantHeaderUniqueName (name :: Symbol) (headers :: [*]) :: Bool where
    ServantHeaderUniqueName name '[] = 'True
    ServantHeaderUniqueName name (Header name t ': rest) = 'False
    ServantHeaderUniqueName name (Header name' t ': rest) = ServantHeaderUniqueName name rest

class HasServantHeader (header :: *) (headerData :: *) where
    getServantHeader :: Proxy header -> headerData -> ServantHeaderType header

instance {-# OVERLAPS #-}
    (
    ) => HasServantHeader (Header name t) (XHRRequestDataHeaders (Header name t ': rest))
  where
    getServantHeader proxyHeader headerData = case headerData of
        XHRRequestDataHeadersCons _ t _ -> t

instance {-# OVERLAPS #-}
    ( HasServantHeader (Header name t) (XHRRequestDataHeaders rest)
    ) => HasServantHeader (Header name t) (XHRRequestDataHeaders (Header name' t' ': rest))
  where
    getServantHeader proxyHeader headerData = case headerData of
        XHRRequestDataHeadersCons _ _ rest -> getServantHeader proxyHeader rest

data XHRRequestDataHeaders (headers :: [*]) where
    XHRRequestDataHeadersNil :: XHRRequestDataHeaders '[]
    XHRRequestDataHeadersCons
        :: ( ServantHeaderUniqueName name headers ~ 'True
           , ToText t
           , KnownSymbol name
           )
        => Proxy name
        -> t
        -> XHRRequestDataHeaders headers
        -> XHRRequestDataHeaders (Header name t ': headers)

data XHRServantPath (path :: [(Symbol, *)]) where
    XHRServantPathNil :: XHRServantPath '[]
    XHRServantPathCons
        :: ( ToText t
           , KnownSymbol name
           )
        => Proxy name
        -> t
        -> XHRServantPath rest
        -> XHRServantPath ( '(name, t) ': rest )

type family XHRServantPathDrop (name :: Symbol) (ty :: *) (path :: [(Symbol, *)]) :: [(Symbol, *)] where
    XHRServantPathDrop name ty ( '(name, ty) ': rest ) = rest
    XHRServantPathDrop name ty ( '(name', ty') ': rest ) = '(name, ty) ': XHRServantPathDrop name ty rest

class
    ( ToText ty
    ) => InXHRServantPath (name :: Symbol) (ty :: *) (path :: [(Symbol, *)])
  where
    xhrServantPathGetValue :: Proxy name -> Proxy ty -> XHRServantPath path -> ty
    xhrServantPathDrop
        :: Proxy name
        -> Proxy ty
        -> XHRServantPath path
        -> XHRServantPath (XHRServantPathDrop name ty path)

instance {-# OVERLAPS #-}
    ( ToText ty
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

class MakeXHRServantPath servantRoute path where
    makeXHRServantPath :: Proxy servantRoute -> XHRServantPath path -> [T.Text]

instance {-# OVERLAPS #-}
    (
    ) => MakeXHRServantPath servantRoute '[]
  where
    makeXHRServantPath _ _ = []

instance {-# OVERLAPS #-}
    ( InXHRServantPath name t path
    , MakeXHRServantPath servantRoute (XHRServantPathDrop name t path)
    ) => MakeXHRServantPath ( Capture name t :> servantRoute ) path
  where
    makeXHRServantPath _ path = toText (xhrServantPathGetValue (Proxy :: Proxy name) (Proxy :: Proxy t) path)
                              : makeXHRServantPath (Proxy :: Proxy servantRoute) (xhrServantPathDrop (Proxy :: Proxy name) (Proxy :: Proxy t) path)

instance {-# OVERLAPS #-}
    ( KnownSymbol name
    , MakeXHRServantPath servantRoute path
    ) => MakeXHRServantPath ( name :> servantRoute ) path
  where
    makeXHRServantPath _ path = T.pack (symbolVal (Proxy :: Proxy name))
                              : makeXHRServantPath (Proxy :: Proxy servantRoute) path

instance {-# OVERLAPS #-}
    ( MakeXHRServantPath servantRoute path
    ) => MakeXHRServantPath ( anything :> servantRoute ) path
  where
    makeXHRServantPath _ path = makeXHRServantPath (Proxy :: Proxy servantRoute) path

-- | Method should be something like
--      Get (contentTypes :: [*]) (t :: *)
--   Body, something like
--      ReqBody (contentTypes :: [*]) (t :: *)
--
--   What about the information used to compute the url?
--   We could either give that all at once, or in order... Nah, should give
--   it at once.
--
--   Can we avoid coupling with Servant? i.e. give some general datatype for
--   constructing XHR request data in a type-rich way, and then just have
--   the servant route types indicate precisely what those type parameters
--   ought to be?
--   Yeah, why not?
--
--       method (Get '[JSON])
--     . part "profile"
--     . part "user"
--     . port 80
--     . host "localhost"
--     . xhrRequestData
--
--   becomes a GET to localhost:80/user/profile
--   Servant types can automatically fill in the string parts...
--   Hm, well it can also fill in the method, and body content type.
--   Right, for a Servant-directed request you just need to give:
--
--     - Named header values
--     - Named query/matrix parameter values
--     - Named capture values
--     - Body data if any
--     - origin (protocol (secure?), host, port)
--
--   To make the url, we may have problems... no, I think a direct typeclass
--   approach will work. So long as every capture name and type is found in
--   the data, we can always construct it.
--
--   For matrix parameters, they're associated with a particular part of
--   the URL. Do we have to position them accordingly? Yeah, that breaks
--   things. Even non-capture parts can have matrix parameters, so I think
--   we *will* have to rely on order.
--
--   How about constraining non-POST methods to have an empty body?
--
{-
data XHRRequestData (method :: *) (headers :: [*]) (queryParams :: [*]) (body :: *) where
    XHRRequestData
        :: XHRRequestDataMethod method
        -> XHRRequestDataHeaders headers
        -> XHRRequestDatQueryParams queryParams
        -> XHRRequestDataBody body
        -> XHRRequestData method headers body

xhrRequestData :: XHRRequestData NoMethod NoHeaders NoBody
xhrRequestData = XHRRequestData (XHRRequestDataMethod)
                                (XHRRequestDataHeadersNil)
                                (XHRRequestDataBody Nothing)

xhrBody
    :: body
    -> XHRRequestData method headers NoBody
    -> XHRRequestData method headers body
xhrBody = undefined

xhrMethod
    :: Proxy method
    -> XHRRequestData NoMethod headers body
    -> XHRRequestData method headers body
xhrMethod = undefined

xhrHeader
    :: Header name t
    -> XHRRequestData method headers body
    -> XHRRequestData method ( '(name, t) ': headers ) body
xhrHeader = undefined
-}

