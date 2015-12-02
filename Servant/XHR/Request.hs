{-|
Module      : Servant.XHR.Request
Description : Definition of the XHRServantRequest datatype.
Copyright   : (c) Alexander Vieth, 2015
Licence     : BSD3
Maintainer  : aovieth@gmail.com
Stability   : experimental
Portability : non-portable (GHC only)
-}

{-# LANGUAGE AutoDeriveTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Servant.XHR.Request where

import GHC.TypeLits
import Data.Proxy
import qualified Data.Text as T
import Servant.XHR.Method
import Servant.XHR.Headers
import Servant.XHR.Path
import Servant.XHR.Query
import Servant.XHR.Body
import Reactive.DOM.XHR
import Servant.API
import Web.HttpApiData

type Location = T.Text

data XHRServantRequest headers path query contentType body where
    XHRServantRequest
        :: XHRServantHeaders headers
        -> XHRServantPath path
        -> XHRServantQuery query
        -> XHRServantBody contentType body
        -> XHRServantRequest headers path query contentType body

xhrServantRequest :: XHRServantRequest '[] '[] '[] contentType EmptyBody
xhrServantRequest = XHRServantRequest XHRServantHeadersNil
                                      XHRServantPathNil
                                      XHRServantQueryNil
                                      XHRServantBodyEmpty

xhrHeader
    :: ( KnownSymbol name
       , ToHttpApiData t
       )
    => Proxy name
    -> t
    -> XHRServantRequest headers path query contentType body
    -> XHRServantRequest ( '(name, t) ': headers ) path query contentType body
xhrHeader name t (XHRServantRequest headers path query body) =
    XHRServantRequest (XHRServantHeadersCons name t headers) path query body

xhrCapture
    :: ( KnownSymbol name
       , ToHttpApiData t
       )
    => Proxy name
    -> t
    -> XHRServantRequest headers path query contentType body
    -> XHRServantRequest headers ( '(name, t) ': path ) query contentType body
xhrCapture name t (XHRServantRequest headers path query body) =
    XHRServantRequest headers (XHRServantPathCons name t path) query body

xhrQuery
    :: ( KnownSymbol name
       , ToHttpApiData t
       )
    => Proxy name
    -> t
    -> XHRServantRequest headers path query contentType body
    -> XHRServantRequest headers path ( '(name, t) ': query ) contentType body
xhrQuery name t (XHRServantRequest headers path query body) =
    XHRServantRequest headers path (XHRServantQueryCons name t query) body

xhrBody
    :: ( )
    => Proxy contentType
    -> body
    -> XHRServantRequest headers path query contentType EmptyBody
    -> XHRServantRequest headers path query contentType (NonEmptyBody body)
xhrBody ctype body (XHRServantRequest headers path query _) =
    XHRServantRequest headers path query (XHRServantBodyNonEmpty ctype body)

class MakeXHRServantRequest servantRoute headers path query contentType body where
    makeXHRServantRequest
        :: Proxy servantRoute
        -> Location
        -> XHRServantRequest headers path query contentType body
        -> XHRRequest

-- TODO Accept header, from the Get/Post/Put/Delete type.
instance
    ( MakeXHRServantMethod servantRoute
    , MakeXHRServantHeaders servantRoute headers
    , MakeXHRServantPath servantRoute path
    , MakeXHRServantQuery servantRoute query
    , MakeXHRServantBody servantRoute contentType body
    , XHRServantCompatibleBody (XHRServantMethod servantRoute) body
    , AddXHRContentTypeHeader contentType body
    ) => MakeXHRServantRequest servantRoute headers path query contentType body
  where
    makeXHRServantRequest proxyServantRoute origin xhrData = case xhrData of
        XHRServantRequest headers path query body ->
            let xhrMethod = makeXHRServantMethod proxyServantRoute
                xhrHeaders = addXHRContentTypeHeader body (makeXHRServantHeaders proxyServantRoute headers)
                xhrPath = makeXHRServantPath proxyServantRoute path
                xhrQuery = makeXHRServantQuery proxyServantRoute query
                xhrBody = makeXHRServantBody proxyServantRoute body
                queryPart = if T.null xhrQuery then "" else T.concat ["?", xhrQuery]
            in  XHRRequest xhrMethod
                           (T.concat [origin, "/", xhrPath, queryPart])
                           xhrHeaders
                           xhrBody

{-
type Example1 =
       Header "auth" String
    :> "users"
    :> Capture "id" Int
    :> QueryParam "offset" Int
    :> QueryParam "count" Int
    :> ReqBody '[PlainText, JSON] T.Text
    :> Put '[JSON] String

example1 :: Proxy Example1
example1 = Proxy

example1D = xhrHeader (Proxy :: Proxy "auth") ("hello" :: String)
          . xhrCapture (Proxy :: Proxy "id") (42 :: Int)
          . xhrQuery (Proxy :: Proxy "count") (10 :: Int)
          . xhrQuery (Proxy :: Proxy "offset") (0 :: Int)
          . xhrBody (Proxy :: Proxy PlainText) ("This is the body" :: T.Text)
          $ xhrServantRequest

type Example2 =
       "users"
    :> ReqBody '[JSON] String
    :> Capture "a" Bool
    :> Capture "b" Int
    :> Post '[JSON, PlainText] Bool

example2 :: Proxy Example2
example2 = Proxy

example2D = xhrCapture (Proxy :: Proxy "a") (True)
          . xhrCapture (Proxy :: Proxy "b") (42 :: Int)
          . xhrBody (Proxy :: Proxy JSON) ("Hello world" :: String)
          $ xhrServantRequest

main = do
    print (makeXHRServantRequest example1 "http://localhost" example1D)
    print (makeXHRServantRequest example2 "https://localhost" example2D)
-}
