{-|
Module      : Servant.Xhr.Request
Description : Definition of the XhrServantRequest datatype.
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

module Servant.Xhr.Request where

import GHC.TypeLits
import Data.Proxy
import qualified Data.Text as T
import Servant.Xhr.Method
import Servant.Xhr.Headers
import Servant.Xhr.Path
import Servant.Xhr.Query
import Servant.Xhr.Body
import Reactive.DOM.Xhr
import Servant.API
import Web.HttpApiData

type Location = T.Text

data XhrServantRequest headers path query contentType body where
    XhrServantRequest
        :: XhrServantHeaders headers
        -> XhrServantPath path
        -> XhrServantQuery query
        -> XhrServantBody contentType body
        -> XhrServantRequest headers path query contentType body

xhrServantRequest :: XhrServantRequest '[] '[] '[] contentType EmptyBody
xhrServantRequest = XhrServantRequest XhrServantHeadersNil
                                      XhrServantPathNil
                                      XhrServantQueryNil
                                      XhrServantBodyEmpty

xhrHeader
    :: ( KnownSymbol name
       , ToHttpApiData t
       )
    => Proxy name
    -> t
    -> XhrServantRequest headers path query contentType body
    -> XhrServantRequest ( '(name, t) ': headers ) path query contentType body
xhrHeader name t (XhrServantRequest headers path query body) =
    XhrServantRequest (XhrServantHeadersCons name t headers) path query body

xhrCapture
    :: ( KnownSymbol name
       , ToHttpApiData t
       )
    => Proxy name
    -> t
    -> XhrServantRequest headers path query contentType body
    -> XhrServantRequest headers ( '(name, t) ': path ) query contentType body
xhrCapture name t (XhrServantRequest headers path query body) =
    XhrServantRequest headers (XhrServantPathCons name t path) query body

xhrQuery
    :: ( KnownSymbol name
       , ToHttpApiData t
       )
    => Proxy name
    -> t
    -> XhrServantRequest headers path query contentType body
    -> XhrServantRequest headers path ( '(name, t) ': query ) contentType body
xhrQuery name t (XhrServantRequest headers path query body) =
    XhrServantRequest headers path (XhrServantQueryCons name t query) body

xhrBody
    :: ( )
    => Proxy contentType
    -> body
    -> XhrServantRequest headers path query contentType EmptyBody
    -> XhrServantRequest headers path query contentType (NonEmptyBody body)
xhrBody ctype body (XhrServantRequest headers path query _) =
    XhrServantRequest headers path query (XhrServantBodyNonEmpty ctype body)

class MakeXhrServantRequest servantRoute headers path query contentType body where
    makeXhrServantRequest
        :: Proxy servantRoute
        -> Location
        -> XhrServantRequest headers path query contentType body
        -> XhrRequest

-- TODO Accept header, from the Get/Post/Put/Delete type.
instance
    ( MakeXhrServantMethod servantRoute
    , MakeXhrServantHeaders servantRoute headers
    , MakeXhrServantPath servantRoute path
    , MakeXhrServantQuery servantRoute query
    , MakeXhrServantBody servantRoute contentType body
    , XhrServantCompatibleBody (XhrServantMethod servantRoute) body
    , AddXhrContentTypeHeader contentType body
    ) => MakeXhrServantRequest servantRoute headers path query contentType body
  where
    makeXhrServantRequest proxyServantRoute origin xhrData = case xhrData of
        XhrServantRequest headers path query body ->
            let xhrMethod = makeXhrServantMethod proxyServantRoute
                xhrHeaders = addXhrContentTypeHeader body (makeXhrServantHeaders proxyServantRoute headers)
                xhrPath = makeXhrServantPath proxyServantRoute path
                xhrQuery = makeXhrServantQuery proxyServantRoute query
                xhrBody = makeXhrServantBody proxyServantRoute body
                queryPart = if T.null xhrQuery then "" else T.concat ["?", xhrQuery]
            in  XhrRequest xhrMethod
                           (T.concat [origin, "/", xhrPath, queryPart])
                           xhrHeaders
                           xhrBody
                           Nothing

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
    print (makeXhrServantRequest example1 "http://localhost" example1D)
    print (makeXhrServantRequest example2 "https://localhost" example2D)
-}
