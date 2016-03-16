{-|
Module      : Servant.XHR
Description : Definition of servantXHR, for XHRs to servant routes.
Copyright   : (c) Alexander Vieth, 2015
Licence     : BSD3
Maintainer  : aovieth@gmail.com
Stability   : experimental
Portability : non-portable (GHC only)
-}

{-# LANGUAGE AutoDeriveTypeable #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Servant.XHR (

      servantXHRHandler
    , module Servant.XHR.Request
    , module Servant.XHR.Response

    ) where

import Data.Proxy
import Reactive.DOM.XHR
import Reactive.Banana.Combinators (Event)
import Reactive.Banana.Frameworks (MomentIO)
import Servant.XHR.Request
import Servant.XHR.Response
import Servant.API.ContentTypes

-- | An XHRHandler which uses the servant request and response types.
--   You almost certainly want to exploit the Profunctor-ness of XHRHandler
--   and use the servant request constructors like xhrQuery, xhrBody to
--   change the input.
servantXHRHandler
    :: forall servantRoute headers path query contentType reqBody resAccept resBody .
       ( MakeXHRServantRequest servantRoute headers path query contentType reqBody
       , resBody ~ XHRServantResponseBodyType servantRoute
       , resAccept ~ XHRServantResponseAccept servantRoute
       , AllCTUnrender resAccept resBody
       )
    => Proxy servantRoute
    -> Location
    -> XHRHandler (XHRServantRequest headers path query contentType reqBody)
                  (XHRServantResponse resBody)
servantXHRHandler proxyRoute location = xhrHandler $ \req ->
    ( makeXHRServantRequest proxyRoute location req
    , makeXHRServantResponse (Proxy :: Proxy resAccept) (Proxy :: Proxy resBody)
    )
