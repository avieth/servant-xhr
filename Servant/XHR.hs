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

module Servant.XHR where

import Data.Proxy
import Reactive.Sequence
import Reactive.DOM.XHR
import Servant.XHR.Request
import Servant.XHR.Response
import Servant.API.ContentTypes

servantXHR
    :: forall servantRoute headers path query contentType reqBody resAccept resBody .
       ( MakeXHRServantRequest servantRoute headers path query contentType reqBody
       , resBody ~ XHRServantResponseBodyType servantRoute
       , resAccept ~ XHRServantResponseAccept servantRoute
       , AllCTUnrender resAccept resBody
       )
    => Proxy servantRoute
    -> Origin
    -> XHRServantRequest headers path query contentType reqBody
    -> SEvent (XHRServantResponse resBody)
servantXHR proxyRoute origin = fmap output . xhr . input
  where
    input :: XHRServantRequest headers path query contentType reqBody -> XHRRequest
    input = makeXHRServantRequest proxyRoute origin

    output :: XHRResponse -> XHRServantResponse resBody
    output = makeXHRServantResponse (Proxy :: Proxy resAccept) (Proxy :: Proxy resBody)
