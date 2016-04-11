{-|
Module      : Servant.Xhr
Description : Definition of servantXhr, for Xhrs to servant routes.
Copyright   : (c) Alexander Vieth, 2015
Licence     : BSD3
Maintainer  : aovieth@gmail.com
Stability   : experimental
Portability : non-portable (GHC only)
-}

{-# LANGUAGE AutoDeriveTypeable #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Servant.Xhr (

      servantXhrHandler
    , module Servant.Xhr.Request
    , module Servant.Xhr.Response

    ) where

import Prelude hiding (id, (.))
import Data.Proxy
import Control.Category
import Control.Arrow
import Reactive.DOM.Xhr
import Reactive.Banana.Combinators (Event)
import Reactive.Banana.Frameworks (MomentIO)
import Servant.Xhr.Request
import Servant.Xhr.Response
import Servant.API.ContentTypes

-- | An XhrHandler which uses the servant request and response types.
--   You almost certainly want to exploit the Profunctor-ness of XhrHandler
--   and use the servant request constructors like xhrQuery, xhrBody to
--   change the input.
--
--   TBD move location inside the profunctor? Move the proxy inside too?
servantXhrHandler
    :: forall servantRoute headers path query contentType reqBody resAccept resBody .
       ( MakeXhrServantRequest servantRoute headers path query contentType reqBody
       , resBody ~ XhrServantResponseBodyType servantRoute
       , resAccept ~ XhrServantResponseAccept servantRoute
       , AllCTUnrender resAccept resBody
       )
    => Proxy servantRoute
    -> Location
    -> XhrHandler (XhrServantRequest headers path query contentType reqBody)
                  (XhrServantResponse resBody)
servantXhrHandler proxyRoute location = arr input >>> xhrHandler >>> arr output
  where
    input :: XhrServantRequest headers path query contentType reqBody -> XhrRequest
    input = makeXhrServantRequest proxyRoute location
    output :: XhrResponse -> XhrServantResponse resBody
    output = makeXhrServantResponse (Proxy :: Proxy resAccept) (Proxy :: Proxy resBody)
