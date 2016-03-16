{-|
Module      : Servant.XHR.Response
Description : Definition of XHRServantResponseBody for response handling.
Copyright   : (c) Alexander Vieth, 2015
Licence     : BSD3
Maintainer  : aovieth@gmail.com
Stability   : experimental
Portability : non-portable (GHC only)
-}

{-# LANGUAGE AutoDeriveTypeable #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeOperators #-}

module Servant.XHR.Response where

import Data.Proxy
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.ByteString.Lazy as BL
import Data.Text.Lazy.Encoding
import Servant.API
import Servant.API.ContentTypes
import Reactive.DOM.XHR

type family XHRServantResponseBodyType servantRoute where
    XHRServantResponseBodyType (Post ctypes t) = t
    XHRServantResponseBodyType (Get ctypes t) = t
    XHRServantResponseBodyType (Put ctypes t) = t
    XHRServantResponseBodyType (Delete ctypes t) = t
    XHRServantResponseBodyType (Post ctypes t :> rest) = t
    XHRServantResponseBodyType (Get ctypes t :> rest) = t
    XHRServantResponseBodyType (Put ctypes t :> rest) = t
    XHRServantResponseBodyType (Delete ctypes t :> rest) = t
    XHRServantResponseBodyType (any :> rest) = XHRServantResponseBodyType rest

type family XHRServantResponseAccept servantRoute where
    XHRServantResponseAccept (Post ctypes t) = ctypes
    XHRServantResponseAccept (Get ctypes t) = ctypes
    XHRServantResponseAccept (Put ctypes t) = ctypes
    XHRServantResponseAccept (Delete ctypes t) = ctypes
    XHRServantResponseAccept (Post ctypes t :> rest) = ctypes
    XHRServantResponseAccept (Get ctypes t :> rest) = ctypes
    XHRServantResponseAccept (Put ctypes t :> rest) = ctypes
    XHRServantResponseAccept (Delete ctypes t :> rest) = ctypes
    XHRServantResponseAccept (any :> rest) = XHRServantResponseAccept rest

-- | Just an XHRResponse but we'll try to parse the body to the type indicated
--   by the servant route.
data XHRServantResponse body = XHRServantResponse {
      xhrServantResponseRaw :: XHRResponse
    , xhrServantResponseParsedBody :: Maybe body
    }

-- | Attempt to parse the body of an XHRResponse.
makeXHRServantResponse
    :: ( AllCTUnrender contentTypes body )
    => Proxy contentTypes
    -> Proxy body
    -> XHRResponse
    -> XHRServantResponse body
makeXHRServantResponse proxyCtypes proxyBody response =
    let parsedBody = case handleCTypeH proxyCtypes (getContentType response) (getResponseBody response) of
            Just (Right t) -> Just t
            _ -> Nothing
    in  XHRServantResponse response parsedBody
  where
    responseCode = xhrResponseStatus response
    isOK = responseCode < 300 && responseCode >= 200

getContentType :: XHRResponse -> BL.ByteString
getContentType response =
    let headers = (\(x, y) -> (T.toLower x, y)) <$> xhrResponseHeaders response
        contentTypes = filter ((==) "content-type" . fst) headers
    in  case contentTypes of
            [(x, y)] -> encodeUtf8 (TL.fromStrict y)
            _ -> ""

getResponseBody :: XHRResponse -> BL.ByteString
getResponseBody = maybe "" encodeUtf8 . xhrResponseBody
