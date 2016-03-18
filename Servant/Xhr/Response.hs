{-|
Module      : Servant.Xhr.Response
Description : Definition of XhrServantResponseBody for response handling.
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

module Servant.Xhr.Response where

import Control.Applicative
import Data.Proxy
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.ByteString.Lazy as BL
import Data.Text.Lazy.Encoding
import Servant.API
import Servant.API.ContentTypes
import Reactive.DOM.Xhr

type family XhrServantResponseBodyType servantRoute where
    XhrServantResponseBodyType (Post ctypes t) = t
    XhrServantResponseBodyType (Get ctypes t) = t
    XhrServantResponseBodyType (Put ctypes t) = t
    XhrServantResponseBodyType (Delete ctypes t) = t
    XhrServantResponseBodyType (Post ctypes t :> rest) = t
    XhrServantResponseBodyType (Get ctypes t :> rest) = t
    XhrServantResponseBodyType (Put ctypes t :> rest) = t
    XhrServantResponseBodyType (Delete ctypes t :> rest) = t
    XhrServantResponseBodyType (any :> rest) = XhrServantResponseBodyType rest

type family XhrServantResponseAccept servantRoute where
    XhrServantResponseAccept (Post ctypes t) = ctypes
    XhrServantResponseAccept (Get ctypes t) = ctypes
    XhrServantResponseAccept (Put ctypes t) = ctypes
    XhrServantResponseAccept (Delete ctypes t) = ctypes
    XhrServantResponseAccept (Post ctypes t :> rest) = ctypes
    XhrServantResponseAccept (Get ctypes t :> rest) = ctypes
    XhrServantResponseAccept (Put ctypes t :> rest) = ctypes
    XhrServantResponseAccept (Delete ctypes t :> rest) = ctypes
    XhrServantResponseAccept (any :> rest) = XhrServantResponseAccept rest

-- | Just an XhrResponse but we'll try to parse the body to the type indicated
--   by the servant route.
data XhrServantResponse body = XhrServantResponse {
      xhrServantResponseRaw :: XhrResponse
    , xhrServantResponseParsedBody :: Maybe body
    }

useResponseStatus :: [XhrStatus] -> XhrServantResponse body -> Maybe body
useResponseStatus codes res =
    let code = xhrResponseStatus (xhrServantResponseRaw res)
    in  if elem code codes then xhrServantResponseParsedBody res else Nothing

newtype ResponseHandler body t = ResponseHandler {
      runResponseHandler :: XhrStatus -> Maybe body -> Maybe t
    }

instance Functor (ResponseHandler body) where
    fmap f (ResponseHandler g) = ResponseHandler ((fmap . fmap . fmap) f g)

instance Applicative (ResponseHandler body) where
    pure x = ResponseHandler $ \_ _ -> pure x
    (ResponseHandler l) <*> (ResponseHandler r) = ResponseHandler $ \code body ->
        l code body <*> r code body

instance Alternative (ResponseHandler body) where
    empty = ResponseHandler $ \_ _ -> empty
    (ResponseHandler l) <|> (ResponseHandler r) = ResponseHandler $ \code body ->
        l code body <|> r code body

responseHandleStatus :: XhrStatus -> (Maybe body -> t) -> ResponseHandler body t
responseHandleStatus code f = ResponseHandler $ \code' body ->
    if code == code'
    then Just (f body)
    else Nothing

withResponseHandler :: ResponseHandler body t -> XhrServantResponse body -> Maybe t
withResponseHandler handler res =
    let code = xhrResponseStatus (xhrServantResponseRaw res)
    in  runResponseHandler handler code (xhrServantResponseParsedBody res)

-- | Attempt to parse the body of an XhrResponse.
makeXhrServantResponse
    :: ( AllCTUnrender contentTypes body )
    => Proxy contentTypes
    -> Proxy body
    -> XhrResponse
    -> XhrServantResponse body
makeXhrServantResponse proxyCtypes proxyBody response =
    let parsedBody = case handleCTypeH proxyCtypes (getContentType response) (getResponseBody response) of
            Just (Right t) -> Just t
            _ -> Nothing
    in  XhrServantResponse response parsedBody
  where
    responseCode = xhrResponseStatus response
    isOK = responseCode < 300 && responseCode >= 200

getContentType :: XhrResponse -> BL.ByteString
getContentType response =
    let headers = (\(x, y) -> (T.toLower x, y)) <$> xhrResponseHeaders response
        contentTypes = filter ((==) "content-type" . fst) headers
    in  case contentTypes of
            [(x, y)] -> encodeUtf8 (TL.fromStrict y)
            _ -> ""

getResponseBody :: XhrResponse -> BL.ByteString
getResponseBody = maybe "" encodeUtf8 . xhrResponseBody
