{-|
Module      : Servant.XHR.Body
Description : Definitions for constructing the request body from servant types.
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
{-# LANGUAGE UndecidableInstances #-}

module Servant.XHR.Body where

import GHC.TypeLits
import Data.Proxy
import qualified Data.Text.Lazy as T
import Data.Text.Lazy.Encoding
import Servant.API

data EmptyBody
data NonEmptyBody body

data XHRServantBody (contentType :: *) (body :: *) where
    XHRServantBodyEmpty :: XHRServantBody contentType EmptyBody
    XHRServantBodyNonEmpty
        :: Proxy contentType
        -> body
        -> XHRServantBody contentType (NonEmptyBody body)

class HasContentType (contentTypes :: [*]) (contentType :: *)
instance {-# OVERLAPS #-} HasContentType (c ': cs) c
instance {-# OVERLAPS #-}  HasContentType cs c => HasContentType (c' ': cs) c

-- | An XMLHttpRequest will set the body of a GET or HEAD to null.
--   This class ensures that a GET can have only the EmptyBody, so that if
--   you try to give a body to a GET, the program won't compile
class XHRServantCompatibleBody method body
instance XHRServantCompatibleBody Get EmptyBody
instance XHRServantCompatibleBody Post body
instance XHRServantCompatibleBody Put body
instance XHRServantCompatibleBody Delete body

-- | Pick out the request body content types and type, if there is a request
--   body at all.
type family RequestBodyType servantRoute :: Maybe ([*], *) where
    RequestBodyType (ReqBody contentTypes body) = 'Just '(contentTypes, body)
    RequestBodyType (ReqBody contentTypes body :> rest) = 'Just '(contentTypes, body)
    RequestBodyType (anything :> rest) = RequestBodyType rest
    RequestBodyType otherwise = 'Nothing

-- | This class should be satisfied when the route has at least one
--   ReqBody in it. If there's more than one, the leftmost one wins, but
--   this is a silly thing to do so we don't worry about it.
class MakeXHRServantBody servantRoute contentType body where
    makeXHRServantBody
        :: Proxy servantRoute
        -> XHRServantBody contentType body
        -> Maybe T.Text

instance
    ( XHRServantBodyCompatible (RequestBodyType servantRoute) contentType body
    ) => MakeXHRServantBody servantRoute contentType body
  where
    makeXHRServantBody _ =
        makeXHRServantBody_ (Proxy :: Proxy (RequestBodyType servantRoute))

class XHRServantBodyCompatible (reqBodyType :: Maybe ([*], *)) contentType body where
    makeXHRServantBody_
        :: Proxy reqBodyType
        -> XHRServantBody contentType body
        -> Maybe T.Text

instance
    ( HasContentType contentTypes contentType
    , MimeRender contentType body
    ) => XHRServantBodyCompatible ('Just '(contentTypes, body)) contentType (NonEmptyBody body)
  where
    makeXHRServantBody_ _ (XHRServantBodyNonEmpty proxyContentType body) =
        -- TODO handle exceptions in decodeUtf8.
        Just (decodeUtf8 (mimeRender proxyContentType body))

instance
    (
    ) => XHRServantBodyCompatible 'Nothing contentType EmptyBody
  where
    makeXHRServantBody_ _ _ = Nothing
