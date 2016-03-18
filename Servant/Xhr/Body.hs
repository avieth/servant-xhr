{-|
Module      : Servant.Xhr.Body
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

module Servant.Xhr.Body where

import GHC.TypeLits
import Data.Proxy
import qualified Data.Text.Lazy as T
import Data.Text.Lazy.Encoding
import Servant.API

data EmptyBody
data NonEmptyBody body

data XhrServantBody (contentType :: *) (body :: *) where
    XhrServantBodyEmpty :: XhrServantBody contentType EmptyBody
    XhrServantBodyNonEmpty
        :: Proxy contentType
        -> body
        -> XhrServantBody contentType (NonEmptyBody body)

class HasContentType (contentTypes :: [*]) (contentType :: *)
instance {-# OVERLAPS #-} HasContentType (c ': cs) c
instance {-# OVERLAPS #-}  HasContentType cs c => HasContentType (c' ': cs) c

-- | An XMLHttpRequest will set the body of a GET or HEAD to null.
--   This class ensures that a GET can have only the EmptyBody, so that if
--   you try to give a body to a GET, the program won't compile
class XhrServantCompatibleBody method body
instance XhrServantCompatibleBody Get EmptyBody
instance XhrServantCompatibleBody Post body
instance XhrServantCompatibleBody Put body
instance XhrServantCompatibleBody Delete body

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
class MakeXhrServantBody servantRoute contentType body where
    makeXhrServantBody
        :: Proxy servantRoute
        -> XhrServantBody contentType body
        -> Maybe T.Text

instance
    ( XhrServantBodyCompatible (RequestBodyType servantRoute) contentType body
    ) => MakeXhrServantBody servantRoute contentType body
  where
    makeXhrServantBody _ =
        makeXhrServantBody_ (Proxy :: Proxy (RequestBodyType servantRoute))

class XhrServantBodyCompatible (reqBodyType :: Maybe ([*], *)) contentType body where
    makeXhrServantBody_
        :: Proxy reqBodyType
        -> XhrServantBody contentType body
        -> Maybe T.Text

instance
    ( HasContentType contentTypes contentType
    , MimeRender contentType body
    ) => XhrServantBodyCompatible ('Just '(contentTypes, body)) contentType (NonEmptyBody body)
  where
    makeXhrServantBody_ _ (XhrServantBodyNonEmpty proxyContentType body) =
        -- TODO handle exceptions in decodeUtf8.
        Just (decodeUtf8 (mimeRender proxyContentType body))

instance
    (
    ) => XhrServantBodyCompatible 'Nothing contentType EmptyBody
  where
    makeXhrServantBody_ _ _ = Nothing
