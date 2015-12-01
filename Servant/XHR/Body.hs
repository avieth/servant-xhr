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

-- | This class should be satisfied when the route has at least one
--   ReqBody in it. If there's more than one, the leftmost one wins, but
--   this is a silly thing to do so we don't worry about it.
class MakeXHRServantBody servantRoute contentType body where
    makeXHRServantBody
        :: Proxy servantRoute
        -> XHRServantBody contentType body
        -> Maybe T.Text

instance {-# OVERLAPS #-}
    ( HasContentType contentTypes contentType
    , MimeRender contentType body
    ) => MakeXHRServantBody ( ReqBody contentTypes body ) contentType (NonEmptyBody body)
  where
    makeXHRServantBody _ (XHRServantBodyNonEmpty proxyContentType body) =
        -- TODO handle exceptions in decodeUtf8.
        Just (decodeUtf8 (mimeRender proxyContentType body))

instance {-# OVERLAPS #-}
    ( HasContentType contentTypes contentType
    , MimeRender contentType body
    ) => MakeXHRServantBody ( ReqBody contentTypes body :> servantRoute ) contentType (NonEmptyBody body)
  where
    makeXHRServantBody _ (XHRServantBodyNonEmpty proxyContentType body) =
        -- TODO handle exceptions in decodeUtf8
        Just (decodeUtf8 (mimeRender proxyContentType body))

instance {-# OVERLAPS #-}
    ( MakeXHRServantBody servantRoute contentType EmptyBody
    ) => MakeXHRServantBody ( anything :> servantRoute ) contentType EmptyBody
  where
    makeXHRServantBody _ body = makeXHRServantBody (Proxy :: Proxy servantRoute) body

instance {-# OVERLAPS #-}
    ( MakeXHRServantBody servantRoute contentType (NonEmptyBody body)
    ) => MakeXHRServantBody ( anything :> servantRoute ) contentType (NonEmptyBody body)
  where
    makeXHRServantBody _ body = makeXHRServantBody (Proxy :: Proxy servantRoute) body

instance {-# OVERLAPS #-}
    (
    ) => MakeXHRServantBody ( anything ) contentType EmptyBody
  where
    makeXHRServantBody _ XHRServantBodyEmpty = Nothing
