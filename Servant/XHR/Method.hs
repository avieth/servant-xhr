{-|
Module      : Servant.XHR.Method
Description : Definition of MakeXHRServantMethod
Copyright   : (c) Alexander Vieth, 2015
Licence     : BSD3
Maintainer  : aovieth@gmail.com
Stability   : experimental
Portability : non-portable (GHC only)
-}

{-# LANGUAGE AutoDeriveTypeable #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Servant.XHR.Method where

import Data.Proxy
import Reactive.DOM.XHR
import Servant.API

type family XHRServantMethod servantRoute where
    XHRServantMethod (Get contentTypes ty) = Get
    XHRServantMethod (Put contentTypes ty) = Put
    XHRServantMethod (Post contentTypes ty) = Post
    XHRServantMethod (Delete contentTypes ty) = Delete
    XHRServantMethod (Get contentTypes ty :> anything) = Get
    XHRServantMethod (Put contentTypes ty :> anything) = Put
    XHRServantMethod (Post contentTypes ty :> anything) = Post
    XHRServantMethod (Delete contentTypes ty :> anything) = Delete
    XHRServantMethod (anything :> anything') = XHRServantMethod anything'

class MakeXHRServantMethod servantRoute where
    makeXHRServantMethod :: Proxy servantRoute -> XHRMethod

instance MakeXHRServantMethod (Get contentTypes ty) where
    makeXHRServantMethod _ = GET

instance MakeXHRServantMethod (Put contentTypes ty) where 
    makeXHRServantMethod _ = PUT

instance MakeXHRServantMethod (Post contentTypes ty) where 
    makeXHRServantMethod _ = POST

instance MakeXHRServantMethod (Delete contentTypes ty) where 
    makeXHRServantMethod _ = DELETE

instance MakeXHRServantMethod (Get contentTypes ty :> rest) where
    makeXHRServantMethod _ = GET

instance MakeXHRServantMethod (Put contentTypes ty :> rest) where 
    makeXHRServantMethod _ = PUT

instance MakeXHRServantMethod (Post contentTypes ty :> rest) where 
    makeXHRServantMethod _ = POST

instance MakeXHRServantMethod (Delete contentTypes ty :> rest) where 
    makeXHRServantMethod _ = DELETE

instance {-# OVERLAPS #-}
    ( MakeXHRServantMethod anything'
    ) => MakeXHRServantMethod (anything :> anything')
  where
    makeXHRServantMethod _ = makeXHRServantMethod (Proxy :: Proxy anything')
