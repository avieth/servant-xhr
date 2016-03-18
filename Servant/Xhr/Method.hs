{-|
Module      : Servant.Xhr.Method
Description : Definition of MakeXhrServantMethod
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

module Servant.Xhr.Method where

import Data.Proxy
import Reactive.DOM.Xhr
import Servant.API

type family XhrServantMethod servantRoute where
    XhrServantMethod (Get contentTypes ty) = Get
    XhrServantMethod (Put contentTypes ty) = Put
    XhrServantMethod (Post contentTypes ty) = Post
    XhrServantMethod (Delete contentTypes ty) = Delete
    XhrServantMethod (Get contentTypes ty :> anything) = Get
    XhrServantMethod (Put contentTypes ty :> anything) = Put
    XhrServantMethod (Post contentTypes ty :> anything) = Post
    XhrServantMethod (Delete contentTypes ty :> anything) = Delete
    XhrServantMethod (anything :> anything') = XhrServantMethod anything'

class MakeXhrServantMethod servantRoute where
    makeXhrServantMethod :: Proxy servantRoute -> XhrMethod

instance MakeXhrServantMethod (Get contentTypes ty) where
    makeXhrServantMethod _ = GET

instance MakeXhrServantMethod (Put contentTypes ty) where 
    makeXhrServantMethod _ = PUT

instance MakeXhrServantMethod (Post contentTypes ty) where 
    makeXhrServantMethod _ = POST

instance MakeXhrServantMethod (Delete contentTypes ty) where 
    makeXhrServantMethod _ = DELETE

instance MakeXhrServantMethod (Get contentTypes ty :> rest) where
    makeXhrServantMethod _ = GET

instance MakeXhrServantMethod (Put contentTypes ty :> rest) where 
    makeXhrServantMethod _ = PUT

instance MakeXhrServantMethod (Post contentTypes ty :> rest) where 
    makeXhrServantMethod _ = POST

instance MakeXhrServantMethod (Delete contentTypes ty :> rest) where 
    makeXhrServantMethod _ = DELETE

instance {-# OVERLAPS #-}
    ( MakeXhrServantMethod anything'
    ) => MakeXhrServantMethod (anything :> anything')
  where
    makeXhrServantMethod _ = makeXhrServantMethod (Proxy :: Proxy anything')
