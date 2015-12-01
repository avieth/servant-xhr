{-|
Module      : Servant.XHR.Query
Description : Definitions for constructing query parts from servant types.
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

module Servant.XHR.Query where

import GHC.TypeLits
import Data.Proxy
import qualified Data.Text as T
import Servant.API

-- | Value-level representation of the capture parts of a query.
--   The type parameter indicates the named, types parts of the query.
data XHRServantQuery (query :: [(Symbol, *)]) where
    XHRServantQueryNil :: XHRServantQuery '[]
    XHRServantQueryCons
        :: ( ToText t
           , KnownSymbol name
           )
        => Proxy name
        -> t
        -> XHRServantQuery rest
        -> XHRServantQuery ( '(name, t) ': rest )

-- | Drop part of a query. The first matching name, type pair in the query is
--   removed.
type family XHRServantQueryDrop (name :: Symbol) (ty :: *) (query :: [(Symbol, *)]) :: [(Symbol, *)] where
    XHRServantQueryDrop name ty ( '(name, ty) ': rest ) = rest
    XHRServantQueryDrop name ty ( '(name', ty') ': rest ) = '( name', ty' ) ': XHRServantQueryDrop name ty rest

-- | Drop part of a query. Every matching name, type pair in the query is
--   removed.
type family XHRServantQueryDropMultiple (name :: Symbol) (ty :: *) (query :: [(Symbol, *)]) :: [(Symbol, *)] where
    XHRServantQueryDropMultiple name ty '[] = '[]
    XHRServantQueryDropMultiple name ty ( '(name, ty) ': rest ) = XHRServantQueryDropMultiple name ty rest
    XHRServantQueryDropMultiple name ty ( '(name', ty') ': rest ) = '( name', ty' ) ': XHRServantQueryDropMultiple name ty rest


-- | Shows that a given capture part is in a query, with proofs showing that its
--   value can be extracted from an XHRServantQuery, and that an XHRServantQuery
--   can be shrunk to exclude it.
class
    ( ToText ty
    ) => InXHRServantQuery (name :: Symbol) (ty :: *) (query :: [(Symbol, *)])
  where
    xhrServantQueryGetValue :: Proxy name -> Proxy ty -> XHRServantQuery query -> ty
    xhrServantQueryDrop
        :: Proxy name
        -> Proxy ty
        -> XHRServantQuery query
        -> XHRServantQuery (XHRServantQueryDrop name ty query)

instance {-# OVERLAPS #-}
    ( ToText ty
    ) => InXHRServantQuery name ty ( '(name, ty) ': rest )
  where
    xhrServantQueryGetValue _ _ query = case query of
        XHRServantQueryCons _ t _ -> t
    xhrServantQueryDrop _ _ query = case query of
        XHRServantQueryCons _ _ rest -> rest

instance {-# OVERLAPS #-}
    ( InXHRServantQuery name ty rest
    ,   XHRServantQueryDrop name ty ( '(name', ty') ': rest )
      ~ ( '(name', ty') ': XHRServantQueryDrop name ty rest )
    ) => InXHRServantQuery name ty ( '(name', ty') ': rest )
  where
    xhrServantQueryGetValue proxyName proxyTy query = case query of
        XHRServantQueryCons _ _ rest -> xhrServantQueryGetValue proxyName proxyTy rest
    xhrServantQueryDrop proxyName proxyTy query = case query of
        XHRServantQueryCons proxyName' proxyTy' rest -> XHRServantQueryCons proxyName' proxyTy' (xhrServantQueryDrop proxyName proxyTy rest)

-- | Like InXHRServantQuery except that 0 or more values are found/dropped.
class
    ( ToText ty
    ) => InXHRServantQueryMultiple (name :: Symbol) (ty :: *) (query :: [(Symbol, *)])
  where
    xhrServantQueryGetValueMultiple
        :: Proxy name
        -> Proxy ty
        -> XHRServantQuery query
        -> [ty]
    xhrServantQueryDropMultiple
        :: Proxy name
        -> Proxy ty
        -> XHRServantQuery query
        -> XHRServantQuery (XHRServantQueryDropMultiple name ty query)

instance {-# OVERLAPS #-}
    ( ToText ty
    ) => InXHRServantQueryMultiple name ty '[]
  where
    xhrServantQueryGetValueMultiple _ _ _ = []
    xhrServantQueryDropMultiple _ _ _ = XHRServantQueryNil

instance {-# OVERLAPS #-}
    ( ToText ty
    , InXHRServantQueryMultiple name ty rest
    ) => InXHRServantQueryMultiple name ty ( '(name, ty) ': rest )
  where
    xhrServantQueryGetValueMultiple proxyName proxyTy query = case query of
        XHRServantQueryCons _ t rest -> t : xhrServantQueryGetValueMultiple proxyName proxyTy rest
    xhrServantQueryDropMultiple proxyName proxyTy query = case query of
        XHRServantQueryCons _ _ rest -> xhrServantQueryDropMultiple proxyName proxyTy rest

instance {-# OVERLAPS #-}
    ( InXHRServantQueryMultiple name ty rest
    ,   XHRServantQueryDropMultiple name ty ( '(name', ty') ': rest )
      ~ ( '(name', ty') ': XHRServantQueryDropMultiple name ty rest )
    ) => InXHRServantQueryMultiple name ty ( '(name', ty') ': rest )
  where
    xhrServantQueryGetValueMultiple proxyName proxyTy query = case query of
        XHRServantQueryCons _ _ rest -> xhrServantQueryGetValueMultiple proxyName proxyTy rest
    xhrServantQueryDropMultiple proxyName proxyTy query = case query of
        XHRServantQueryCons proxyName' proxyTy' rest -> XHRServantQueryCons proxyName' proxyTy' (xhrServantQueryDropMultiple proxyName proxyTy rest)



-- | Use XHRServantQuery to make the query string of a servant route.
--   TODO must escape the keys and values, since they may contain
--   '=', '&', etc.
makeXHRServantQuery
    :: forall servantRoute query .
       ( MakeXHRServantQuery servantRoute query )
    => Proxy servantRoute
    -> XHRServantQuery query
    -> T.Text
makeXHRServantQuery proxyRoute query =
    let parts = makeXHRServantQueryParts proxyRoute query
        keyValuePairs = makePair <$> parts 
    in  T.intercalate "&" keyValuePairs
  where
    makePair :: (T.Text, Either Bool T.Text) -> T.Text
    makePair (key, value) = case value of
        Left True -> key
        Left False -> ""
        Right value -> T.concat [key, "=", value]

-- | Shows that a given query provides all the details for the query of a servant
--   route.
--
--   We don't handle cases where a QueryParam, QueryFlag, or QueryParams is
--   at the very end of a route. We could, but that's not canonical; surely
--   the method should go at the end.
class MakeXHRServantQuery servantRoute query where
    makeXHRServantQueryParts
        :: Proxy servantRoute
        -> XHRServantQuery query
        -> [(T.Text, Either Bool T.Text)]

instance {-# OVERLAPS #-}
    (
    ) => MakeXHRServantQuery servantRoute '[]
  where
    makeXHRServantQueryParts _ _ = []

instance {-# OVERLAPS #-}
    ( InXHRServantQuery name Bool (q ': qs)
    , KnownSymbol name
    , MakeXHRServantQuery servantRoute (XHRServantQueryDrop name Bool (q ': qs))
    ) => MakeXHRServantQuery ( QueryFlag name :> servantRoute ) (q ': qs)
  where
    makeXHRServantQueryParts _ query =
          (toText (symbolVal (Proxy :: Proxy name)), Left (xhrServantQueryGetValue (Proxy :: Proxy name) (Proxy :: Proxy Bool) query))
        : makeXHRServantQueryParts (Proxy :: Proxy servantRoute) (xhrServantQueryDrop (Proxy :: Proxy name) (Proxy :: Proxy Bool) query)

instance {-# OVERLAPS #-}
    ( InXHRServantQuery name t (q ': qs)
    , KnownSymbol name
    , MakeXHRServantQuery servantRoute (XHRServantQueryDrop name t (q ': qs))
    ) => MakeXHRServantQuery ( QueryParam name t :> servantRoute ) (q ': qs)
  where
    makeXHRServantQueryParts _ query =
          (toText (symbolVal (Proxy :: Proxy name)), Right (toText (xhrServantQueryGetValue (Proxy :: Proxy name) (Proxy :: Proxy t) query)))
        : makeXHRServantQueryParts (Proxy :: Proxy servantRoute) (xhrServantQueryDrop (Proxy :: Proxy name) (Proxy :: Proxy t) query)

instance {-# OVERLAPS #-}
    ( InXHRServantQueryMultiple name t (q ': qs)
    , KnownSymbol name
    , MakeXHRServantQuery servantRoute (XHRServantQueryDropMultiple name t (q ': qs))
    ) => MakeXHRServantQuery ( QueryParams name t :> servantRoute ) (q ': qs)
  where
    makeXHRServantQueryParts _ query =
        let multiple :: [t]
            multiple = xhrServantQueryGetValueMultiple (Proxy :: Proxy name) (Proxy :: Proxy t) query
            key = toText (symbolVal (Proxy :: Proxy name))
            makePart :: t -> (T.Text, Either Bool T.Text)
            makePart t = (key, Right (toText t))
            parts :: [(T.Text, Either Bool T.Text)]
            parts = makePart <$> multiple
            theRest = makeXHRServantQueryParts (Proxy :: Proxy servantRoute) (xhrServantQueryDropMultiple (Proxy :: Proxy name) (Proxy :: Proxy t) query)
        in  parts ++ theRest

instance {-# OVERLAPS #-}
    ( InXHRServantQueryMultiple name t '[]
    , KnownSymbol name
    , MakeXHRServantQuery servantRoute (XHRServantQueryDropMultiple name t query)
    ) => MakeXHRServantQuery ( QueryParams name t :> servantRoute ) query
  where
    makeXHRServantQueryParts _ _ = []

instance {-# OVERLAPS #-}
    ( MakeXHRServantQuery servantRoute '[]
    ) => MakeXHRServantQuery ( anything :> servantRoute ) '[]
  where
    makeXHRServantQueryParts _ = makeXHRServantQueryParts (Proxy :: Proxy servantRoute)

instance {-# OVERLAPS #-}
    ( MakeXHRServantQuery servantRoute (q ': qs)
    ) => MakeXHRServantQuery ( anything :> servantRoute ) (q ': qs)
  where
    makeXHRServantQueryParts _ = makeXHRServantQueryParts (Proxy :: Proxy servantRoute)

{-
type Example = "foo" :> QueryParam "location" Int :> QueryParams "user" String :> Get '[] ()
example :: Proxy Example
example = Proxy
exampleD = XHRServantQueryCons (Proxy :: Proxy "user") ("john" :: String)
         . XHRServantQueryCons (Proxy :: Proxy "location") (42 :: Int)
         . XHRServantQueryCons (Proxy :: Proxy "user") ("joe" :: String)
         $ XHRServantQueryNil
-}
