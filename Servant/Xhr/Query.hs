{-|
Module      : Servant.Xhr.Query
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

module Servant.Xhr.Query where

import GHC.TypeLits
import Data.Proxy
import qualified Data.Text as T
import Servant.API
import Web.HttpApiData

-- | Value-level representation of the capture parts of a query.
--   The type parameter indicates the named, types parts of the query.
data XhrServantQuery (query :: [(Symbol, *)]) where
    XhrServantQueryNil :: XhrServantQuery '[]
    XhrServantQueryCons
        :: ( ToHttpApiData t
           , KnownSymbol name
           )
        => Proxy name
        -> t
        -> XhrServantQuery rest
        -> XhrServantQuery ( '(name, t) ': rest )

-- | Drop part of a query. The first matching name, type pair in the query is
--   removed.
type family XhrServantQueryDrop (name :: Symbol) (ty :: *) (query :: [(Symbol, *)]) :: [(Symbol, *)] where
    XhrServantQueryDrop name ty ( '(name, ty) ': rest ) = rest
    XhrServantQueryDrop name ty ( '(name', ty') ': rest ) = '( name', ty' ) ': XhrServantQueryDrop name ty rest

-- | Drop part of a query. Every matching name, type pair in the query is
--   removed.
type family XhrServantQueryDropMultiple (name :: Symbol) (ty :: *) (query :: [(Symbol, *)]) :: [(Symbol, *)] where
    XhrServantQueryDropMultiple name ty '[] = '[]
    XhrServantQueryDropMultiple name ty ( '(name, ty) ': rest ) = XhrServantQueryDropMultiple name ty rest
    XhrServantQueryDropMultiple name ty ( '(name', ty') ': rest ) = '( name', ty' ) ': XhrServantQueryDropMultiple name ty rest


-- | Shows that a given capture part is in a query, with proofs showing that its
--   value can be extracted from an XhrServantQuery, and that an XhrServantQuery
--   can be shrunk to exclude it.
class
    ( ToHttpApiData ty
    ) => InXhrServantQuery (name :: Symbol) (ty :: *) (query :: [(Symbol, *)])
  where
    xhrServantQueryGetValue :: Proxy name -> Proxy ty -> XhrServantQuery query -> ty
    xhrServantQueryDrop
        :: Proxy name
        -> Proxy ty
        -> XhrServantQuery query
        -> XhrServantQuery (XhrServantQueryDrop name ty query)

instance {-# OVERLAPS #-}
    ( ToHttpApiData ty
    ) => InXhrServantQuery name ty ( '(name, ty) ': rest )
  where
    xhrServantQueryGetValue _ _ query = case query of
        XhrServantQueryCons _ t _ -> t
    xhrServantQueryDrop _ _ query = case query of
        XhrServantQueryCons _ _ rest -> rest

instance {-# OVERLAPS #-}
    ( InXhrServantQuery name ty rest
    ,   XhrServantQueryDrop name ty ( '(name', ty') ': rest )
      ~ ( '(name', ty') ': XhrServantQueryDrop name ty rest )
    ) => InXhrServantQuery name ty ( '(name', ty') ': rest )
  where
    xhrServantQueryGetValue proxyName proxyTy query = case query of
        XhrServantQueryCons _ _ rest -> xhrServantQueryGetValue proxyName proxyTy rest
    xhrServantQueryDrop proxyName proxyTy query = case query of
        XhrServantQueryCons proxyName' proxyTy' rest -> XhrServantQueryCons proxyName' proxyTy' (xhrServantQueryDrop proxyName proxyTy rest)

-- | Like InXhrServantQuery except that 0 or more values are found/dropped.
class
    ( ToHttpApiData ty
    ) => InXhrServantQueryMultiple (name :: Symbol) (ty :: *) (query :: [(Symbol, *)])
  where
    xhrServantQueryGetValueMultiple
        :: Proxy name
        -> Proxy ty
        -> XhrServantQuery query
        -> [ty]
    xhrServantQueryDropMultiple
        :: Proxy name
        -> Proxy ty
        -> XhrServantQuery query
        -> XhrServantQuery (XhrServantQueryDropMultiple name ty query)

instance {-# OVERLAPS #-}
    ( ToHttpApiData ty
    ) => InXhrServantQueryMultiple name ty '[]
  where
    xhrServantQueryGetValueMultiple _ _ _ = []
    xhrServantQueryDropMultiple _ _ _ = XhrServantQueryNil

instance {-# OVERLAPS #-}
    ( ToHttpApiData ty
    , InXhrServantQueryMultiple name ty rest
    ) => InXhrServantQueryMultiple name ty ( '(name, ty) ': rest )
  where
    xhrServantQueryGetValueMultiple proxyName proxyTy query = case query of
        XhrServantQueryCons _ t rest -> t : xhrServantQueryGetValueMultiple proxyName proxyTy rest
    xhrServantQueryDropMultiple proxyName proxyTy query = case query of
        XhrServantQueryCons _ _ rest -> xhrServantQueryDropMultiple proxyName proxyTy rest

instance {-# OVERLAPS #-}
    ( InXhrServantQueryMultiple name ty rest
    ,   XhrServantQueryDropMultiple name ty ( '(name', ty') ': rest )
      ~ ( '(name', ty') ': XhrServantQueryDropMultiple name ty rest )
    ) => InXhrServantQueryMultiple name ty ( '(name', ty') ': rest )
  where
    xhrServantQueryGetValueMultiple proxyName proxyTy query = case query of
        XhrServantQueryCons _ _ rest -> xhrServantQueryGetValueMultiple proxyName proxyTy rest
    xhrServantQueryDropMultiple proxyName proxyTy query = case query of
        XhrServantQueryCons proxyName' proxyTy' rest -> XhrServantQueryCons proxyName' proxyTy' (xhrServantQueryDropMultiple proxyName proxyTy rest)



-- | Use XhrServantQuery to make the query string of a servant route.
--   TODO must escape the keys and values, since they may contain
--   '=', '&', etc.
makeXhrServantQuery
    :: forall servantRoute query .
       ( MakeXhrServantQuery servantRoute query )
    => Proxy servantRoute
    -> XhrServantQuery query
    -> T.Text
makeXhrServantQuery proxyRoute query =
    let parts = makeXhrServantQueryParts proxyRoute query
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
class MakeXhrServantQuery servantRoute query where
    makeXhrServantQueryParts
        :: Proxy servantRoute
        -> XhrServantQuery query
        -> [(T.Text, Either Bool T.Text)]

instance {-# OVERLAPS #-}
    (
    ) => MakeXhrServantQuery servantRoute '[]
  where
    makeXhrServantQueryParts _ _ = []

instance {-# OVERLAPS #-}
    ( InXhrServantQuery name Bool (q ': qs)
    , KnownSymbol name
    , MakeXhrServantQuery servantRoute (XhrServantQueryDrop name Bool (q ': qs))
    ) => MakeXhrServantQuery ( QueryFlag name :> servantRoute ) (q ': qs)
  where
    makeXhrServantQueryParts _ query =
          (toQueryParam (symbolVal (Proxy :: Proxy name)), Left (xhrServantQueryGetValue (Proxy :: Proxy name) (Proxy :: Proxy Bool) query))
        : makeXhrServantQueryParts (Proxy :: Proxy servantRoute) (xhrServantQueryDrop (Proxy :: Proxy name) (Proxy :: Proxy Bool) query)

instance {-# OVERLAPS #-}
    ( InXhrServantQuery name t (q ': qs)
    , KnownSymbol name
    , MakeXhrServantQuery servantRoute (XhrServantQueryDrop name t (q ': qs))
    ) => MakeXhrServantQuery ( QueryParam name t :> servantRoute ) (q ': qs)
  where
    makeXhrServantQueryParts _ query =
          (toQueryParam (symbolVal (Proxy :: Proxy name)), Right (toQueryParam (xhrServantQueryGetValue (Proxy :: Proxy name) (Proxy :: Proxy t) query)))
        : makeXhrServantQueryParts (Proxy :: Proxy servantRoute) (xhrServantQueryDrop (Proxy :: Proxy name) (Proxy :: Proxy t) query)

instance {-# OVERLAPS #-}
    ( InXhrServantQueryMultiple name t (q ': qs)
    , KnownSymbol name
    , MakeXhrServantQuery servantRoute (XhrServantQueryDropMultiple name t (q ': qs))
    ) => MakeXhrServantQuery ( QueryParams name t :> servantRoute ) (q ': qs)
  where
    makeXhrServantQueryParts _ query =
        let multiple :: [t]
            multiple = xhrServantQueryGetValueMultiple (Proxy :: Proxy name) (Proxy :: Proxy t) query
            key = toQueryParam (symbolVal (Proxy :: Proxy name))
            makePart :: t -> (T.Text, Either Bool T.Text)
            makePart t = (key, Right (toQueryParam t))
            parts :: [(T.Text, Either Bool T.Text)]
            parts = makePart <$> multiple
            theRest = makeXhrServantQueryParts (Proxy :: Proxy servantRoute) (xhrServantQueryDropMultiple (Proxy :: Proxy name) (Proxy :: Proxy t) query)
        in  parts ++ theRest

instance {-# OVERLAPS #-}
    ( InXhrServantQueryMultiple name t '[]
    , KnownSymbol name
    , MakeXhrServantQuery servantRoute (XhrServantQueryDropMultiple name t query)
    ) => MakeXhrServantQuery ( QueryParams name t :> servantRoute ) query
  where
    makeXhrServantQueryParts _ _ = []

instance {-# OVERLAPS #-}
    ( MakeXhrServantQuery servantRoute '[]
    ) => MakeXhrServantQuery ( anything :> servantRoute ) '[]
  where
    makeXhrServantQueryParts _ = makeXhrServantQueryParts (Proxy :: Proxy servantRoute)

instance {-# OVERLAPS #-}
    ( MakeXhrServantQuery servantRoute (q ': qs)
    ) => MakeXhrServantQuery ( anything :> servantRoute ) (q ': qs)
  where
    makeXhrServantQueryParts _ = makeXhrServantQueryParts (Proxy :: Proxy servantRoute)

{-
type Example = "foo" :> QueryParam "location" Int :> QueryParams "user" String :> Get '[] ()
example :: Proxy Example
example = Proxy
exampleD = XhrServantQueryCons (Proxy :: Proxy "user") ("john" :: String)
         . XhrServantQueryCons (Proxy :: Proxy "location") (42 :: Int)
         . XhrServantQueryCons (Proxy :: Proxy "user") ("joe" :: String)
         $ XhrServantQueryNil
-}
