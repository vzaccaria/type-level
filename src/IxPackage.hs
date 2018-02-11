{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module IxPackage where

import Control.Monad.Writer.Lazy (Monad, Monoid, return, tell)
import Data.Proxy
import Data.String
import GHC.Exts
import GHC.TypeLits ()
import IxWriter

-- return = ireturn
(>>=) = ibind

v >> w =
    v >>=
    \_ ->
         w

-- <PackageName> is a kind (i.e. our 'types of types') with three inhabitant types
-- (<'Foo>, <'Bar>, <'Baz>). For each of them, we can construct an inhabitant value
-- with <(Proxy :: Proxy 'Bar)>.
data PackageState
    = Needed
    | Loaded
    | Initialized
    | Running

type PackageNames = [PackageName]

data PackageName
    = Foo
    | Bar
    | Baz

data Package (name :: pn) (state :: PackageState)

type PNeeded p = Package p 'Needed

type PLoaded p = Package p 'Loaded

type PInited p = Package p 'Initialized

load
    :: (PNeeded p :^: ctx0)
    => Proxy (p :: pnk) -> SIxWriter ctx0 (PLoaded p ': ctx0) ()
load p = do
    IXW (tell "Load xyz")

init
    :: (PLoaded p :^: ctx0)
    => Proxy (p :: pnk) -> SIxWriter ctx0 (PInited p ': ctx0) ()
init p = do
    IXW (tell "Load xyz")

p1 = Proxy :: Proxy 'Bar

p2 = Proxy :: Proxy 'Foo

-- This should typecheck, but cant prove ctx1 ~ ...
-- myBarConfig
--     :: (PNeeded 'Bar :^: ctx0, PInited 'Bar :^: ctx1)
--     => IxWriter String ctx0 ctx1 ()
myBarConfig
    :: (PNeeded 'Bar :^: ctx0)
    => IxWriter String ctx0 (PInited 'Bar : PLoaded 'Bar : ctx0) ()
myBarConfig = do
    load p1
    init p1