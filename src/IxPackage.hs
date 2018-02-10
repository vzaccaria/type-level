{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE KindSignatures       #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE Rank2Types           #-}
{-# LANGUAGE RebindableSyntax     #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeInType           #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

module IxPackage where

import           Control.Monad.Writer.Lazy (Monad, Monoid, tell)
import           Data.Proxy
import           Data.String
import           GHC.TypeLits              ()
import           IxWriter

return
    :: (Monoid o, IxMonad m)
    => rs -> m o pr po rs
return = ireturn

(>>=)
    :: (Monoid o, IxMonad m)
    => m o pr1 po1 rs1 -> (rs1 -> m o po1 pr2 rs2) -> m o pr1 po2 rs2
(>>=) = ibind

v >> w =
    v >>=
    \_ ->
         w

-- <PackageName> is a kind (i.e. our 'types of types') with three inhabitant types
-- (<'Foo>, <'Bar>, <'Baz>). For each of them, we can construct an inhabitant value
-- with <(Proxy :: Proxy 'Bar)>.
data PackageState
    = Loaded
    | Initialized
    | Running

data PackageType
    = Major
    | Minor

data FileType
    = AllOf [String]
    | All

type PackageNames = [PackageName]

data PackageName
    = Foo
    | Bar
    | Baz

data Package (name :: pn) (state :: PackageState)
     (t :: PackageType) where
        Load ::
          Proxy (p :: pn) -> Proxy (t :: PackageType) -> Package p 'Loaded t
        Init ::
          Package (p :: pn) 'Loaded (t :: PackageType) ->
            Package p 'Initialized t
        EnableMajorOn ::
          Package (p :: pn) 'Initialized 'Major ->
            FileType -> Package p 'Running 'Major

load
    :: Proxy (p :: pnk)
    -> Proxy (t :: PackageType)
    -> SIxWriter () (Package p 'Loaded t) ()
load p t = do
    IXW (tell "Load xyz")

init
    :: Proxy (p :: pnk)
    -> Proxy (t :: PackageType)
    -> SIxWriter (Package p 'Loaded t) (Package p 'Initialized t) ()
init p t = do
    IXW (tell "Load xyz")

p1 = Proxy :: Proxy 'Bar

p2 = Proxy :: Proxy 'Foo

major = Proxy :: Proxy 'Major

myConfig :: SIxWriter () (Package 'Bar 'Initialized 'Major) ()
myConfig = do
    load p1 major
    init p1 major
