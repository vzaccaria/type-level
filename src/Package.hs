{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE KindSignatures       #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE Rank2Types           #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeInType           #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

module Main where

import           Data.Proxy
import           GHC.Exts
import           GHC.TypeLits ()

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

-- This type family is a type function (whose result is of kind Constraint, that
-- has a single inhabitant type Unit). It allows to detect if two types e and
-- es, whose kinds are structurally compatible, share the at least one type.
type family ContainedIn (e :: pn) (es :: pns) :: Constraint where
        ContainedIn x (x ': xs) = ()
        ContainedIn y (x ': xs) = ContainedIn y xs

main :: IO ()
main = putStrLn "hello world"
