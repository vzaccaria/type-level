{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE PolyKinds #-}

module Main where

import Data.Proxy
import GHC.Exts
import GHC.TypeLits ()


-- <PackageName> is a kind (i.e. our 'types of types') with three inhabitant types
-- (<'Foo>, <'Bar>, <'Baz>). For each of them, we can construct an inhabitant value
-- with <(Proxy :: Proxy 'Bar)>.

data PackageName
  = Foo
  | Bar
  | Baz




-- This type family is a type function (whose result is of kind Constraint, that
-- has a single inhabitant type Unit). It allows to detect if two types e and
-- es, whose kinds are structurally compatible, share the at least one type.

type family Elem (e :: PackageName) (es ::[PackageName]) :: Constraint where
        Elem x (x ': xs) = ()
        Elem y (x ': xs) = Elem y xs

class HasImportInstructions (p :: PackageName)  where
  requirePackage
    :: Proxy (p :: PackageName) -> IO ()
  packageName
    :: Proxy (p :: PackageName) -> String

instance HasImportInstructions 'Bar where
  requirePackage _ = putStrLn "Bar"

-- Polymorphic function that typechecks only if has a type contained in ps.
-- Since types will be inferred by the type checker, either the values we use
-- have the right type, or they will not type check.

runRequirePackage :: (Elem p ps,HasImportInstructions p)
                  => Proxy (p :: PackageName)
                  -> Proxy (ps :: [PackageName])
                  -> IO ()
runRequirePackage p1 p2 = requirePackage p1



-- runBar will typecheck a context type only if it contains a 'Bar
runBar :: (Elem 'Bar p) => Proxy (p :: [PackageName]) -> IO ()
runBar  = runRequirePackage (Proxy :: Proxy 'Bar)

-- Type <MyContextPackages> (and its inhabitant <packages>) represents the actual context
-- in which we will will run our query. It has kind <'[PackageName]>. 
type MyContextPackages = 'Bar ': 'Baz ': '[]
type MyFailedContextPackages = 'Baz ': '[]

-- This will typecheck 
reqins1 = runBar (Proxy :: Proxy MyContextPackages)

-- This will not type check
-- reqins2 = runBar (Proxy :: Proxy MyFailedContextPackages)


main :: IO ()
main = putStrLn "hello world"
