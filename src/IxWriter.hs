{-# LANGUAGE AllowAmbiguousTypes       #-}
{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE KindSignatures            #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE PolyKinds                 #-}
{-# LANGUAGE Rank2Types                #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE TypeInType                #-}
{-# LANGUAGE TypeOperators             #-}
{-# LANGUAGE UndecidableInstances      #-}

module IxWriter where

import           Control.Monad.Writer.Lazy
import           Data.Kind
import           GHC.Exts                  ()

type family (:^:) (e :: u) (es :: [u]) :: Constraint where
        x :^: (x ': xs) = ()
        y :^: (x ': xs) = y :^: xs

class IxMonad m  where
    ibind
        :: (Monoid o)
        => m o ctx0 ctx1 rs1 -> (rs1 -> m o ctx1 ctx2 rs2) -> m o ctx0 ctx2 rs2

-- Interpret like this:
newtype IxWriter o ctx0 ctx1 rs = IXW
    { unIXW :: Writer o rs
    }

instance IxMonad IxWriter where
    ibind (IXW w) f = IXW (w >>= unIXW . f)

type SIxWriter pr po rs = IxWriter String pr po rs
