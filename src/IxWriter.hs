module IxWriter where

import           Control.Monad.Writer.Lazy

class IxMonad m  where
    ireturn
        :: (Monoid o)
        => rs -> m o pr po rs
    ibind
        :: (Monoid o)
        => m o pr1 po1 rs1 -> (rs1 -> m o po1 pr2 rs2) -> m o pr1 po2 rs2

-- Interpret like this:
newtype IxWriter o pr po rs = IXW
    { unIXW :: Writer o rs
    }

instance IxMonad IxWriter where
    ireturn = IXW . return
    ibind (IXW w) f = IXW (w >>= unIXW . f)

type SIxWriter pr po rs = IxWriter String pr po rs
