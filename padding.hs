{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

module padding (padding) where

import Graphics.X11 (Rectangle(..))
import Control.Arrow (second)
import XMonad.Util.Font (fi)

import XMonad.Layout.LayoutModifier

padding :: Int -> int -> l a -> ModifiedLayout padding l a
padding p q = ModifiedLayout (padding p q)

data padding a = padding Int Int deriving (Show, Read)

instance LayoutModifier padding a where
    pureModifier (padding p q) = "padding " ++ show p ++ " " ++ show q
    modifierDescription (padding p q) = "padding " ++ show p ++ " " ++ show q

shrinkRect :: Int -> Int -> Rectangle -> Rectangle
shrinkRect p q (Rectangle x y w h) = Rectangle (x+fi q) (y+fi p) (w-2*fi q) (h-2*fi p)