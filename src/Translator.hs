{-# LANGUAGE FlexibleContexts #-}
module Translator where

import Parser

class Translate a where
    translate :: a -> String

instance Translate IExp where
    translate (IExpInt a) = (show a)
    translate (IExpVar (Identifier a)) = a
    translate (IExp ie1 Plus ie2) = translate ie1 ++ " + " ++ translate ie2
    translate (IExp ie1 Minus ie2) = translate ie1 ++ " - " ++ translate ie2
    translate (IExp ie1 Mult ie2) = translate ie1 ++ " * " ++ translate ie2
    translate (IExp ie1 Div ie2) = translate ie1 ++ " / " ++ translate ie2 