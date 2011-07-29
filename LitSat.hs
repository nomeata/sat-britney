-- |
-- Module: ClauseSat
-- Copyright: (c) 2011 Joachim Breitner
-- License: GPL-2
--
module LitSat where

data Clause a 
    = OneOf [a] String
    | AtMostOne [a] String
    | AllOrNone [a] String
    | Implies a [a] String
    | NotBoth a a String
    | Not a String
    deriving (Ord,Eq,Show)
