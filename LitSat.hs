module LitSat where

data Clause a 
    = OneOf [a] String
    | AtMostOne [a] String
    | Implies a [a] String
    | NotBoth a a String
    | Not a String
    deriving (Ord,Eq,Show)
