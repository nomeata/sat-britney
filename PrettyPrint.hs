-- |
-- Module: PrettyPrint
-- Copyright: (c) 2011 Joachim Breitner
-- License: GPL-2
--
module PrettyPrint where

import Text.PrettyPrint
import Data.IntMap ((!))

import Types
import Arches
import Indices
import LitSat
import AtomIndex

class PP a where pp :: AtomIndex -> a -> Doc

instance PP a => PP (Clause a) where
    pp ai (OneOf [atom] reason) = 
        text "definitely" <+> pp ai atom <+> text "because" <+> text reason
    pp ai (OneOf atoms reason) = 
        text "one of" <+> listSep empty (text "or") (map (pp ai) atoms)
        <+> text "because" <+> text reason
    pp ai (AtMostOne atoms reason) = 
        text "at most one of" <+> listSep empty (text "or") (map (pp ai) atoms)
        <+> text "because" <+> text reason
    pp ai (AllOrNone atoms reason) = 
        text "all or nothing of" <+> listSep empty (text "and") (map (pp ai) atoms)
        <+> text "because" <+> text reason
    pp ai (Implies atom [] reason) = 
        pp ai atom <+> text "cannot be fulfilled"
        <+> text "because" <+> text reason
    pp ai (Implies atom atoms reason) = 
        pp ai atom <+> text "implies" <+> listSep empty (text "or") (map (pp ai) atoms)
        <+> text "because" <+> text reason
    pp ai (NotBoth atom atom2 reason) = 
        text "not both" <+> pp ai atom <+> text "and" <+> pp ai atom2 
        <+> text "because" <+> text reason
    pp ai (Not atom reason) = 
        text "not" <+> pp ai atom
        <+> text "because" <+> text reason

instance PP Arch where
    pp _ a = text (show a)

instance PP Atom where
    pp ai (InstAtom (Inst f p a)) = pp ai p <> char '@' <> pp ai f <> char '@' <> pp ai a
    pp _ a = text (show a)

instance PP (Index a) where pp ai i = pp ai $ ai `lookupAny` i 

listSep :: Doc -> Doc -> [Doc] -> Doc
listSep sep1 sep2 = go
  where go [] = empty
        go [d] = d
        go [d1,d2] = d1 <+> sep2 <+> d2
        go (d:ds) = d <+> sep1 <+> go ds
