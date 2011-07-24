module PrettyPrint where

import Text.PrettyPrint
import Data.Map ((!))

import Types
import LitSat

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
    pp ai (Implies atom [] reason) = 
        pp ai atom <+> text "cannot be fulfilled"
        <+> text "because" <+> text reason
    pp ai (Implies atom atoms reason) = 
        pp ai atom <+> text "implies" <+> listSep empty (text "or") (map (pp ai) atoms)
        <+> text "because" <+> text reason
    pp ai (Not atom reason) = 
        text "not" <+> pp ai atom
        <+> text "because" <+> text reason

instance PP Atom where pp _ a = text (show a)

instance PP (Index a) where pp ai@(_,m,_) (Index i) = pp ai (m ! i)

listSep :: Doc -> Doc -> [Doc] -> Doc
listSep sep1 sep2 = go
  where go [] = empty
        go [d] = d
        go [d1,d2] = d1 <+> sep2 <+> d2
        go (d:ds) = d <+> sep1 <+> go ds
