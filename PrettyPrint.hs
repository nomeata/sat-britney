module PrettyPrint where

import Text.PrettyPrint

import Types
import LitSat

class PP a where pp :: a -> Doc

instance PP a => PP (Clause a) where
    pp (OneOf [atom] reason) = 
        text "definitely" <+> pp atom <+> text "because" <+> text reason
    pp (OneOf atoms reason) = 
        text "one of" <+> listSep empty (text "or") (map pp atoms)
        <+> text "because" <+> text reason
    pp (AtMostOne atoms reason) = 
        text "at most one of" <+> listSep empty (text "or") (map pp atoms)
        <+> text "because" <+> text reason
    pp (Implies atom [] reason) = 
        pp atom <+> text "cannot be fulfilled"
        <+> text "because" <+> text reason
    pp (Implies atom atoms reason) = 
        pp atom <+> text "implies" <+> listSep empty (text "or") (map pp atoms)
        <+> text "because" <+> text reason
    pp (Not atom reason) = 
        text "not" <+> pp atom
        <+> text "because" <+> text reason

instance PP Atom where pp a = text (show a)

instance PP (Index a) where pp a = text (show a)

listSep :: Doc -> Doc -> [Doc] -> Doc
listSep sep1 sep2 = go
  where go [] = empty
        go [d] = d
        go [d1,d2] = d1 <+> sep2 <+> d2
        go (d:ds) = d <+> sep1 <+> go ds
