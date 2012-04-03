{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module: Hints
-- Copyright: (c) 2011 Joachim Breitner
-- License: GPL-2
--
module Difference where

import qualified Data.Set as S
import qualified Data.Strict as ST
import qualified Data.ByteString.Lazy.Char8 as L
import System.IO
import Control.Monad

import Types
import Arches
import Indices
import AtomIndex
import qualified IndexMap as IxM
import qualified IndexSet as IxS

suiteDifference ai testing unstable newAtomIs =
    let newAtoms = S.map (ai `lookupAtom`) newAtomIs
        (newSource, newBinaries, _) = splitAtoms newAtoms
    in L.unlines $
        [ "Changes of Sources:" ] ++
        difference (setMap (ai `lookupSrc`) $ sources testing) newSource ++
        [ "Changes of Package:" ] ++
        difference (setMap (ai `lookupBin`) $ binaries testing) newBinaries

difference :: (Show a, Ord a) => S.Set a -> S.Set a -> [L.ByteString]
difference old new =
    {-
    putStrLn "New state"
    forM_ (S.toList new) $ \x -> putStrLn $ "    " ++ show x
    -}
    let added = new `S.difference` old
        removed = old `S.difference` new
    in  [ "Newly added:" ] ++
        [ "   " `L.append` L.pack (show x) | x <- S.toList added ] ++
        [ "Removed:" ] ++
        [ "   " `L.append` L.pack (show x) | x <- S.toList removed ]

setMap f = S.fromList . map f . IxS.toList


