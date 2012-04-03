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

printSuiteDifference h ai testing unstable newAtomIs = do
    let newAtoms = S.map (ai `lookupAtom`) newAtomIs
    let (newSource, newBinaries, _) = splitAtoms newAtoms
    hPutStrLn h "Changes of Sources:"
    printDifference h (setMap (ai `lookupSrc`) $ sources testing) newSource
    hPutStrLn h "Changes of Package:"
    printDifference h (setMap (ai `lookupBin`) $ binaries testing) newBinaries
    hFlush h

printDifference :: (Show a, Ord a) => Handle -> S.Set a -> S.Set a -> IO ()
printDifference h old new = do
    {-
    putStrLn "New state"
    forM_ (S.toList new) $ \x -> putStrLn $ "    " ++ show x
    -}
    let added = new `S.difference` old
    hPutStrLn h "Newly added:"
    forM_ (S.toList added) $ \x -> hPutStrLn h $ "    " ++ show x
    let removed = old `S.difference` new
    hPutStrLn h "Removed:"
    forM_ (S.toList removed) $ \x -> hPutStrLn h $ "    " ++ show x

setMap f = S.fromList . map f . IxS.toList


