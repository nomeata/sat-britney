{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Indices where

import Control.DeepSeq

newtype Index a = Index { unIndex :: Int }
 deriving (Eq, Ord, NFData)
instance Show (Index a) where show (Index i ) = "I" ++ show i


