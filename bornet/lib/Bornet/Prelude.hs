module Bornet.Prelude
  ( xlookup,
    xlookupS,
  )
where

import Data.Map.Strict qualified as Map
import GHC.Stack (HasCallStack)

xlookup :: (HasCallStack, Show k, Ord k) => k -> Map.Map k a -> a
xlookup k aMap = case Map.lookup k aMap of
  Just v -> v
  Nothing -> error $ "xlookup: key not found: " ++ show k

xlookupS :: (HasCallStack, Show k, Ord k) => String -> k -> Map.Map k a -> a
xlookupS msg k aMap = case Map.lookup k aMap of
  Just v -> v
  Nothing -> error $ "xlookups: " ++ msg ++ ": key not found: " ++ show k
