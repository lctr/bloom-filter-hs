module BloomFilter where

import BloomFilter.Internal
import BloomFilter.Mutable (insert, new)
import Data.Array.IArray (bounds, (!))
import Data.Array.ST (runSTUArray)
import Data.Word (Word32)
import Prelude hiding (elem, length, notElem)

length :: Bloom a -> Int
length = fromIntegral . len

len :: Bloom a -> Word32
len = succ . snd . bounds . blmArray

elem :: a -> Bloom a -> Bool
elt `elem` fltr = all test (blmHash fltr elt)
  where
    test hash = blmArray fltr ! (hash `mod` len fltr)

notElem :: a -> Bloom a -> Bool
elt `notElem` fltr = not (elt `elem` fltr)

