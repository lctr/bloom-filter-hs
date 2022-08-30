module BloomFilter (
  Bloom
  , length
  , elem
  , notElem
  , (?>)
  , fromList
) where

import           BloomFilter.Internal
import           BloomFilter.Mutable  (insert, new)
import           Data.Array.IArray    (bounds, (!))
import           Data.Array.ST        (runSTUArray)
import           Data.Word            (Word32)
import           Prelude              hiding (elem, length, notElem)

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

-- | A synonym for the BloomFilter membership predicate `B.elem`.
(?>) :: a -> Bloom a -> Bool
(?>) = elem

-- | An ergonomic way to create an /immutable/ Bloom filter from a list of hash
-- functions, a capacity, and a list of items to store.
fromList
  :: (a -> [Word32]) -- ^ family of hash functions to use
  -> Word32          -- ^ number of bits in the filter
  -> [a]             -- ^ values with which to populate the filter
  -> Bloom a
fromList hash numBits values =
  B hash $ runSTUArray $
    do mb <- new hash numBits
       mapM_ (insert mb) values
       return (mutArray mb)
