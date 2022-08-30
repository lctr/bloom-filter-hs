-- | A space-efficient data structure used for testing set membership,
-- suitable for use with large data sets. Bloom filters by default
-- support two operations: /insertion/ and /membership querying/.
-- Elements may be added to a Bloom filter, but not /removed/ (unless
-- the Bloom filter in question is a 'counting' variant).
--
-- Provided in this module are two main ways to construct a Bloom
-- filter: via the 'fromList' function located in the canonical
-- 'BloomFilter' module, which requires all hash functions to be
-- specified along with the capacity of the set (i.e., the number of
-- bits that should live within the filter), and the elements to
-- populate the set, OR via the 'easyList' function located in the
-- 'BloomFilter.Easy' module, which instead takes a /false positive/
-- rate (between '0' and '1', exclusive) and a list of elements with
-- which to populate the set. The size of the bit array for filters
-- constructed with the 'easyList' function is dynamically computed
-- based on the error rate given and the size of the list provided.
--
-- Note that in order for an element of type @T@ to be stored in a
-- Bloom filter, it /must/ be an instance of the class /Hashable/ from
-- the module 'BloomFilter.Hash', which is separately (and publically)
-- available. Various basic types already have existing instances, as
-- do their lists (and 'ByteString's). Elements must be 'Hashable', as
-- the actual machinery with which values are hashed is implemented in
-- 'C', and the class 'Hashable' allows for a high-level interface
-- with which low-level details regarding hashing may be avoided.
--
-- Like a (hash)set, a Bloom filter is an efficient data structure on
-- top of which cache-like functionality is exploited, and in
-- particular is well suited for storing larger amounts of data than a
-- typical set while taking up less space. However, this comes at a
-- cost, as Bloom filters trade in space efficiency for accuracy an
-- will generally have a non-zero (yet generally low) chance of
-- producing /false positives/. In particular, the probability of
-- witnessing a /false positive/ increases monotonically with the
-- number of elements inserted.
--
-- Note that while false positives are possible, false /negatives/ are
-- not. This leads to Bloom filters answering the question "is some
-- element 'x' a member of some set 'X'?" with either "possibly" or
-- "definitely not".
--
-- Bloom filters are fairly simple in their internal structure,
-- comprising a bit array and a (generally relatively small) constant,
-- finite number of hash functions that map some set element to one of
-- the bit array's positions. Elements are added to a Bloom filter by
-- computing a hash value for each associated hash function and using
-- the resulting hashed values as the indices for which the
-- corresponding bits in the bit array are flipped from '0' to '1'.
-- Analogously, querying a Bloom filter is achieved by computing an
-- item's hash with each of the associated hash functions, and testing
-- whether the bits in the bit array whose indices correspond to the
-- computed hash values are set to '1'; if /not/, then the element is
-- known to /definitely/ not be a member of the set.
--
-- Note that upon insertion, while all the bits -- located at the
-- indices computed by each hash function -- are set to '1', it is
-- possible that /some/ or /any/ of those bits were set to '1' during
-- the insertion of /other/ elements, hence resulting in a false
-- positive. Since bits are only turned /on/, no bits in the bit array
-- are ever set to '0', therefore any element for which every bit
-- corresponding to a computed hash value is '0' is known to
-- /definitely/ never have been inserted.
--
-- Original implementation by Bryan O\'Sullivan, this library was
-- adopted from chapter 26 of O\'Sullivan's book 'Real World Haskell'.
--
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

-- | Returns the length of the bit array used by a
-- Bloom to store data.
length :: Bloom a -> Int
length = fromIntegral . len

-- | Returns the number of bits contained within the bit array held by
-- a Bloom filter as a 32-bit value.
len :: Bloom a -> Word32
len = succ . snd . bounds . blmArray

-- | Membership predicate: returns @True@ if a given value /may be/ an
-- element of the set represented by a Bloom filter. Note that this
-- method has the possibility for false positives.
elem :: a -> Bloom a -> Bool
elt `elem` fltr = all test (blmHash fltr elt)
  where
    test hash = blmArray fltr ! (hash `mod` len fltr)

-- | Returns @True@ is a given value is /definitely/ not an element of
-- the set represented by a Bloom filter. Note that, unlike the
-- function 'elem', this function will not return false negatives.
notElem :: a -> Bloom a -> Bool
elt `notElem` fltr = not (elt `elem` fltr)

-- | A synonym for the membership predicate `elem`.
(?>) :: a -> Bloom a -> Bool
(?>) = elem

-- | An ergonomic way to create an /immutable/ Bloom filter from a
-- list of hash functions, a capacity, and a list of items to store.
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
