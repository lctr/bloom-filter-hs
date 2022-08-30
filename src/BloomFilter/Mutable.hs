-- | This module contains functions used to manipulate and query a
-- mutable Bloom filter.
--
-- Note that a /mutable/ Bloom filter does not have an exposed public
-- API, but is rather used as a way to circumvent requiring multiple
-- passes for the creation of a Bloom filter. Thus it follows that
-- /none/ of the functions in this module be directly exported or
-- exposed to clients!
module BloomFilter.Mutable where

import           Control.Monad.ST     (ST)
import           Data.Array.MArray    (getBounds, newArray, readArray,
                                       writeArray)
import           Data.Word            (Word32)
import           Prelude              hiding (elem, length, notElem)

import           BloomFilter.Internal (MutBloom (..))

-- | Given a hash function that returns a list of 32-bit values and a
-- 32-bit capacity for a bit array, returns a state transformer monad
-- wrapping a new mutable Bloom filter, to be constructed without
-- copying any of the values with which it is to be populated.
--
-- Note that the inner mutable filter should /not/ be exposed in the
-- public API.
new :: (a -> [Word32]) -> Word32 -> ST s (MutBloom s a)
new hash numBits = MB hash <$> newArray (0, numBits - 1) False

-- | Computes the length of the Bloom fiilter's bit array. This
-- function is complicated by two factors:
--
-- 1. we are relying on our bit array's record of its own bounds, and
--    an 'MArray' instance's 'getBounds' function has a monadic type
-- 2. We also have to add one to the result, as the upper bound of
--    the array is one less than the actual length
length :: MutBloom s a -> ST s Word32
length fltr = succ . snd <$> getBounds (mutArray fltr)

-- | Inserts an element into the Bloom filter. When an element is
-- inserted, the values returned from applying each of the filter's
-- hash functions to the item are treated as indices, the array bit
-- values of which are then set to '1'.
insert :: MutBloom s a -> a -> ST s ()
insert fltr elt =
    indices fltr elt
        >>= mapM_ (\bit -> writeArray (mutArray fltr) bit True)

-- | Computes the offsets into the bit array of a given /mutable/
-- Bloom filter, ensuring that all of the hashes stay within the
-- bounds of the array.
--
-- This function should not be exposed in the public API.
indices :: MutBloom s a -> a -> ST s [Word32]
indices fltr elt = do
    modulus <- length fltr
    return $ map (`mod` modulus) (mutHash fltr elt)

-- | Positive membership: for a given value, the array bit index
-- corresponding to the value resulting from
-- applying each hash function will be checked in the bit array. If
-- /any/ of the bits in the array found in these locations is '0',
-- then this function returns @False@ lifted into the state
-- transformer wrapping a mutable Bloom filter; otherwise, a lifted
-- @True@ value is returned.
--
-- Note that not only may this function provide /false positive/s, it
-- is also a private internal function and should not be exposed
-- directly in the public API.
elem :: a -> MutBloom s a -> ST s Bool
elem elt fltr = indices fltr elt >>= allM (readArray (mutArray fltr))

-- | Negative membership: equivalent to the negation of 'elem'.
notElem :: a -> MutBloom s a -> ST s Bool
notElem elt fltr = not <$> elem elt fltr

-- | A helper function in the form of a monadic version of 'all', used
-- when querying a mutable Bloom filter (wrapped in a state
-- transformer) for membership.
allM :: Monad m => (a -> m Bool) -> [a] -> m Bool
allM _ [] = return True
allM p (x : xs) = do
    ok <- p x
    if ok
        then allM p xs
        else return False
