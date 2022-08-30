module BloomFilter.Mutable where

import Control.Monad (liftM)
import Control.Monad.ST (ST)
import Data.Array.MArray (getBounds, newArray, readArray, writeArray)
import Data.Word (Word32)
import Prelude hiding (elem, length, notElem)

import BloomFilter.Internal (MutBloom (..))

new :: (a -> [Word32]) -> Word32 -> ST s (MutBloom s a)
new hash numBits = MB hash `fmap` newArray (0, numBits - 1) False

-- This function is complicated by two factors:
-- 1.) we are relying on our bit array's record of its own bounds, and an
-- 'MArray' instance's 'getBounds' function has a monadic type
-- 2.) We also have to add one to the result, as the upper bound of the array is
-- one less than the actual length
length :: MutBloom s a -> ST s Word32
length fltr = succ . snd <$> getBounds (mutArray fltr)

-- To add an element to the Bloom filter, we set all of the bits indicated by
-- the hash function.
insert :: MutBloom s a -> a -> ST s ()
insert fltr elt =
    indices fltr elt
        >>= mapM_ (\bit -> writeArray (mutArray fltr) bit True)

-- Compute offsets into the bit array
indices :: MutBloom s a -> a -> ST s [Word32]
indices fltr elt = do
    modulus <- length fltr
    return $ map (`mod` modulus) (mutHash fltr elt)

{- | Membership
 If every bit indicated by the hash function is set, we consider the element
 to be present within the Bloom filter
-}
elem :: a -> MutBloom s a -> ST s Bool
elem elt fltr = indices fltr elt >>= allM (readArray (mutArray fltr))

notElem :: a -> MutBloom s a -> ST s Bool
notElem elt fltr = not <$> elem elt fltr

-- A monadic version of 'all'
allM :: Monad m => (a -> m Bool) -> [a] -> m Bool
allM p (x : xs) = do
    ok <- p x
    if ok
        then allM p xs
        else return False
allM p [] = return True