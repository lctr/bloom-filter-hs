module BloomFilter.Easy (
  suggestSizing
  , sizings
  , easyList

  -- re-exporting useful names from BloomFilter
  , B.Bloom
  , B.length
  , B.elem
  , B.notElem
  , (?>)

) where

import           BloomFilter      ((?>))
import qualified BloomFilter      as B
import           BloomFilter.Hash (Hashable, doubleHash)
import           Data.List        (genericLength)
import           Data.Maybe       (mapMaybe)
import           Data.Word        (Word32)

-- | An "easier" way to create a Bloom filter that does not require specifying
-- the hashing functions to be used, instead relying on the 'Hashable'
-- typeclass. Additionally, a 'false positivity rate' parameter is used (along
-- with the list of elements to insert) to calculate the necessary size of the
-- filter.
--
-- Note that this function will fail if the length of the input list is /too
-- long/.
easyList
  :: (Hashable a)
  => Double -- ^ false positive rate, between 0 and 1
  -> [a] -- ^ list of values with which to populate the filter
  -> Either String (B.Bloom a)
easyList errRate values =
  case suggestSizing (genericLength values) errRate of
    Left err -> Left err
    Right (bits, hashes) -> Right filt
     where filt = B.fromList (doubleHash hashes) bits values

suggestSizing
  :: Integer                        -- ^ expected max capacity
  -> Double                         -- ^ desired false positive rate
  -> Either String (Word32, Int)    -- ^ (filter size, number of hashes)
suggestSizing cap errRate
  | cap <= 0 = Left "capacity too small"
  | errRate <= 0 || errRate >= 1 = Left "invalid error rate"
  | null saneSizes = Left "capacity too large"
  | otherwise = Right (minimum saneSizes)
  where
    saneSizes = mapMaybe sanitize $ sizings cap errRate
    sanitize (bits, hashes)
      | bits > maxWord32 - 1 = Nothing
      | otherwise = Just (ceiling bits, truncate hashes)
      where maxWord32 = fromIntegral (maxBound :: Word32)

-- | Suggests pairs of array sizes and hash count without validating
-- suggestions. Note that array sizes must be filtered out since 32-bit hashes
-- are used.
sizings :: Integer -> Double -> [(Double, Double)]
sizings cap errRate =
  [ ((-k) * cap' / log (1 - (errRate ** (1 / k))), k) | k <- [1..50] ]
  where cap' = fromIntegral cap
