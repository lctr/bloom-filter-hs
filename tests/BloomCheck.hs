module BloomCheck where

import qualified BloomFilter.Easy     as B
import           BloomFilter.Hash     (Hashable)
import qualified Data.ByteString      as S
import qualified Data.ByteString.Lazy as L
import           Data.Word            (Word32, Word8)
-- import           System.Random        (Random (..), RandomGen)
import           Test.QuickCheck

handyCheck :: Testable a => Int -> a -> IO ()
handyCheck limit = quickCheckWith (stdArgs { maxSuccess = limit })

-- | A note regarding generating arbitrary false positives: we need to
-- ensure that zero and one are excluded from the false positives we
-- test with! QuickCheck lets us ensure this either by construction or
-- by elimination:
--
--  * by /construction/: we specify the range of valid values to
--    generate. QuickCheck provides the combinator `forAll` for this
--    exact purpose.
--  * by /elmination/: when QuickCheck generates an arbitrary value,
--    we filter out those that don't fit the criteria using the
--    `(==>)` operator; rejecting a value this way will cause a test
--    to appear to /succeed/.
--
-- It is preferable to take the constructive approach, so that is the
-- path that we will take. A great example of a scenario supporting
-- the constructive approach is as follows: consider the case in which
-- QuickCheck generates 1000 arbitrary values, and 800 of those are
-- filtered out for whatever reason we've deemed necessary. Then we
-- will /appear/ to have run /800/ tests, when in reality only /200/
-- of those will have actually done anything useful!
--
-- Thus, when we generate our desired false positive rates, instead of
-- eliminating zeros and ones as invalid test values, we will instead
-- construct values in an interval that will /always/ be valid.
falsePositive :: Gen Double
falsePositive = choose (epsilon, 1 - epsilon) where epsilon = 1e-6

-- | Filters out (generated 'easyList') failures, automatically
-- passing a test if the generated arbitrary value fails.
(=~>) :: Either a b -> (b -> Bool) -> Bool
k =~> f = either (const True) f k

-- | A synonym for the BloomFilter membership predicate `B.elem`.
(?>) :: a -> B.Bloom a -> Bool
(?>) = B.elem

-- | Tests a property against singleton Bloom filters. The first
-- argument is ignored and only exists to simulate monomorphic
-- properties, as required by QuickCheck. Do note that the type of the
-- first argument will be the same as the type for the first element
-- of the second argument.
prop_one_present :: Hashable a => p -> a -> Property
prop_one_present _ elt = forAll falsePositive $ \errRate ->
    B.easyList errRate [elt] =~> (elt ?>)

-- | Testing a property against a Bloom filter populated with multiple
-- elements; if a Bloom filter is populated with many elements, they
-- should all be present afterwards!
prop_all_present :: Hashable a => p -> [a] -> Property
prop_all_present _ xs = forAll falsePositive $ \errRate -> B.easyList errRate xs =~> \fltr -> all (?> fltr) xs

-- | Since QuickCheck does not provide instances for 'ByteString's, we
-- manually define them; note that, instead of directly creating a
-- 'ByteString', we use the relevant 'pack' function to create one
-- from a 'Word8' byte.
instance Arbitrary L.ByteString where
    arbitrary = L.pack <$> arbitrary
    -- coarbitrary = coarbitrary . L.unpack

instance Arbitrary S.ByteString where
    arbitrary = S.pack <$> arbitrary



-- | Indirectly tests whether 'easyList' behaves well on larger inputs
-- by ignoring complexity and instead checking whether 'suggestSizing'
-- provides a sensible array size and number of inputs even with
-- extreme inputs.
--
-- This check relies on 'suggestSizing' since it is not practical to
-- test that 'easyList' behaves well on larger inputs, as the cost of
-- testing properties of 'easyList' increases rapidly as we increase
-- the number of tests to run.
--
-- This test is not particularly useful, as the counterexamples
-- generated by QuickCheck would, when plugged into 'suggestSizings',
-- wouresult in a bit array that would be too large.
prop_suggest_try1 :: Property
prop_suggest_try1 =
    forAll falsePositive $ \errRate ->
        forAll (choose (1, maxBound :: Word32)) $ \cap ->
            either (const False) sane $ B.suggestSizing (fromIntegral cap) errRate
    where sane (bits, hashes) = bits > 0 && bits < maxBound && hashes > 0

-- | A second attempt at testing whether 'easyList' behaves well on
-- larger inputs, achieved by eliminating sizes and positive rates
-- before they become problematic.
--
-- This test appears to perform well for a small number of tests, but
-- leads to too many combinations being filtered out on a larger body
-- of texts.
prop_suggest_try2 :: Property
prop_suggest_try2 =
    forAll falsePositive $ \errRate ->
        forAll (choose (1, fromIntegral maxWord32)) $ \cap ->
            let bestSize = fst . minimum $ B.sizings cap errRate
            in bestSize < fromIntegral maxWord32 ==> either (const False) sane $ B.suggestSizing cap errRate
    where maxWord32 = maxBound :: Word32
          sane (bits, hashes) = bits > 0 && bits < maxBound && hashes > 0

-- | A third attempt at testing whether 'easyList' behaves well on
-- larger inputs, achieved by reducing the likelihood of generating
-- inputs that we will subsequently reject. This property successfully
-- encapsulates what 'prop_suggest_try1' and 'prop_suggest_try2' aim
-- to achieve.
prop_suggestions_sane :: Property
prop_suggestions_sane =
    forAll falsePositive $ \errRate ->
        forAll (choose (1, fromIntegral maxWord32 `div` 8)) $ \cap ->
            let size = fst . minimum $ B.sizings cap errRate
            in size < fromIntegral maxWord32 ==>
                either (const False) sane $ B.suggestSizing cap errRate
    where maxWord32 = maxBound :: Word32
          sane (bits, hashes) = bits > 0 && bits < maxBound && hashes > 0