{-# LANGUAGE ForeignFunctionInterface #-}

module BloomFilter.Hash (
    Hashable(..)
    , hash
    , doubleHash
) where

import           Control.Monad         (foldM)
import           Data.Bits             (shiftR, (.&.))
import qualified Data.ByteString       as Strict
import qualified Data.ByteString.Lazy  as Lazy
import           Data.Word             (Word32, Word64)
import           Foreign.C.Types       (CSize (..))
import           Foreign.Marshal.Array (withArrayLen)
import           Foreign.Marshal.Utils (with)
import           Foreign.Ptr           (Ptr, castPtr, plusPtr)
import           Foreign.Storable      (Storable, peek, sizeOf)
import           System.IO.Unsafe      (unsafePerformIO)

-- | Foreign call to 'C' function hashing values with 32-bit aligned
-- addresses. This function is /faster/ than @hashLittle2@.
foreign import ccall unsafe "lookup3.h hashword2" hashWord2
    :: Ptr Word32
    -> CSize
    -> Ptr Word32
    -> Ptr Word32
    -> IO ()

-- | Foreign call to 'C' function that hashes values of
-- arbitrarily-sized address alignment. Since it has no restrictions
-- on the address alignment of a value, it is slower than the function
-- @hashWord2@.
foreign import ccall unsafe "lookup3.h hashlittle2" hashLittle2
    :: (Ptr a)
    -> CSize
    -> Ptr Word32
    -> Ptr Word32
    -> IO ()

-- | Combines the 32-bit salts consume and the computed hash values
-- into a single 64-bit value.
hashIO :: (Ptr a)      -- ^ value to hash
  -> CSize             -- ^ number of bytes
  -> Word64            -- ^ salt
  -> IO Word64
hashIO ptr bytes salt =
    -- allocates space for the salt on C's stack;
    -- note that 'sp' is a Ptr Word64
    with (fromIntegral salt) $ \sp -> do
        -- Split the single 'Word64' into two 'Ptr Word32's to which C
        -- will write the computed hashed values points at the low
        -- word of 'sp'
        let p1 = castPtr sp
        -- points to the high word of 'sp'
            p2 = castPtr sp `plusPtr` 4
        -- pass to the C hashing functions; because all data pointers
        -- come from the Haskell heap, we know that they will be
        -- aligned on an address that is safe to pass to either of
        -- 'hashWord2' (which only accepts 32-bit aligned addresses)
        -- or 'hashLittle2'
        go p1 p2
        -- retrieve the computed hash
        peek sp
  where
    parts = bytes `div` 4
    go p1 p2
     -- 'hashWord2' is faster, so we call it if we know if the size of
     -- our data is a multiple of 4
     | bytes .&. 3 == 0 = hashWord2 (castPtr ptr) parts p1 p2
     | otherwise = hashLittle2 ptr bytes p1 p2

-- | High-level interface allowing the client to bypass fiddlng with
-- low-level details when hashing values.
--
-- The single method in this class takes a 64-bit /salt/ and a value,
-- and returns a 64-bit value corresponding to the combination of the
-- hashed 32-bit salt values and computed hashes.
class Hashable a where
    hashSalt :: Word64  -- ^ salt
             -> a       -- ^ value to hash
             -> Word64

hash :: Hashable a => a -> Word64
-- hash = hashSalt 0x106fc397cf62f64d3 -- overflows
hash = hashSalt 503340467227682003

-- | Helper (boilerplate) function used in 'Hashable' instances for
-- basic types.
hashStorable :: Storable a => Word64 -> a -> Word64
hashStorable salt k = unsafePerformIO . with k $ \ptr -> hashIO ptr (fromIntegral (sizeOf k)) salt

instance Hashable Char where hashSalt = hashStorable
instance Hashable Int where hashSalt = hashStorable
instance Hashable Double where hashSalt = hashStorable

----------------------------------------------------------------
-- HASHING LISTS -----------------------------------------------
----------------------------------------------------------------

hashList :: Storable a => Word64 -> [a] -> IO Word64
hashList salt xs =
    withArrayLen xs $ \len ptr ->
        hashIO ptr (fromIntegral (len * sizeOf x)) salt
  where x = head xs

instance Storable a => Hashable [a] where
    hashSalt salt xs = unsafePerformIO $ hashList salt xs

----------------------------------------------------------------
-- HASHING TUPLE TYPES -----------------------------------------
----------------------------------------------------------------

-- | To hash tuple types, we use function composition, taking a salt
-- in at one end of of the composition pipeline and use the result of
-- hashing each tuple element as the salt for the /next/ element.
--
-- The 'hash2' function encapsulates each instance of hashing a
-- tuple's element and is used with each element when hashing a tuple.
-- Note that 'hash2' is essentially 'flip Hashable.hashSalt'!
hash2 :: Hashable a => a -> Word64 -> Word64
hash2 k salt = hashSalt salt k

instance (Hashable a, Hashable b) => Hashable (a, b) where
    hashSalt salt (a, b) = hash2 b . hash2 a $ salt

instance (Hashable a, Hashable b, Hashable c) => Hashable (a, b, c) where
    hashSalt salt (a, b, c) = hash2 c . hash2 b . hash2 a $ salt

instance (Hashable a
    , Hashable b
    , Hashable c
    , Hashable d
    ) => Hashable (a, b, c, d) where
    hashSalt salt (a, b, c, d) = hash2 d
        . hash2 c
        . hash2 b
        . hash2 a $ salt

instance (Hashable a
    , Hashable b
    , Hashable c
    , Hashable d
    , Hashable e
    ) => Hashable (a, b, c, d, e) where
    hashSalt salt (a, b, c, d, e) = hash2 e
        . hash2 d
        . hash2 c
        . hash2 b
        . hash2 a $ salt

instance (Hashable a
  , Hashable b
  , Hashable c
  , Hashable d
  , Hashable e
  , Hashable f
  ) => Hashable (a, b, c, d, e, f) where
    hashSalt salt (a, b, c, d, e, f) = hash2 f
        . hash2 e
        . hash2 d
        . hash2 c
        . hash2 b
        . hash2 a $ salt

instance (Hashable a
    , Hashable b
    , Hashable c
    , Hashable d
    , Hashable e
    , Hashable f
    , Hashable g
    ) => Hashable (a, b, c, d, e, f, g) where
    hashSalt salt (a, b, c, d, e, f, g) = hash2 g
        . hash2 f
        . hash2 e
        . hash2 d
        . hash2 c
        . hash2 b
        . hash2 a $ salt

instance (Hashable a
    , Hashable b
    , Hashable c
    , Hashable d
    , Hashable e
    , Hashable f
    , Hashable g
    , Hashable h
    ) => Hashable (a, b, c, d, e, f, g, h) where
    hashSalt salt (a, b, c, d, e, f, g, h) = hash2 h
        . hash2 g
        . hash2 f
        . hash2 e
        . hash2 d
        . hash2 c
        . hash2 b
        . hash2 a $ salt

instance (Hashable a
    , Hashable b
    , Hashable c
    , Hashable d
    , Hashable e
    , Hashable f
    , Hashable g
    , Hashable h
    , Hashable i
    ) => Hashable (a, b, c, d, e, f, g, h, i) where
    hashSalt salt (a, b, c, d, e, f, g, h, i) = hash2 i
        . hash2 h
        . hash2 g
        . hash2 f
        . hash2 e
        . hash2 d
        . hash2 c
        . hash2 b
        . hash2 a $ salt

----------------------------------------------------------------
-- HASHING BYTESTRINGS -----------------------------------------
----------------------------------------------------------------

-- | 'Hashable' instances for 'ByteString's take advantage of the
-- 'ByteString' type internals, which give excellent hashing
-- performance. Note that the /value/ of the 'ByteString' itself is
-- /not stored.
hashByteString :: Word64 -> Strict.ByteString -> IO Word64
hashByteString salt bs =
    Strict.useAsCStringLen bs $ \(ptr, len) ->
        hashIO ptr (fromIntegral len) salt

instance Hashable Strict.ByteString where
    hashSalt salt bs = unsafePerformIO $ hashByteString salt bs

-- | Takes a lazy 'ByteString' and return a list of 'ByteString'
-- chunks, with each chunk uniformly sized at 64KB.
rechunk :: Lazy.ByteString -> [Strict.ByteString]
rechunk s
  | Lazy.null s = []
  | otherwise = let (pre, suf) = Lazy.splitAt chunkSize s
                in repack pre : rechunk suf
    where repack = Strict.concat . Lazy.toChunks
          chunkSize = 64 * 1024

-- | Since lazy 'ByteString's are represented as a series of chunks,
-- we have to take those chunks' boundaries into consideration.
-- Namely, we ensure that we pass chunks that are uniformly 64KB in
-- size to the C hashing function, so that we consistently hash chunks
-- regardless of the original chunk boundaries. This is necessary
-- since, for example, the string "foobar" may be represented as
-- @["fo", "obar"]@, @["foob", "ar"]@, etc.
instance Hashable Lazy.ByteString where
    hashSalt salt bs = unsafePerformIO $ foldM hashByteString salt (rechunk bs)

-- | Combines the two values computed by the Jenkins hash functions to
-- a greater number of more hashes.
--
-- This is helpful since we need more than two hashes (by a
-- potentially large margin) to make effective use of a Bloom filter,
-- but computing many distinct hashes may be computationally
-- expensive.
doubleHash :: Hashable a => Int -> a -> [Word32]
doubleHash numHashes value = [ h1 + h2 * i | i <- [0..num] ]
  where
    h = hashSalt 0x9150a946c4a8966e value
    h1 = fromIntegral (h `shiftR` 32) .&. maxBound
    h2 = fromIntegral h
    num = fromIntegral numHashes
