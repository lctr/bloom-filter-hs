module WordTest where

import qualified BloomFilter.Easy      as B
import           Control.DeepSeq       (NFData (..))
import           Control.Monad         (forM_, mapM_)
import qualified Data.ByteString.Char8 as BS
import           Data.Time.Clock       (diffUTCTime, getCurrentTime)
import           System.Environment    (getArgs)
import           System.Exit           (exitFailure)

timed :: NFData a => String -> IO a -> IO a
timed desc act = do
    start <- getCurrentTime
    ret <- act
    end <- rnf ret `seq` getCurrentTime
    putStrLn $ show (diffUTCTime end start) ++ " to " ++ desc
    return ret

instance NFData (B.Bloom a) where
    rnf fltr = B.length fltr `seq` ()

-- | Creates a Bloom filter from the contents of a file, treating each
-- line as an element to add to the filter
main = do
    args <- getArgs
    let files | null args = ["/usr/share/dict/words"]
              | otherwise = args
    forM_ files $ \file -> do
        words <- timed "read words" $
          BS.lines <$> BS.readFile file
        let len = length words
            errRate = 0.01

        putStrLn $ show len ++ " words"
        putStrLn $ "suggested sizings: " ++
          show (B.suggestSizing (fromIntegral len) errRate)

        fltr <- timed "construct filter" $ either
            ((putStrLn . ("Error " ++)) >> const exitFailure)
            return
            (B.easyList errRate words)

        timed "query every element" $ mapM_ print $ filter (not . (`B.elem` fltr)) words
