
import Control.Monad.ST
import Control.Applicative
import Control.Monad
import qualified Data.Vector as IV
import qualified Data.Vector.Mutable as MV
import qualified Data.Vector.Generic as V
import qualified Data.Vector.Algorithms.Intro as VA
import System.IO
import System.Environment

discountedCumulativeGain :: IV.Vector Int -> Double
discountedCumulativeGain = V.sum . V.imap gain
  where gain idx score = (2 ^ score  - 1) / logBase 2 (fromIntegral (idx + 2))

ndcg :: IV.Vector Int -> Double
ndcg scores = discountedCumulativeGain scores / discountedCumulativeGain sorted
  where
    sorted = runST $ do
      v <- V.thaw scores
      VA.sortBy (flip compare) v
      V.freeze v

prels :: IV.Vector Int -> IV.Vector Double
prels = V.map toPRel
  where toPRel 4 = 0.4
        toPRel 3 = 0.1
        toPRel 2 = 0.1
        toPRel _ = 0

-- look[0] = 1
-- look[i] = look[i-1] * (1 - prel(i-1)) * (1 - pbreak)

pbreak = 0.15

plooks :: IV.Vector Double -> IV.Vector Double
plooks prels = V.fromList $ list
  where
    list = 1 : zipWith go list (V.toList prels)
    go prevLook prevPrel = prevLook * (1 - prevPrel) * (1 - pbreak)

pfound :: IV.Vector Int -> Double
pfound scores = V.sum (V.zipWith (*) ls rels)
  where
    rels = prels scores
    ls = plooks rels

main = do
  args <- getArgs
  if length args /= 1 then do
    putStrLn "Usage: Metrics METRIC"
    putStrLn "\t where METRIC can be: dcg, ndcg, pfound"
  else do
    let metric = case head args of
          "dcg" -> discountedCumulativeGain
          "ndcg" -> ndcg
          "pfound" -> pfound
    getLine
    scores <- (V.fromList . map read . words) <$> getLine
    let dcg = metric scores
    print dcg
