module AiServing.Cost
  ( tokenCost
  , requestCost
  , cacheSavings
  , monthlyBill
  ) where

-- | Dollar cost of a token count at a per-million-token rate.
tokenCost :: Double -> Double -> Double
tokenCost tokens ratePerMillion = tokens / 1000000.0 * ratePerMillion

-- | Cost of one request when input tokens, output tokens, and a
-- per-request fee all land on the same bill. A cached share of the
-- input traffic bills at the discounted rate, so the input cost
-- shrinks by that share. The cache share clamps to [0, 1].
requestCost :: Double -> Double -> Double -> Double -> Double -> Double -> Double
requestCost inputTokens outputTokens inputRate outputRate cacheShare perRequest =
  inCost + outCost + perRequest
  where
    share   = max 0.0 (min 1.0 cacheShare)
    inCost  = tokenCost inputTokens inputRate * (1.0 - share)
    outCost = tokenCost outputTokens outputRate

-- | Dollars a cached share of input traffic saves at a given volume,
-- floored at zero when the share is nonsense.
cacheSavings :: Double -> Double -> Double -> Double
cacheSavings tokens inputRate cacheShare =
  tokenCost tokens inputRate * max 0.0 (min 1.0 cacheShare)

-- | Monthly bill for a workload: per-request cost times requests per
-- day, over a 30-day month.
monthlyBill :: Double -> Double -> Double
monthlyBill costPerRequest requestsPerDay =
  costPerRequest * requestsPerDay * 30.0
