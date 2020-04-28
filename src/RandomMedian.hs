module RandomMedian
    ( randomList
    , sample
    , median
    ) where

import System.Random (getStdGen, randomRs, Random)
import Data.List (sort)
import Control.Applicative
import Control.Monad.Trans
import Control.Monad.Trans.Maybe

randomList :: Random a => Int -> (a, a) -> IO [a]
randomList n (lo, hi) = do
    g <- getStdGen
    return $ take n $ randomRs (lo, hi) g

sample :: Integer -> [a] -> IO [a]
sample n xs = do
    g <- getStdGen
    return $ map (\x->xs !! x) $ take (fromInteger n) $ randomRs (0, (length xs)-1) g

randomMedian :: (Ord a) => [a] -> MaybeT IO a
randomMedian xs = do
    let n = length xs
    let n1 = (fromIntegral n :: Double) ** 0.75
    let sqrtn = (fromIntegral n :: Double) ** 0.5
    r <- lift $ flip sample xs $ ceiling n1
    let sortedR = sort r
    let d = sortedR !! ((floor $ 0.5*n1-sqrtn) :: Int)
    let u = sortedR !! ((ceiling $ 0.5*n1+sqrtn) :: Int)
    let c = filter (\x -> x >= d && x <= u) xs
    let ld = length $ filter (<d) xs
    let lu = length $ filter (>u) xs
    if ld > n `div` 2 || lu > n `div` 2
    then empty
    else 
        if ((fromIntegral $ length c) :: Double) > 4*n1 
        then empty
        else 
            let sortedC = sort c
            in
            lift $ return $ sortedC !! (n `div` 2 - ld + 1)
    
median :: (Ord a) => [a] -> MaybeT IO a
median xs = 
    let n = length xs
    in
        if n == 0 
        then empty
        else if n <= 100
             then lift $ return $ (sort xs) !! (n `div` 2)
             else randomMedian xs
