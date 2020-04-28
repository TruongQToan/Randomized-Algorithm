module Main where

import RandomMedian
import Control.Monad.Trans
import Control.Monad.Trans.Maybe
import Data.List (sort)

main :: IO ()
main = do
    xs <- randomList 1000000 (1::Integer, 10000::Integer)
--     print xs
--     print $ sort xs
--     print $ (sort xs) !! ((length xs) `div` 2)
    s <- runMaybeT $ median xs
    case s of
        Nothing -> print "Fail"
        Just x -> print x
