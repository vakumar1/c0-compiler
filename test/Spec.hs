module Main where

import Test.Hspec

import Common.GraphsTest
import Common.LivenessTest

main :: IO ()
main = do
    graphs_test;
    liveness_test
