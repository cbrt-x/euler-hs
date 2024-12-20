{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TypeApplications #-}

module Main (main) where

import Input

import Data.List
import Data.Maybe

main :: IO ()
main = print p10

{-
========================================================
Helper functions
========================================================
-}

divides :: Integral a => a -> a -> Bool
n `divides` m = m `mod` n == 0

primeFactors :: Integral a => a -> [a]
primeFactors n
  | x:_ <- factors n = primeFactors x ++ primeFactors (n `div` x)
  | otherwise = [n]

factors :: Integral a => a -> [a]
factors n = [ x | x <- [2..n-1], x `divides` n]

square :: Num a => a -> a
square x = x^(2 :: Int)

window :: Int -> [a] -> [[a]]
window n = go
  where
    go xs@(_:xs') | length xs >= n = take n xs : go xs'
    go _ = []

primes :: Integral a => [a]
primes = filterPrimes [2..]
  where filterPrimes (x:xs) = x : filterPrimes [ y | y <- xs, not (x `divides` y) ]
        filterPrimes _ = error "impossible"

intSqrt :: Integral a => a -> Maybe a
intSqrt x
    | x == square sqrt_x = Just sqrt_x
    | otherwise          = Nothing
  where
    sqrt_x = floor $ sqrt @Double (fromIntegral x)


{-
========================================================
Solutions
========================================================
-}

-- Find the sum of all the multiples of 3 or 5 below 1000.
p1 :: Int
p1 = sum [ n | n <- [1..1000], 3 `divides` n, 5 `divides` n ]

-- By considering the terms in the Fibonacci sequence whose values do not exceed four million,
-- find the sum of the even-valued terms.
p2 :: Int
p2 = sum (filter even $ takeWhile (< limit) fibs)
  where 
    fibs = 0 : 1 : zipWith (+) fibs (drop 1 fibs)
    limit = 4_000_000

-- What is the largest prime factor of the number 600851475143?
p3 :: Int
p3 = maximum $ primeFactors 600851475143

-- Find the largest palindrome made from the product of two 3-digit numbers.
p4 :: Int
p4 = maximum [ n | x <- threeDigitN, y <- threeDigitN, let n = x * y, palindrome (show n) ]
  where
    threeDigitN = [ 100 .. 999 ]
    palindrome str = str == reverse str

-- What is the smallest positive number that is evenly divisible by all of the numbers from 1 to 20?
p5 :: Int
p5 = uniqueProduct (map primeFactors [1..20])
  where
    uniqueProduct = \case
      []   -> 1
      x:xs -> product x * uniqueProduct (map (\\ x) xs)

-- Find the difference between the sum of the squares of the first one hundred natural numbers and the square of the sum.
p6 :: Int
p6 = square (sum nums) - sum (map square nums)
  where
    nums = [1..100]

-- What is the 10001st prime number?
p7 :: Int
p7 = primes !! (10001 - 1)

p8 :: Int
p8 = maximum $ map (product . readDigits) $ window 13 $(withInput 8)
  where
    readDigits = map (\x -> read [x])

-- There exists exactly one Pythagorean triplet for which a+b+c = 1000.
-- Find the product abc.
p9 :: Int
p9 | (x:_) <- triples = x
   | otherwise = error "impossible"
  where 
    nums = [1..1000]
    triples = [ a*b*c | a <- nums, b <- nums, let c_sq = square a + square b
              , c <- maybeToList (intSqrt c_sq)
              , a+b+c == 1000 ]

-- Find the sum of all the primes below two million.
p10 :: Int
p10 = sum $ takeWhile (< 2_000_000) primes
