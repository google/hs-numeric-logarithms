-- Copyright 2021 Google LLC
--
-- Licensed under the Apache License, Version 2.0 (the "License");
-- you may not use this file except in compliance with the License.
-- You may obtain a copy of the License at
--
--      http://www.apache.org/licenses/LICENSE-2.0
--
-- Unless required by applicable law or agreed to in writing, software
-- distributed under the License is distributed on an "AS IS" BASIS,
-- WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
-- See the License for the specific language governing permissions and
-- limitations under the License.

{-# LANGUAGE CPP #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Main(main) where

import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck
  ( (===), (.&&.), Arbitrary(..), Property
  , forAll, suchThat
#if MIN_VERSION_QuickCheck(2, 10, 0)
  , withMaxSuccess
#else
  , Testable, property
#endif
  )

import Test.Framework(defaultMain)

import Numeric.Logarithms
  ( log2Floor, log2Ceiling, log2Approx
  , ilog2Floor, ilog2Ceiling
  )

exp2 :: Int -> Rational
exp2 x | x < 0 = recip (exp2 (-x))
exp2 x = 2 ^ x

#if !MIN_VERSION_QuickCheck(2, 10, 0)
withMaxSuccess :: Testable prop => Int -> prop -> Property
withMaxSuccess _n = property
#endif

prop_log2FloorCorrect :: forall a. (Arbitrary a, Real a, Show a) => Property
prop_log2FloorCorrect =
  withMaxSuccess 10000 $
  forAll (arbitrary @a `suchThat` (>0)) $ \x ->
  let xr = toRational x
      lg = log2Floor x
      lo = exp2 lg
      hi = exp2 (lg+1)
  in  (lo <= xr) === True .&&. (xr < hi) === True

prop_log2CeilingCorrect :: forall a. (Arbitrary a, Real a, Show a) => Property
prop_log2CeilingCorrect =
  withMaxSuccess 10000 $
  forAll (arbitrary @a `suchThat` (>0)) $ \x ->
  let xr = toRational x
      lg = log2Ceiling x
      lo = exp2 (lg-1)
      hi = exp2 lg
  in  (lo < xr) === True .&&. (xr <= hi) === True

prop_log2ApproxCorrect :: forall a. (Arbitrary a, Real a, Show a) => Property
prop_log2ApproxCorrect =
  withMaxSuccess 10000 $
  forAll (arbitrary @a `suchThat` (>0)) $ \x ->
  let xr = toRational x
      (lg, o) = log2Approx x
      lt = exp2 (lg-1)
      eq = exp2 lg
      gt = exp2 (lg+1)
  in  case o of
        LT -> (lt < xr) === True .&&. (xr < eq) === True
        EQ -> xr === eq
        GT -> (eq < xr) === True .&&. (xr < gt) === True

prop_ilog2FloorAgrees :: forall a. (Arbitrary a, Integral a, Show a) => Property
prop_ilog2FloorAgrees =
  withMaxSuccess 10000 $
  forAll (arbitrary @a `suchThat` (>0)) $ \x ->
  ilog2Floor x === log2Floor x

prop_ilog2CeilingAgrees :: forall a. (Arbitrary a, Integral a, Show a) => Property
prop_ilog2CeilingAgrees =
  withMaxSuccess 10000 $
  forAll (arbitrary @a `suchThat` (>0)) $ \x ->
  ilog2Ceiling x === log2Ceiling x

main :: IO ()
main = defaultMain
  [ testProperty "log2Floor @Int"        (prop_log2FloorCorrect @Int)
  , testProperty "log2Floor @Integer"    (prop_log2FloorCorrect @Integer)
  , testProperty "log2Floor @Double"     (prop_log2FloorCorrect @Double)
  , testProperty "log2Floor @Rational"   (prop_log2FloorCorrect @Rational)

  , testProperty "log2Ceiling @Int"      (prop_log2CeilingCorrect @Int)
  , testProperty "log2Ceiling @Integer"  (prop_log2CeilingCorrect @Integer)
  , testProperty "log2Ceiling @Double"   (prop_log2CeilingCorrect @Double)
  , testProperty "log2Ceiling @Rational" (prop_log2CeilingCorrect @Rational)

  , testProperty "log2Approx @Int"       (prop_log2ApproxCorrect @Int)
  , testProperty "log2Approx @Integer"   (prop_log2ApproxCorrect @Integer)
  , testProperty "log2Approx @Double"    (prop_log2ApproxCorrect @Double)
  , testProperty "log2Approx @Rational"  (prop_log2ApproxCorrect @Rational)

  , testProperty "ilog2Floor @Int"       (prop_ilog2FloorAgrees @Int)
  , testProperty "ilog2Floor @Integer"   (prop_ilog2FloorAgrees @Integer)
  , testProperty "ilog2Floor @Word"      (prop_ilog2FloorAgrees @Word)

  , testProperty "ilog2Ceiling @Int"     (prop_ilog2CeilingAgrees @Int)
  , testProperty "ilog2Ceiling @Integer" (prop_ilog2CeilingAgrees @Integer)
  , testProperty "ilog2Ceiling @Word"    (prop_ilog2CeilingAgrees @Word)
  ]
