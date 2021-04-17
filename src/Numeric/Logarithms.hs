-- Copyright 2021 Google LLC
--
-- Licensed under the Apache License, Version 2.0 (the "License");
-- you may not use this file except in compliance with the License.
-- You may obtain a copy of the License at
--
--     https://www.apache.org/licenses/LICENSE-2.0
--
-- Unless required by applicable law or agreed to in writing, software
-- distributed under the License is distributed on an "AS IS" BASIS,
-- WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
-- See the License for the specific language governing permissions and
-- limitations under the License.

-- | Rounded base-2 logarithms of 'Integral' and 'Real' types.

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MagicHash #-}

module Numeric.Logarithms
         ( log2Approx, log2Floor, log2Ceiling, log2With
         , ilog2Floor, ilog2Ceiling
         ) where

import Data.Bits (bit)
import Data.Ratio (denominator, numerator)
import GHC.Exts (Int(I#))
import GHC.Integer (shiftLInteger)
import GHC.Integer.Logarithms (integerLog2#)
import GHC.Stack (HasCallStack)

integerLog2Floor :: Integer -> Int
integerLog2Floor x = I# (integerLog2# x)

-- Derivation of log2 algorithms for Rational:
--
--   floor(log2(r))
-- = floor(log2(x/y)) with x, y the numerator and denominator of r.
-- = floor(log2((2^n * (1 + k/2^n)) / (2^m * (1 + j/2^m))))
--     with x = 2^n + k, k < 2^n, n an integer
--          y = 2^m + j, j < 2^m, m an integer
-- = floor(log2(2^n/2^m) + log2((1+k/2^n)/(1+j/2^m)))
-- = floor(n - m + log2((1+k/2^n)/(1+j/2^m)))
-- = n - m + floor(log2((1+k/2^n)/(1+j/2^m)))
-- = n - m + if (1+k/2^n) < (1+j/2^m) then -1 else 0
--     because 1+k/2^n and 1+j/2^m are both in [1, 2),
--     so their quotient is in (1/2, 2)
--     so log2 of their quotient is in (-1, 1)
--     so the floor of log2 of their quotient is -1 or 0:
--       -1 if the quotient is less than 1; 0 if the quotient is >= 1
-- = n - m + if k/2^n < j/2^m then -1 else 0
-- = n - m + if k*2^m < j*2^n then -1 else 0
-- = n - m + if (k<<m) < (j<<n) then -1 else 0
--
-- The same derivation holds for ceil(log2(r)) with adjustment values being 0
-- and 1 rather than -1 and 0, and the comparison being <= instead of <.

-- Returns a tuple @(e, num', den')@ s.t. @2^e * num' 'Data.Ratio.%' den' ==
-- num % den@, and @num' % den'@ in /(1\/2, 2)/ (exclusive at both bounds).
{-# INLINE splitLog2Unchecked #-}
splitLog2Unchecked :: Integer -> Integer -> (Int, Integer, Integer)
splitLog2Unchecked num den = (n - m, num', den')
 where
  n = integerLog2Floor num
  m = integerLog2Floor den
  -- Note: the Bits instance for Integer doesn't define unsafeShiftL or shiftL,
  -- so it defaults to testing the sign of the argument and dispatching to left
  -- or right shifts. So, we call what its definition should have been instead.
  shl x (I# s) = shiftLInteger x s
  (num', den') = if m > n
    then (num `shl` (m-n), den)
    else (num, den `shl` (n-m))

{-# INLINE log2Unchecked #-}
log2Unchecked :: Integer -> Integer -> (Int, Ordering)
log2Unchecked num den =
  let (lg, num', den') = splitLog2Unchecked num den
  in  (lg, compare num' den')

-- The underscore-suffixed variants here are inlined versions so that GHC will
-- implement the exported functions by big chunks of optimized code, but not
-- try to include that code in the module interface.

{-# INLINE log2Approx_ #-}
log2Approx_ :: (HasCallStack, Real a) => a -> (Int, Ordering)
log2Approx_ x =
  let !xr = toRational x
      !num = numerator xr
      !den = denominator xr
  in  if num <= 0
        then error "log2With_: x <= 0"
        else log2Unchecked num den

{-# INLINE log2With_ #-}
log2With_ :: (HasCallStack, Real a) => (Ordering -> Int) -> a -> Int
log2With_ adj x =
  let (lg, cmp) = log2Approx_ x
  in  lg + adj cmp

-- | Returns an approximate base-2 logarithm of the argument.
--
-- The returned @Int@ is one of the two nearest integers to the exact result,
-- and the returned @Ordering@ tells whether the exact result is greater than,
-- equal to, or less than the @Int@.
log2Approx :: (HasCallStack, Real a) => a -> (Int, Ordering)
log2Approx = log2Approx_

-- | Returns the base-2 logarithm with custom rounding.
--
log2With :: (HasCallStack, Real a) => (Ordering -> Int) -> a -> Int
log2With = log2With_

-- | Returns the floor of the base-2 logarithm of a 'Real' argument.
log2Floor :: (HasCallStack, Real a) => a -> Int
log2Floor = log2With_ (\case LT -> -1; _ -> 0)

-- | Returns the ceiling of the base-2 logarithm of a 'Real' argument.
log2Ceiling :: (HasCallStack, Real a) => a -> Int
log2Ceiling = log2With_ (\case GT -> 1; _ -> 0)

{-# INLINE withPositiveInteger #-}
withPositiveInteger :: HasCallStack => (Integer -> r) -> Integer -> r
withPositiveInteger f x = if x <= 0
  then error "withPositiveInteger: x <= 0"
  else f x

-- | Returns the floor of the base-2 logarithm of an 'Integral' argument.
{-# INLINABLE ilog2Floor #-}
ilog2Floor :: (HasCallStack, Integral a) => a -> Int
ilog2Floor x = withPositiveInteger integerLog2Floor (toInteger x)

-- | Returns the ceiling of the base-2 logarithm of an 'Integral' argument.
{-# INLINABLE ilog2Ceiling #-}
ilog2Ceiling :: (HasCallStack, Integral a) => a -> Int
ilog2Ceiling x = withPositiveInteger
  (\xi -> let lg = integerLog2Floor xi in lg + if xi > bit lg then 1 else 0)
  (toInteger x)
