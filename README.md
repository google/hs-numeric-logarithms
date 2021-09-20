# numeric-logarithms

Efficient rounded log2 algorithms on integral and rational types.

[![Stack CI](https://github.com/google/hs-numeric-logarithms/actions/workflows/stack-ci.yml/badge.svg)](https://github.com/google/hs-numeric-logarithms/actions/workflows/stack-ci.yml)

## Disclaimer

This is not an officially supported Google product.

## Hackage Status

* [![numeric-logarithms](https://badgen.net/runkit/awpr/hackage/v/numeric-logarithms?icon=haskell&cache=600)](https://hackage.haskell.org/package/numeric-logarithms)
  ![Uploaded](https://badgen.net/runkit/awpr/hackage/t/numeric-logarithms?cache=600)
  ![Haddock](https://badgen.net/runkit/awpr/hackage/d/numeric-logarithms?cache=600)

## Overview

This provides a user-facing API for the GHC primitive `integerLog2#`, in the
form of functions computing the floor, ceiling, or other rounded form of the
base-2 logarithm of `Integral` or `Rational` types.
