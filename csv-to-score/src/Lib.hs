module Lib
    ( increasing
    ) where

increasing list = list >>= const [False]
