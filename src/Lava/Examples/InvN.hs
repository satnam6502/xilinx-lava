module Lava.Examples.InvN where

import Lava

inv4 :: Hardware m bit => bit -> m bit
inv4 = inv >-> inv >-> inv >-> inv

invNTop :: RTL ()
invNTop
  = do setModuleName "invN"
       a <- input "invNa" BitType
       b <- inv4 a
       output "invNb" b

inv4x2 :: Hardware m bit => (bit, bit) -> m (bit, bit)
inv4x2 = vpar2 inv4 inv4

inv4x2Top :: RTL ()
inv4x2Top
  = do setModuleName "inv4x2"
       a <- input "a" BitType
       b <- input "a" BitType
       (c, d) <- inv4x2 (a, b)
       output "c" c
       output "c" d
