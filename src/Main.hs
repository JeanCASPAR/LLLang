module Main where

import Types

main :: IO ()
main = let
    t1 = Forall . Forall . Tuple $ [Variable 4 , Variable 2,
        Forall . Tuple $ [Variable 1, Variable 3]
        ]
    t2 = Forall . Tuple $ [Variable 5, Variable 1]
  in do
    putStrLn "Hello, Haskell!"
    print t1
    print t2
    print (specialize t1 t2)