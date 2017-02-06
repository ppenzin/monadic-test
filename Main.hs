module Main where

import Control.Exception

import MiniTest

-- | Return number div 2, but if number is odd use monadic fail to throw error
getHalf :: (Monad m) => Integer -> m Integer
getHalf n = if (n == 0) then error "panic" -- deliberate bug
       else if (odd n) then fail "Odd number" else return(n `div` 2)

testFail :: Either String Integer -> String -> Bool
testFail (Left str) msg = (str == msg)
testFail _ _            = False

handle :: SomeException -> IO a
handle e = fail ("Got a " ++ show e)

main = putStrLn "--- Purely functional tests ---"
    >> putStr "Get half of 3 raises error: "  >> putStrLn (show (_expectFailure (getHalf 3) "Odd number"))
    >> putStr "Get half of 4 yield 2: "       >> putStrLn (show (_expectValue (getHalf 4) 2))
    >> putStr "Get half of 5 yield 2: "       >> putStrLn (show (_expectValue (getHalf 5) 2))
    >> putStrLn "--- Tests with error handling ---"
    >> putStr "Get half of 3 raises error: "  >> expectFailure' (getHalf 3) "Odd number" >>= print
    >> putStr "Get half of 4 yield 2: "       >> expectValue' (getHalf 4) 2 >>= print
    >> putStr "Get half of 5 yield 2: "       >> expectValue' (getHalf 5) 2 >>= print
    >> putStr "Get half of 0 yield 0: "       >> expectValue' (getHalf 0) 0 >>= print
    >> putStrLn "--- Tests with pretty printing ---"
    >> initTestSession
    >>= expectFailure "Get half of 3 raises error" (getHalf 3) "Odd number"
    >>= expectValue "Get half of 4 yield 2" (getHalf 4) 2
    >>= expectValue "Get half of 5 yield 2" (getHalf 5) 2
    >>= expectValue "Get half of 0 yield 0" (getHalf 0) 0
    >>= reportIO
