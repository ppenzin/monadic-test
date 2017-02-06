module MiniTest where

import Control.Applicative
import Control.Monad
import Control.Exception

data Verifier a = Value a | Failure String
                  deriving Show

instance Monad Verifier where
  v >>= f = case v of 
             Failure s -> Failure s
             Value x -> f x
  return  = Value
  fail    = Failure

instance Applicative Verifier where
  pure = return
  (<*>) = ap

instance Functor Verifier where
  fmap = liftM

data TestStatus = Pass | Fail | Error
                  deriving (Show, Eq)

-- | Convert bool test result to pass/fail (but not error)
fromBool :: Bool -> TestStatus
fromBool True  = Pass
fromBool False = Fail

-- | Convert (Maybe Bool) test result to pass/fail - Nothing stands for error
fromMaybeBool :: Maybe Bool -> TestStatus
fromMaybeBool (Just x) = fromBool x
fromMaybeBool Nothing  = Error

-- | Test failure message
_expectFailure :: (Verifier a) -> String -> Bool
_expectFailure (Failure msg) str = (str == msg)
_expectFailure _ _ = False

-- | Test value
_expectValue :: (Eq a) => (Verifier a) -> a -> Bool
_expectValue (Value x) y = (x == y)
_expectValue _ _ = False

fromEither :: Either String a -> Verifier a
fromEither (Left s)  = Failure s
fromEither (Right x) = Value x

-- | Test error string in "Either String a"
_expectLeft :: (Either String a) -> String -> Bool
_expectLeft v s = _expectFailure (fromEither v) s

-- | Test value in "Either String a"
_expectRight :: (Eq a) => (Either String a) -> a -> Bool
_expectRight v x = _expectValue (fromEither v) x

expectFailure' :: (Verifier a) -> String -> IO TestStatus
expectFailure' v s = catch (seq (v) (return (Just (_expectFailure v s)))) handleTestError >>= return . fromMaybeBool

expectValue' :: (Eq a) => (Verifier a) -> a -> IO TestStatus
expectValue' v x = catch (seq (v) (return (Just (_expectValue v x)))) handleTestError >>= return . fromMaybeBool

handleTestError :: SomeException -> IO (Maybe Bool)
handleTestError _ = return(Nothing)

-- | Add a test to the schedule that expects failure
expectFailure :: String -> (Verifier a) -> String -> [(String, IO TestStatus)] -> IO [(String, IO TestStatus)]
expectFailure desc action str tests = return (tests ++ [(desc, expectFailure' action str)])

-- | Add a test expecting a value to the schedule
expectValue :: (Eq a) => String -> (Verifier a) -> a -> [(String, IO TestStatus)] -> IO [(String, IO TestStatus)]
expectValue desc action val tests = return (tests ++ [(desc, expectValue' action val)])

-- | Initialize test schedule
initTestSession :: IO [(String, IO TestStatus)]
initTestSession = return []

-- | Run schedule and produce a summary
report :: [(String, IO TestStatus)] -> IO [(String, TestStatus)]
report tests = sequence (map run tests)
       where run (desc, a) = a >>= \res -> return (desc, res)

-- | Run schedule and print summary out
-- TODO:
--  - counts of passed/failed/error
--  - exit status
reportIO :: [(String, IO TestStatus)] -> IO ()
reportIO tests = report tests >>= prettyPrint

prettyPrint :: [(String, TestStatus)] -> IO ()
prettyPrint [] = return()
prettyPrint ((desc, res):rest) = putStrLn (desc ++ ": " ++ (show res)) >> prettyPrint rest

