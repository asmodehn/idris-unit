module UnitTest

%default total
%access private

-- Data Types

data Status = Ok | Failure

data Color = Red | Green | Reset

ESC : String
ESC = singleton $ chr 27

CSI : String
CSI = ESC ++ "["

instance Show Color where
  show Red = CSI ++ "31m"
  show Green = CSI ++ "32m"
  show Reset = CSI ++ "0m"

green: String -> String
green s = (show Green) ++ s ++ (show Reset)

red: String -> String
red s = show Red ++ s ++ show Reset


instance Show Status where
  show Ok       = "[" ++  (green "Ok  ") ++  "] "
  show Failure  = "[" ++ (red "Fail") ++ "] "



-- Helper

printResult: (status: Status) -> (assertionText: String) -> IO ()
printResult Ok assertionText = putStrLn $ show Ok  ++ assertionText
printResult Failure assertionText = putStrLn $ show Failure  ++ assertionText

printHint: String -> IO ()
printHint x = putStrLn ("       " ++ x)

-- Public API

||| Tests for equality
public
assertEq: (Show a, Eq a) =>  (assertionText: String) -> (actual : a) -> (expected: a) -> IO ()
assertEq assertionText x y with (x == y)
  assertEq assertionText _ _              | True  = printResult Ok assertionText
  assertEq assertionText actual expected  | False =  do
                                                      printResult Failure assertionText
                                                      printHint $ show actual ++ " does not equal " ++ show expected

||| Tests for inequality
public
assertNotEq: (Show a, Eq a) =>  (assertionText: String) -> (actual : a) -> (expected: a) -> IO ()
assertNotEq assertionText x y with (x /= y)
  assertEq assertionText _ _              | True  = printResult Ok assertionText
  assertEq assertionText actual expected  | False =  do
                                                      printResult Failure assertionText
                                                      printHint $ show actual ++ " does  equal " ++ show expected
