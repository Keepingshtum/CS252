{-
  Name: Anant Shukla
  Class: CS 252
  Assigment: HW1
  Date: Feb 13 2023
  Description: Implements a basic calculator, assuming there is no bigNum class in Haskell.
-}

module BigNum (
  BigNum,
  bigAdd,
  bigSubtract,
  bigMultiply,
  bigEq,
  bigDec,
  bigPowerOf,
  prettyPrint,
  stringToBigNum,
) where

type Block = Int -- An Int from 0-999

type BigNum = [Block]

maxblock = 1000

bigAdd :: BigNum -> BigNum -> BigNum
bigAdd x y = bigAdd' x y 0


-- Logic : Append the result of the sum of the blocks to the start of the list
-- If there is a carry, pass that on to the next block
-- x,y are the two blocks, z is used for carry 
bigAdd' :: BigNum -> BigNum -> Block -> BigNum
bigAdd' [] [] 0 = []
bigAdd' [] [] z = [z]
bigAdd' [] y z = bigAdd' [0] y z
bigAdd' x [] z = bigAdd' x [0] z 
bigAdd' (x:xs) (y:ys) z 
  | (x+y+z) < maxblock = (x+y+z) : (bigAdd' xs ys 0) -- if no carry is present, continue with the rest of the blocks as planned
  | (x+y+z) >= maxblock = ((x+y+z) - maxblock) : (bigAdd' xs ys 1) -- for the carry case

bigSubtract :: BigNum -> BigNum -> BigNum
bigSubtract x y =
  if length x < length y
    then error "Negative numbers not supported"
    else reverse $ stripLeadingZeroes $ reverse result
      where result = bigSubtract' x y 0

stripLeadingZeroes :: BigNum -> BigNum
stripLeadingZeroes (0:[]) = [0]
stripLeadingZeroes (0:xs) = stripLeadingZeroes xs
stripLeadingZeroes xs = xs

-- Negative numbers are not supported, so you may throw an error in these cases
-- x,y are blocks to be subtracted, z is for borrow
-- Logic similar to bigSubtract applies 
bigSubtract' :: BigNum -> BigNum -> Block -> BigNum
bigSubtract' [] [] 0 = []
bigSubtract' [] [] z = [z]
bigSubtract' [] y z = bigSubtract' [0] y z 
bigSubtract' x [] z = bigSubtract' x [0] z
bigSubtract' (x:xs) (y:ys) z
  | (x-y-z) >= 0 = (x-y-z) : (bigSubtract' xs ys 0) -- normal case
  | (x-y-z) < 0 = ((x-y-z) + maxblock) : (bigSubtract' xs ys 1) -- borrow case

bigEq :: BigNum -> BigNum -> Bool
bigEq _ _ = error "Your code here"

bigDec :: BigNum -> BigNum
bigDec x = bigSubtract x [1]

-- Handle multiplication following the same approach you learned in grade
-- school, except dealing with blocks of 3 digits rather than single digits.
-- If you are having trouble finding a solution, write a helper method that
-- multiplies a BigNum by an Int.
bigMultiply :: BigNum -> BigNum -> BigNum
bigMultiply x [0] = [0]
bigMultiply [0] y = [0]
bigMultiply x [y] = bigMultiply' x [y] 0
bigMultiply x (y:ys) = bigAdd (bigMultiply x [y]) ([0] ++ bigMultiply x ys) -- to simulate the adding of 0 after each stack of basic multiplication


-- x,y blocks are mutliplied together. Z is for carry.
bigMultiply' :: BigNum -> BigNum -> Block -> BigNum
bigMultiply' [] _  0 =  []
bigMultiply' [] _  z =  [z]
bigMultiply' (x:xs)[y] z 
  | (x*y+z) < maxblock = (x*y+z) : (bigMultiply' xs [y] 0) -- in case there is no carry, no need to do anything else
  | (x*y+z) >= maxblock = ((x*y+z) `mod` maxblock) : (bigMultiply' xs [y] ((x*y+z) `quot` maxblock)) -- carry can be more than 1, so compute it by dividing by maxblock
  

bigPowerOf :: BigNum -> BigNum -> BigNum
bigPowerOf x [0] = [1]
bigPowerOf x y = (bigMultiply x (bigPowerOf x (bigDec y)))

prettyPrint :: BigNum -> String
prettyPrint [] = ""
prettyPrint xs = show first ++ prettyPrint' rest
  where (first:rest) = reverse xs

prettyPrint' :: BigNum -> String
prettyPrint' [] = ""
prettyPrint' (x:xs) = prettyPrintBlock x ++ prettyPrint' xs

prettyPrintBlock :: Int -> String
prettyPrintBlock x | x < 10     = ",00" ++ show x
                   | x < 100    = ",0" ++ show x
                   | otherwise  = "," ++ show x

stringToBigNum :: String -> BigNum
stringToBigNum "0" = [0]
stringToBigNum s = stringToBigNum' $ reverse s

stringToBigNum' :: String -> BigNum
stringToBigNum' [] = []
stringToBigNum' s | length s <= 3 = read (reverse s) : []
stringToBigNum' (a:b:c:rest) = block : stringToBigNum' rest
  where block = read $ c:b:a:[]

sig = "9102llaf"
