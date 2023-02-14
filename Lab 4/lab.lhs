> import Data.List

Experiment with foldl, foldr, and foldl'

First, implement your own version of the foldl function,
defined as myFoldl

> myFoldl :: (a -> b -> a) -> a -> [b] -> a
> myFoldl _ x [] = x
> myFoldl f x (y:ys) =  myFoldl f (f x y) ys 


Next, define a function to reverse a list using foldl.

> myReverse :: [a] -> [a]
> myReverse x = foldl (\z y -> y:z) [] x

Now define your own version of foldr, named myFoldr

> myFoldr :: (a -> b -> b) -> b -> [a] -> b
> myFoldr _ x [] = x
> myFoldr f x (y:ys) = f y (myFoldr f x ys) 


Now try using foldl (the library version, not yours) to sum up the numbers of a large list.
Why is it so slow?

> myListSum ::(Num a) => [a] -> a
> myListSum x = foldl (\acc y -> acc + y) 0 x

Instead of foldl, try using foldl'.
Why is it faster?
(Read http://www.haskell.org/haskellwiki/Foldr_Foldl_Foldl%27 for some hints)

> myListSum2 ::(Num a) => [a] -> a
> myListSum2 x = foldl' (\acc y -> acc + y) 0 x


For an extra challenge, try to implement foldl in terms of foldr.
See http://www.haskell.org/haskellwiki/Foldl_as_foldr for details.


> foldlAsFoldr :: (a -> b -> a) -> a -> [b] -> a
> foldlAsFoldr f a bs = foldr (\b g x -> g (f x b)) id bs a


Testing out the above in myReverse can be done as follows:

> myReverseUsingfoldlAsFoldr :: [a] -> [a]
> myReverseUsingfoldlAsFoldr x = foldlAsFoldr (\z y -> y:z) [] x


Next, using the map function, convert every item in a list to its absolute value

> listAbs :: [Integer] -> [Integer]
> listAbs x = map (abs) x

Finally, write a function that takes a list of Integers and returns the sum of
their absolute values.

> sumAbs :: [Integer] -> Integer
> sumAbs x = sum(map (abs) x)

