--import Prelude hiding ((^))
--m ^ 0 = 0
--m ^ n = m * m ^ (n - 1)

--m ^ 0 = 1
--m ^ n = m * m ^ (n - 1)

--m ^ 0 = 1
--m ^ n = m * m ^ n - 1

--m ^ 0 = 1
--m ^ n = n * n ^ (m - 1)

--m ^ 0 = 1
--m ^ n = m * (^) m (n - 1)

--m ^ 0 = 1
--m ^ n = m * m * m ^ (n - 2)

--m ^ 0 = 1
--m ^ n = (m * m) ^ (n - 1)

len :: [a] -> Int
len [] = 0
len (_:xs) = 1 + len xs

drop' :: Int -> [a] -> [a]
drop' 0 xs = xs
drop' n [] = []
drop' n (_:xs) = drop'(n-1) xs

and' [] = True
--and' (b:bs) = b && and' bs
--and' (b:bs)
--  | b == False = False
--  | otherwise = and bs
--and' (b:bs) = and bs && b
and' (b: bs)
  | b = b
  | otherwise = and bs


concat' [] = []
concat' (xs:xss) = xs ++ concat xss

replicate' 0 _ = []
replicate' n x = x : replicate(n-1) x

--import Prelude hiding ((!!))
--(!!) :: [a] -> Int -> a
--(x:_) !! 0 = x
--(_:xs) !! n = xs !! (n-1)

elem' :: Eq a=> a -> [a] -> Bool
elem' _ [] = False
elem' x (y:ys)
  | x == y = True
  | otherwise = elem x ys
  
merge :: Ord a => [a] -> [a] -> [a]
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys)
  = if x<=y then x: merge xs (y:ys) else y : merge (x:xs) ys
  
halve :: [a] -> ([a],[a])
halve xs = splitAt(length xs`div`2) xs

msort::Ord a => [a] -> [a]
msort [] = []
msort [x] = [x]
msort xs = merge (msort ys) (msort zs)
  where (ys,zs) = halve xs