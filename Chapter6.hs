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
