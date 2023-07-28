fib :: Int -> Int
fib n = case n of
    0 -> 0
    1 -> 1
    _ -> fib(n-1)+fib(n-2)

listReverse  :: Eq a => [a] -> [a]
listReverse  = \list ->
    case list of
        [] -> []
        x:xs -> listReverse xs ++ [x]


listAdd :: [Int] -> [Int] -> [Int]
listAdd [] list2 = list2
listAdd list1 [] = list1
listAdd (x:xs) (y:ys) = x + y : listAdd xs ys

inList :: Eq a => [a] -> a -> Bool
inList [] a = False
inList (x:xs) a
  | a == x = True
  | otherwise = inList xs a

sumTailRec :: Num a => [a] -> a
sumTailRec [] = 0
sumTailRec (x:t) = x + sumTailRec t
