myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl f v [] = v -- Base Case, when list empty
myFoldl f v (x:xs) = myFoldl f (f v x) xs -- Recursion, Accumulator = a, then Recursion again using accumulator and current list item x, then next item

myFoldr :: (a -> b -> b) -> b -> [a] -> b
myFoldr f v [] = v -- Base Case, when list empty
myFoldr f v (x:xs) = f x (myFoldr f v xs) -- Recursion, Accumulator = a, front item = b, then Recursion again using accumulator and next list item

alternativeMap :: (a -> b) -> (a -> b) -> [a] -> [b]
alternativeMap f g [] = [] -- Base Case, when list empty
alternativeMap f g (x:xs) = f x : alternativeMap g f xs

myLength :: [a] -> Int
myLength l = foldr f 0 l -- 2nd argument = 0, last list item = l and applies f to 0
    where
        f x y = y + 1 -- Add 1 for each item encountered

myFilter :: (a ->Bool) -> [a] -> [a]
myFilter f xs = foldl(\k x -> if f x then k . (x:) else k) id xs []

sumsqeven :: [Int] -> Int
sumsqeven = sum . map(^2) . filter (even)  -- Utilize outside functions in conjunction to achieve desire outcome, filter by evens, then square all nums, then sum them.



