module Example 
( uniqueCons
, sortedMerge
, exactSubsetSum
, approxSubsetSum
) where

uniqueCons :: (Eq a) => a -> [a] -> [a]
uniqueCons x [] = [x]
uniqueCons x (y:xs) 
  | x == y = y:xs
  | x /= y = x:y:xs

sortedMerge :: (Ord a) => [a] -> [a] -> [a]
sortedMerge [] x = x
sortedMerge x [] = x
sortedMerge ax@(x:xs) ay@(y:ys)
  | x < y = x `uniqueCons` sortedMerge xs ay
  | x >= y = y `uniqueCons` sortedMerge ax ys

quicksort :: Ord a => [a] -> [a]
quicksort []     = []
quicksort (p:xs) = (quicksort lesser) ++ [p] ++ (quicksort greater)
    where
        lesser  = filter (< p) xs
        greater = filter (>= p) xs

exactSubsetSum :: (Ord a, Num a) => [a] -> a -> a
exactSubsetSum arr t
  | t < 0 = negate $ exactSubsetSum (quicksort $ map negate arr) (negate t)
  | otherwise = last $ last $ foldl (\acc el -> 
      let prev_l = last acc
          new_l = prev_l `sortedMerge` map (+el) prev_l
          filtered_new_l = filter (<=t) new_l
      in acc ++ [filtered_new_l]) [[0]] arr

percentDiff :: (Fractional a) => a -> a -> a
percentDiff estimate real = abs $ (estimate-real) / real

trim :: (Ord a, Num a, Fractional a) => [a] -> a -> [a]
trim (l:ls) delta = foldl (\newL next_l -> 
  if (percentDiff (last newL) next_l) > delta 
    then newL++[next_l] 
    else newL) [l] ls

approxSubsetSum :: (Ord a, Num a, Fractional a) => [a] -> a -> a -> a
approxSubsetSum arr t epsilon
  | t < 0 = negate $ exactSubsetSum (quicksort $ map negate arr) (negate t)
  | otherwise = last $ last $ foldl (\acc el -> 
      let prev_l = last acc
          new_l = prev_l `sortedMerge` map (+el) prev_l
          len = fromIntegral $ length arr
          trimmed_new_l = trim new_l (epsilon / (2*len))
          filtered_new_l = filter (<=t) trimmed_new_l
      in acc ++ [filtered_new_l]) [[0]] arr

  
main :: IO ()    -- This says that main is an IO action.
main = return () -- This tells main to do nothing.