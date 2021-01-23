module CollatzConjecture (collatz) where

collatz' :: Integer -> Integer -> Integer
collatz' 1 steps = steps
collatz' n steps = collatz' (if even n then n `div` 2 else 3 * n + 1) (steps + 1)

collatz :: Integer -> Maybe Integer
collatz n
  | n <= 0 = Nothing
  | otherwise = Just (collatz' n 0)
