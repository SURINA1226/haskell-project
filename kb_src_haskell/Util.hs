

module Util where
  type Assoc k a = [(k,a)]
  type Set a = [a]
  
  find   :: Eq k => k -> Assoc k a -> Maybe a
  find _ [] = Nothing
  find x ((y, v):zs) = if x==y then Just v else find x zs
    
  append :: (Eq k, Eq a) => Assoc k a -> Assoc k a -> Maybe (Assoc k a)
  append [] ys = Just ys
  append ((x,v):xs) ys =
        case find x ys of
            Nothing -> append xs ((x,v):ys)
            Just v1 -> Nothing

  union        :: Eq a => Set a -> Set a -> Set a
  union [] ys = ys
  union (x:xs) ys =
      if elem x ys then union xs ys else x:(union xs ys)

  intersection :: Eq a => Set a -> Set a -> Set a
  intersection [] ys = []
  intersection (x : xs) ys = 
    if elem x ys then x:(intersection xs ys ) else intersection xs ys


