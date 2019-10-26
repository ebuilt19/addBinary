------------------------------binary-------------------------------------------

--prepends zeros to the shorter of the two list
--to make the length of each list to be the same size
addZeros :: Int -> [Int] -> [Int]
addZeros 0 zerosAdded@(x:xs) = zerosAdded
addZeros n list@(x:xs) = [0] ++ addZeros (n-1)  list

addBinary :: [Int] -> [Int] -> [Int]
addBinary a b
    | (length a) > (length b) = addBinaryHelper a
    (addZeros ((length a)-(length b)) b) [] []

    | (length a) < (length b) = addBinaryHelper
    (addZeros ((length b)-(length a)) a) b [] []

    |otherwise = addBinaryHelper a b [] []

--takes in the two list to add a carry list and an empty list to build
--uses patern matching to determine when to carry a 1 or when to apply the one
addBinaryHelper :: [Int] -> [Int] -> [Int] -> [Int] -> [Int]
addBinaryHelper [] [] [] a@(x:xs)  = 1:a
addBinaryHelper [] [] [1] a@(x:xs) = 1:a

addBinaryHelper a b [] he@[] = if (last a) + (last b) == 2
  then addBinaryHelper (init a) (init b) [1] (0:he)
  else check
  where
    check = if (last a) + (last b) == 1
        then addBinaryHelper (init a) (init b) [] (1:he)
        else addBinaryHelper (init a) (init b) [] (0:he)


addBinaryHelper a b [1] he@(x:xs) = if  (last a) + (last b) == 2
    then addBinaryHelper (init a) (init b) [1] (1:he)
    else check
    where
      check = if (last a) + (last b) == 1
          then addBinaryHelper (init a) (init b) [1] (0:he)
          else addBinaryHelper (init a) (init b) [] (0:he)


addBinaryHelper a b [] y@(x:xs) = if (last a) + (last b) == 2
  then addBinaryHelper (init a) (init b) [1] (0:y)
   else check
   where
     check = if (last a) + (last b) == 1
         then addBinaryHelper (init a) (init b) [] (1:y)
         else addBinaryHelper (init a) (init b) [] (0:y)
