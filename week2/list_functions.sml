fun sum_list (xs: int list) =
  if null xs
  then 0
  else hd xs + sum_list(tl xs)

fun list_product (xs : int list) =
  if null xs
  then 1
  else hd xs * list_product(tl xs)

fun countdown (x : int) =
  if x = 0
  then []
  else x::countdown (x-1)

fun append (xs : int list, ys : int list) =
  if null xs
  then ys
  else (hd xs) :: append ((tl xs), ys)

fun factorial (n : int) = list_product(countdown n)

fun countup_from1(x : int) =
    let
      fun count (from : int) =
         if from=x
         then x::[]
         else from :: count(from+1)
    in
      count (1)
    end

