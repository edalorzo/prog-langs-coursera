
(* Sums all integers in a list*)
fun sum(xs: int list) = 
    if null xs 
    then 0
    else hd xs + sum(tl xs)

(* Appends the integers of a list into another *)
fun append(xs: int list, ys: int list) =
    if null xs 
    then ys 
    else hd xs :: append(tl xs, ys)

    

