
(*
datatype 'a listof = Nil | Cons of 'a * 'a listof

val r = Cons(1, Cons(2, Cons(3, Cons(4, Cons(5, Nil)))))


fun sum Nil = 0
  | sum(Cons(n,tail)) = n + sum tail

val s = sum(r)  


fun foldr f x Nil = x
  | foldr f x (Cons(a, tail)) = f(a, (foldr f x tail))

val sum = foldr (op +) 0
val s = sum([1,2,3,4,5])

val sum = foldr op + 0
val product = foldr op * 1

val s1 = sum r
val s2 = product r

val also = fn(a,b) => a andalso b
val t = Cons(true, Cons(true, Cons(true, Nil)))
val s3 = foldr also true t
*)

fun doubleandcons xs =
  case xs of
    [] => []
  | x::xs' => x*2::doubleandcons(xs')

fun double n = 2 * n

(*
fun fandcons f xs =
  case xs of
      [] => []
    | x::xs' => (f x)::fandcons f xs'
*)

fun fandcons f = (fn a => fn b => b :: a) o f















