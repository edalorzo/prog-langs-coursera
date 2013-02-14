signature STACK =
sig
	type 'a Stack
	val empty: 'a Stack
	val isEmpty: 'a Stack -> bool
	val cons: 'a * 'a Stack -> 'a Stack
	val head: 'a Stack -> 'a
	val tail: 'a Stack -> 'a Stack
end

structure Stack : STACK =
struct
	type 'a Stack = 'a list
	val empty = []

	fun isEmpty s = null s
	fun cons(x, s) = x::s
	fun head s = hd s
	fun tail s = tl s
end

structure CustomStack : STACK =
struct
	datatype 'a Stack = Nil | Cons of 'a * 'a Stack

	val empty = Nil
	fun isEmpty Nil = true
	  | isEmpty _ 	= false

	fun cons(x, s) = Cons(x, s)
	fun head s = case s of Nil => raise Empty | Cons(h,_) => h
	fun tail s = case s of Nil => raise Empty | Cons(_,t) => t

end