datatype set = S of { insert : int -> set, member : int -> bool, size : unit -> int }


val empty_set =
	let
		fun make_set xs = (* xs is a "private field" in result *)
			let (* contains a "private method" in result *)
				fun contains i = List.exists (fn j => i=j) xs
			in
				S { insert = fn i => if contains i
   									 then make_set xs
   									 else make_set (i::xs), 
   					member = contains, 
   					size = fn () => length xs
 				}
			end
	in
		make_set []
	end

val S(s1) = empty_set
val S(s2) = (#insert s1) 10
val S(s3) = (#insert s2) 11
val n = (#size s3) () (*prints 2*)
val b1 = (#member s3) 11 (*prints true*)
val b2= (#member s3) 12 (*prints false*)