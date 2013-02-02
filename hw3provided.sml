(* Coursera Programming Languages, Homework 3, Provided Code *)

exception NoAnswer

datatype pattern = Wildcard
		 | Variable of string
		 | UnitP
		 | ConstP of int
		 | TupleP of pattern list
		 | ConstructorP of string * pattern

datatype valu = Const of int
	      | Unit
	      | Tuple of valu list
	      | Constructor of string * valu

fun g f1 f2 p =
    let 
	val r = g f1 f2 
    in
	case p of
	    Wildcard          => f1 ()
	  | Variable x        => f2 x
	  | TupleP ps         => List.foldl (fn (p,i) => (r p) + i) 0 ps
	  | ConstructorP(_,p) => r p
	  | _                 => 0
    end

(**** for the challenge problem only ****)

datatype typ = Anything
	     | UnitT
	     | IntT
	     | TupleT of typ list
	     | Datatype of string

(**** you can put all your code here ****)
fun only_capitals xs = List.filter (Char.isUpper o (fn s => String.sub(s,0))) xs
fun longest_string1 xs = foldl (fn (a,b) => if size a > size b then a else b) "" xs
fun longest_string2 xs = foldl (fn (a,b) => if size a >= size b then a else b) "" xs
fun longest_string_helper f xs = foldl (fn (a,b) => if f(size a, size b) then a else b) "" xs
val longest_string3 = longest_string_helper (fn (x,y) => x > y)
val longest_string4 = longest_string_helper (fn (x,y) => x >= y)
val longest_capitalized = longest_string2 o only_capitals
val rev_string = implode o rev o explode

fun first_anwer f xs = 
	case List.filter isSome (map f xs) of
	   (SOME x)::xs' => x
	  | _ => raise NoAnswer

fun all_answers f xs = 
	let 
		val opts = map f xs
		fun assemble(pending, checked) = 
			case pending of
			  [] => SOME checked
			  | (NONE)::_ => NONE
			  | (SOME p)::ps => assemble(ps, p @ checked)

	in
		assemble(opts, [])
	end	  

val tests1 =  
	[
		only_capitals [] = [],
		only_capitals ["one","two","three"] = [],
		only_capitals ["One","two","Three"] = ["One","Three"]
	]

val tests2 =  
	[
		longest_string1 [] = "",
		longest_string1 ["one"] = "one",
		longest_string1 ["one","two"] = "one",
		longest_string1 ["one","two","three"] = "three",
		longest_string1 ["one","two","three","fives"] = "three"
	]	

val tests3 = 
	[
		longest_string2 [] = "",
		longest_string2 ["one"] = "one",
		longest_string2 ["one","two"] = "two",
		longest_string2 ["one","two","three"] = "three",
		longest_string2 ["one","two","three","fives"] = "fives"
	]	

val tests4 = 
	[
		longest_string_helper (fn (x,y) => x > y) [] = "",
		longest_string_helper (fn (x,y) => x > y) ["one"] = "one",
		longest_string_helper (fn (x,y) => x > y) ["one","two"] = "one",
		longest_string_helper (fn (x,y) => x > y) ["one","two","three"] = "three",
		longest_string_helper (fn (x,y) => x > y) ["one","two","three","fives"] = "three"
	]		

val tests5 =  
	[
		longest_string3 [] = "",
		longest_string3 ["one"] = "one",
		longest_string3 ["one","two"] = "one",
		longest_string3 ["one","two","three"] = "three",
		longest_string3 ["one","two","three","fives"] = "three"
	]	

val tests6 = 
	[
		longest_string4 [] = "",
		longest_string4 ["one"] = "one",
		longest_string4 ["one","two"] = "two",
		longest_string4 ["one","two","three"] = "three",
		longest_string4 ["one","two","three","fives"] = "fives"
	]		

val tests7 = 
	[
		longest_capitalized [] = "",
		longest_capitalized ["one"] = "",
		longest_capitalized ["one","two"] = "",
		longest_capitalized ["one","Two","three"] = "Two",
		longest_capitalized ["one","two","Three","Fives"] = "Fives"
	]			

val tests8 = 
	[
		rev_string "" = "",
		rev_string "one" = "eno",
		rev_string "rotor" = "rotor"
	]				

val tests9 = 
	[
		((first_anwer (fn n => if n mod 2 = 0 then SOME n else NONE) []) handle NoAnswer => 0) = 0,
		((first_anwer (fn n => if n mod 2 = 0 then SOME n else NONE) [3,5,7]) handle NoAnswer => 0) = 0,
		((first_anwer (fn n => if n mod 2 = 0 then SOME n else NONE) [2]) handle NoAnswer => 0) = 2,
		((first_anwer (fn n => if n mod 2 = 0 then SOME n else NONE) [2,4]) handle NoAnswer => 0) = 2
	]					

val q8 = (fn xs => SOME (map valOf xs) handle Option => NONE) o map Int.fromString o map Char.toString o explode
val tests10 = 
	[
		all_answers q8 [] = SOME [],
		all_answers q8 ["123"] = SOME [1,2,3],
		all_answers q8 ["123","456"] = SOME [4,5,6,1,2,3],
		all_answers q8 ["123a"] = NONE
	]						

val tests11 = 
	[
	]					
val tests = tests1 @ tests2 @ tests3 @ tests4 @ tests5 @ tests6 @ tests7 @ tests8 @ tests9 @ tests10 @ tests11

val all_tests = List.all (fn x => x) tests