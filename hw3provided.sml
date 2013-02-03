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

(*val g = fn : (unit -> int) -> (string -> int) -> pattern -> int*)
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

fun count_wildcards p = g (fn unit => 1) (fn s => 0) p
fun count_wild_and_variable_lengths p = g (fn unit => 1) (fn s => size s) p
fun count_some_vars (s,p) = g (fn unit => 0) (fn n => if n=s then 1 else 0) p

fun check_pat p = 
	let
		fun get_vars p =
			case p of
	  		    Variable x        => [x]
	  		  | TupleP ps         => List.foldl (fn (p,vs) => (get_vars p) @ vs) [] ps
	  		  | ConstructorP(_,p) => get_vars p
	  		  | _                 => []

    	fun repeated xs = 
    		case xs of 
    			[] => false
    		  | (x::xs') => List.exists ( fn s => s = x ) xs' orelse repeated xs'
	in
		repeated (get_vars p)
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

(*Converts a string of numbers into a option list of numbers (i.e. "123" into SOME [1,2,3] and "123a" into NONE*)
val q8 = (fn xs => SOME (map valOf xs) handle Option => NONE) o map Int.fromString o map Char.toString o explode
val tests10 = 
	[
		all_answers q8 [] = SOME [],
		all_answers q8 ["123"] = SOME [1,2,3],
		(*Notice that this test can fail to other depending on how they use the @ operator in the function*)
		all_answers q8 ["123","456"] = SOME [4,5,6,1,2,3],
		all_answers q8 ["123a"] = NONE
	]						

val tests11 = 
	[
		
		count_wildcards(UnitP) = 0,
		count_wildcards(Wildcard) = 1,
		count_wildcards(ConstructorP("Test", Wildcard)) = 1,
		count_wildcards(TupleP([Wildcard, Wildcard, Wildcard])) = 3,
		count_wildcards(ConstructorP("Test", TupleP([Wildcard, Wildcard]))) = 2,
		count_wildcards(ConstructorP("Test", TupleP([Wildcard, Variable "x", Wildcard, Variable "y"]))) = 2,
		count_wildcards(ConstructorP("Test", TupleP([ConstructorP("More",Wildcard),ConstructorP("EvenMore",Wildcard)]))) = 2,
		count_wildcards(TupleP([TupleP([ConstP 37, Wildcard, ConstructorP("Test", TupleP([Wildcard, ConstP 10, TupleP([Wildcard, Wildcard])]))])])) = 4
	]

val tests12 = 
	[
		count_wild_and_variable_lengths(UnitP) = 0,
		count_wild_and_variable_lengths(Variable "x") = 1,
		count_wild_and_variable_lengths(ConstructorP("Test", TupleP([Wildcard,Variable "one"]))) = 4,
		count_wild_and_variable_lengths(ConstructorP("Test", TupleP([ConstructorP("More",Wildcard),ConstructorP("EvenMore",Variable "five")]))) = 5,
		count_wild_and_variable_lengths(TupleP([TupleP([ConstP 37, Wildcard, ConstructorP("Test", TupleP([Variable "x", ConstP 10, TupleP([Wildcard, Variable "y"])]))])])) = 4
	]

val tests13 = 
	[
		count_some_vars("x", UnitP) = 0,
		count_some_vars("x", Variable "x") = 1,
		count_some_vars("x", ConstructorP("Test",Variable "x")) = 1,
		count_some_vars("x", TupleP([Wildcard, UnitP, ConstP 10, Variable "y", Variable "x"])) = 1,
		count_some_vars("x", TupleP([ConstructorP("Test",TupleP([Variable "x", Variable "y", Variable "z"])), Variable "x"])) = 2
	]


val tests = tests1 @ tests2 @
			tests3 @ tests4 @
			tests5 @ tests6 @
			tests7 @ tests8 @
			tests9 @ tests10 @
			tests11 @ tests12 @
			tests13

val all_tests = List.all (fn x => x) tests
