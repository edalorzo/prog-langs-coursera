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
val only_capitals = List.filter (fn s => Char.isUpper(String.sub(s,0)))
val longest_string1 = foldl (fn (s1,s2) => if size s1 > size s2 then s1 else s2) ""
val longest_string2 = foldl (fn (s1,s2) => if size s1 >= size s2 then s1 else s2) "" 
fun longest_string_helper f = foldl (fn (s1,s2) => if f(size s1, size s2) then s1 else s2) ""
val longest_string3 = longest_string_helper (fn (x,y) => x > y)
val longest_string4 = longest_string_helper (fn (x,y) => x >= y)
val longest_capitalized = longest_string1 o only_capitals
val rev_string = implode o rev o explode

fun first_answer f xs = 
	case xs of
		[] => raise NoAnswer
	  | x::xs' => case f x of
	  				SOME e => e
	  			  | NONE => first_answer f xs'


fun all_answers f xs = 
	let
		(*this ensures mapping stops in the moment NONE is found*)
		val g = (fn y => if isSome y then y else raise NoAnswer) o f
		val h = foldl (fn (x,y) => x @ y) [] o map valOf o map g
	in
		SOME(h xs) handle NoAnswer => NONE
	end


val count_wildcards = g (fn () => 1) (fn var => 0)
val count_wild_and_variable_lengths = g (fn () => 1) (fn var => String.size var)
fun count_some_var (name,p) = g (fn () => 0) (fn var => if name=var then 1 else 0) p

fun check_pat p = 
	let
		fun names p = 
			case p of
				Variable x => [x]
			  | TupleP ps => List.foldl (fn (p,vars) => vars @ (names p)) [] ps
			  | ConstructorP(_,p) => names p
			  | _  => []

		fun different xs = 
			case xs of
				[] => true
			  | x::[] => true
			  | x::xs' => not(List.exists (fn e => e = x) xs') andalso different xs'				  

	in
		different (names p)
	end	

	
fun match(v,p) = 
	case (p,v) of
		(Wildcard, _) => SOME []
	  | (UnitP, Unit) => SOME []
	  |	(ConstP n, Const m) => if n = m then SOME [] else NONE
	  | (Variable x, _) => SOME [(x,v)]
	  | (ConstructorP(n1, cp), Constructor(n2,cv)) => if n1 = n2 then match(cv,cp) else NONE
	  | (TupleP tp, Tuple tv) => if List.length tp = List.length tv
	  							 then all_answers match (ListPair.zip(tv,tp))
	  							 else  NONE
	  | _ => NONE

fun first_match v ps =  SOME (first_answer (fn p => match(v,p)) ps) handle NoAnswer => NONE