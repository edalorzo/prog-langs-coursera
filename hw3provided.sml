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
(* Author: Edwin Dalorzo *)

fun only_capitals xs = List.filter (Char.isUpper o (fn s => String.sub(s,0))) xs
fun longest_string1 xs = foldl (fn (a,b) => if size a > size b then a else b) "" xs
fun longest_string2 xs = foldl (fn (a,b) => if size a >= size b then a else b) "" xs
fun longest_string_helper f xs = foldl (fn (a,b) => if f(size a, size b) then a else b) "" xs
val longest_string3 = longest_string_helper (fn (x,y) => x > y)
val longest_string4 = longest_string_helper (fn (x,y) => x >= y)
val longest_capitalized = longest_string2 o only_capitals
val rev_string = implode o rev o explode

fun first_answer f xs = 
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

fun match (value, pattern) = 
	let
		val r = all_answers (fn (p,v) => match (p,v))
	in
		case (pattern,value) of 
			 	(Wildcard,_) => SOME []
		   	  | (UnitP, Unit) => SOME []
		      | (ConstP p, Const v) => if p=v then SOME [] else NONE
		      | (Variable p, Const v) => SOME [(p,Const v)]
		      | (ConstructorP (pn, p), Constructor (vn,v)) => if pn=vn then r [(v,p)] else NONE
			  | (TupleP ps, Tuple vs) => if length(ps) = length(vs) 
			  							 then r (ListPair.zipEq(vs,ps)) 
			  							 else NONE
			  | _ => NONE
	end


fun first_match (v, ps) = SOME (first_answer (fn p => match(v,p)) ps) handle NoAnswer => NONE
(*
fun typecheck_patterns(types, patterns) = 
	let

		fun check((consname, constype, argtype), patterns) = 
			case patterns of
				[] => []
			   | (p::ps) => 
			

	in
	end
*)