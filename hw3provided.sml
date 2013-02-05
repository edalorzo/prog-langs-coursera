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
val longest_capitalized = longest_string1 o only_capitals
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
fun count_some_var (s,p) = g (fn unit => 0) (fn n => if n=s then 1 else 0) p

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
			  							 then r (ListPair.zip(vs,ps)) 
			  							 else NONE
			  | _ => NONE
	end


fun first_match (v, ps) = SOME (first_answer (fn p => match(v,p)) ps) handle NoAnswer => NONE


exception ConstructorNotFound of string * typ
exception IncompatiblePatterns of pattern * pattern

fun typecheck_patterns(types, patterns) = 
	let

		fun find(name,argtype) = 
			let
				val found = List.find (fn (c,t,a) => name = c andalso argtype = a) types
			in
				case found of
					NONE => raise ConstructorNotFound(name, argtype)
				  | SOME (c,t,a) => Datatype t 
			end

		fun to_type(pattern) =
			case pattern of
				Wildcard => Anything
			  | UnitP => UnitT
			  | ConstP(_) => IntT
			  | Variable(_) => IntT
			  | TupleP(ps) => TupleT( map (fn p => to_type(p)) ps)
			  | ConstructorP(cn, p) => find(cn, to_type(p))

		
		fun generalize(p1,p2) = 
			case (p1,p2) of
				(Wildcard, Wildcard) => p1
			  | (Wildcard, Variable(_)) => p1
			  | (Wildcard, ConstP(_)) => p2
			  | (Wildcard, UnitP) => p1
			  | (Wildcard, ConstructorP(_,_)) => p2
			  | (Wildcard, TupleP(_)) => p2
			  | (Variable(_), Variable(_)) => p1
			  | (Variable(_), ConstP(_)) => p2
			  | (Variable(_), Wildcard) => p2
			  | (ConstP(_), ConstP(_)) => p1
			  | (ConstP(_), Variable(_)) => p1
			  | (ConstP(_), Wildcard) => p1
			  | (UnitP, UnitP) => p1
			  | (UnitP, Wildcard) => p2
			  | (TupleP(ps1), TupleP(ps2)) => if length(ps1) = length(ps2)  
			  								  then TupleP(ListPair.map generalize (ps1,ps2))
			  								  else raise IncompatiblePatterns(p1,p2)
			  | (TupleP(_), Wildcard) => p1
			  | (ConstructorP(_,_), ConstructorP(_,_)) => if to_type(p1) = to_type(p2)
			  													handle ConstructorNotFound(_,_) => false
			  											  then p1
			  											  else raise IncompatiblePatterns(p1,p2)
			  | (ConstructorP(_,_), Wildcard) => p1
			  | _ => raise IncompatiblePatterns(p1,p2)
				
			  
		fun check(patterns, prevpattern) = 
			case patterns of 
				[] => SOME(to_type(prevpattern))
			  | (p::ps) => check(ps,generalize(p, prevpattern)) 
			  				handle IncompatiblePatterns(_,_) => NONE
	in
		case patterns of
			[] => NONE
		  | p::ps => check(ps,p)
	end


datatype rango = Sota | Reina | Rey | As | Numero of int

val types = [("Red","color",UnitT),("Blue","color",UnitT)]
val tests16 = 
	[
		typecheck_patterns(types, [Wildcard, Wildcard]) = SOME Anything,
		typecheck_patterns(types, [Wildcard, Variable "x"]) = SOME Anything,
		typecheck_patterns(types, [Wildcard, ConstP 10]) = SOME IntT,
		typecheck_patterns(types, [Wildcard, UnitP]) = SOME Anything,
		typecheck_patterns(types, [Wildcard, TupleP[Wildcard, Variable "x"]]) = SOME(TupleT[Anything, IntT]),
		typecheck_patterns(types, [Wildcard, ConstructorP("Red",UnitP)]) = SOME(Datatype("color")),
		typecheck_patterns(types, [Variable "x", Wildcard]) = SOME Anything,
		typecheck_patterns(types, [Variable "x", ConstP 10]) = SOME IntT,
		typecheck_patterns(types, [Variable "x", Variable "y"]) = SOME IntT,
		typecheck_patterns(types, [ConstP 10, Wildcard]) = SOME IntT,
		typecheck_patterns(types, [ConstP 10, Variable "x"]) = SOME IntT,
		typecheck_patterns(types, [ConstP 10, ConstP 11]) = SOME IntT,
		typecheck_patterns(types, [UnitP, Wildcard]) = SOME Anything,
		typecheck_patterns(types, [TupleP[Variable "x", ConstP 10], Wildcard]) = SOME(TupleT[IntT, IntT]),
		typecheck_patterns(types, [ConstructorP("Red",UnitP),ConstructorP("Blue",UnitP), Wildcard]) = SOME(Datatype("color")),
		typecheck_patterns(types, [Variable "x", UnitP]) = NONE
		(*typecheck_patterns(types, [ConstructorP("Red",ConstP 10)]) = NONE*)


	]

val tests = tests16

val all_tests = List.all (fn x => x) tests	

