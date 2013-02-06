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
		not(repeated (get_vars p))
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


fun first_match v ps = SOME (first_answer (fn p => match(v,p)) ps) handle NoAnswer => NONE

exception ConstructorNotFound of string * typ
exception IncompatiblePatterns of pattern * pattern

fun typecheck_patterns(types, patterns) = 
	let

		(*t2 type declared, t1 type of pattern*)
		fun is_compatible(t1,t2) = 
			case (t1,t2) of
				(Anything,_) => true
			  | (_, Anything) => true
			  | (UnitT, UnitT) => true
			  | (IntT, IntT) => true
			  | (TupleT(ts1), TupleT(ts2)) => length(ts1) = length(ts2) andalso ListPair.all is_compatible (ts1,ts2)
			  | (Datatype(s1), Datatype(s2)) => s1 = s2
			  | _ => false

		fun find(name,argtype) = 
			let
				val found = List.find (fn (c,t,a) => name = c andalso is_compatible(a, argtype)) types
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
			  | Variable(_) => Anything
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
			  | (Variable(_), TupleP(_)) => p2
			  | (Variable(_), ConstructorP(_,_)) => p2
			  | (Variable(_), Wildcard) => p2
			  | (ConstP(_), ConstP(_)) => p1
			  | (ConstP(_), Variable(_)) => p1
			  | (ConstP(_), Wildcard) => p1
			  | (UnitP, UnitP) => p1
			  | (UnitP, Wildcard) => p2
			  | (TupleP(_), Variable(_)) => p1
			  | (TupleP(ps1), TupleP(ps2)) => if length(ps1) = length(ps2)  
			  								  then TupleP(ListPair.map generalize (ps1,ps2))
			  								  else raise IncompatiblePatterns(p1,p2)
			  | (TupleP(_), Wildcard) => p1
			  | (ConstructorP(_,_), Variable(_)) => p1
			  | (ConstructorP(_,_), ConstructorP(_,_)) => if to_type(p1) = to_type(p2)
			  											  then p1
			  											  else raise (print("\nIncompatible\n");IncompatiblePatterns(p1,p2))
			  | (ConstructorP(_,_), Wildcard) => p1
			  | _ => (print("\nExhausted options\n");raise IncompatiblePatterns(p1,p2))
				
			  
		fun check(patterns, prevpattern) = 
			case patterns of 
				[] => SOME(prevpattern)
			  | (p::ps) => check(ps,generalize(p, prevpattern)) 
			  				handle exn => NONE
	in
		case patterns of
			[] => SOME Anything
		  | p::ps => case check(ps,p) of
		  				NONE => NONE
		  			  | SOME p => SOME(to_type(p)) handle ConstructorNotFound(cn,p) => (print("\nPuff: " ^ cn ^ "\n");NONE)
		  			                                      | exn => NONE
	end
