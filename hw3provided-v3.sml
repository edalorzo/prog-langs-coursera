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
val longest_string1 = foldl (fn (a,b) => if size(a) > size(b) then a else b) ""
val longest_string2 = foldl (fn (a,b) => if size(a) < size(b) then b else a) ""
val longest_string_helper = fn p => foldl (fn (a,b) => if p(size(a), size(b)) then a else b) ""
val longest_string3 = longest_string_helper (fn (a,b) => a > b)
val longest_string4 = longest_string_helper (fn (a,b) => not(a < b))
val longest_capitalized = longest_string1 o only_capitals
val rev_string = implode o rev o explode

fun first_answer f xs =
    case xs of
	[] => raise NoAnswer
      | x::xs' => case f(x) of
		      SOME v => v
		    | _ => first_answer f xs'


fun all_answers f xs = 
    let
	fun iter(ys, acc) =
	    case ys of
		[] => SOME acc
	      | y::ys' => case f(y) of
			      SOME lst => iter(ys', acc @ lst)
			   |  _ => NONE
    in
	iter(xs, [])
    end
		       

val count_wildcards = g (fn () => 1) (fn s => 0)
val count_wild_and_variable_lengths = g (fn () => 1) (fn s => size s)
fun count_some_var(var,p) = g (fn () => 0) (fn s => if var = s then 1 else 0) p

fun check_pat p = 
    let
	fun get_var_names p =
	    case p of
		Variable x => [x] 
	      | ConstructorP(_, p') => get_var_names p'
	      | TupleP ps => foldl (fn (a,b) => b @ a) [] (map get_var_names ps)
	      | _ => []
	fun not_repeated vs =
	    case vs of
		[] => true
	      | v::vs' => not(List.exists (fn s => s = v) vs') andalso not_repeated(vs')
    in
	not_repeated(get_var_names p)
    end

fun match(v, p) =
    case (v,p) of
	(Unit, UnitP) => SOME []
      | (Unit, Wildcard) => SOME [] 
      | (Const n, ConstP m) => if n=m then SOME [] else NONE
      | (Const n, Variable x) => SOME [(x, v)] 
      | (Const _, Wildcard) => SOME []
      | (Constructor(s, v'), ConstructorP(s', p')) => if(s = s') then match(v', p') else NONE
      | (Constructor _, Wildcard) => SOME []
      | (Constructor _, Variable x) => SOME [(x, v)] 
      | (Tuple vs, TupleP ps) => (((all_answers match) (ListPair.zipEq(vs,ps))) handle UnequalLengths => NONE)
      | (Tuple _, Wildcard) => SOME []
      | _ => NONE   

fun first_match v ps = SOME (first_answer (fn p => match(v,p)) ps) handle NoAnswer => NONE
