(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)

(* 1a. Return NONE if the string is not in the list, else return SOME lst where lst is identical 
to the argument list except the string is not in it. *)
fun all_except_option(item: string, items: string list) =
	let 
		fun move(xs, ys) = 
			case xs of 
			  [] => NONE
			| (x::xs') => if same_string(x, item) then SOME (xs' @ ys) else move(xs',x::ys)
	in
		move(items, [])
	end

(* 1b. The result has all the strings that are in some list in substitutions that also has s, 
	but s itself should not be in the result. *)
fun get_substitutions1(subs: string list list, item: string) = 
	case subs of
		[] => []
	  | (xs::xss) => case all_except_option(item, xs) of
	  				 NONE => get_substitutions1(xss, item)
	  			   | SOME xs' => xs' @ get_substitutions1(xss, item)

(* 1c. Same as the previous one but using a tail-recursive helper function. *)
fun get_substitutions2(subs: string list list, item: string) =
	let
		fun iterate(source: string list list, dest: string list) = 
			case source of
				[] => dest
			  | (xs::xss) => case all_except_option(item, xs) of
			  					NONE => iterate(xss, dest)
			  				  | SOME xs' => iterate(xss, xs' @ dest)
	in
		iterate(subs, [])
	end
(* 1d. The result is all the full names you can produce by substituting for the first name 
   (and only the first name) using substitutions. *)
fun similar_names(subs: string list list, name:{first:string, middle:string, last:string}) = 
	let
		val {first=f, middle=m, last=l} = name
		val other = get_substitutions2(subs, f)
		fun iterate(names: string list) =
			case names of
				[] => []
			  | (x::xs) => {first=x, middle=m, last=l}::iterate(xs)
	in
		name::iterate(other)
	end

(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

(* put your solutions for problem 2 here *)

fun card_color(c) = 
	case c of
		(Clubs, _) => Black
	  | (Spades, _) => Black
	  | (Diamonds,_) => Red
	  | (Hearts,_) => Red


fun card_value(c) = 
	case c of
		(_,Num n) => n
	  | (_,Ace) => 11
	  | _ => 10

fun remove_card(cs: card list, c: card, e: exn) = 
	let 
		fun remove(source: card list, dest: card list) = 
			case source of
				[] => raise e
			  |	(x::xs) => if x=c then dest @ xs else remove(xs, x::dest)
	in
		remove(cs, [])
	end

fun all_same_color(cs: card list) = 
	case cs of
		  [] => false
	 | _::[] => true
	 | head::(neck::rest) => (card_color(head)=card_color(neck) andalso all_same_color(neck::rest))


fun sum_cards(cs: card list) = 
	let
		fun sum(cards: card list, acc: int) = 
			case cards of
				[] => acc
			  | x::xs => sum(xs, acc + card_value(x))
	in
		sum(cs, 0)
	end


val test1 = all_except_option("orange",["apple","grape","orange","lemon"]) = SOME ["lemon","grape", "apple"]
val test2 = all_except_option("tangerine",["apple","grape","orange","lemon"]) = NONE
val test3 = get_substitutions1( [["Fred","Frederick"], ["Elizabeth","Betty"], ["Freddie","Fred","F"]], "Fred") = ["Frederick","F","Freddie"]
val test4 = get_substitutions1([["Fred","Frederick"],["Jeff","Jeffrey"],["Geoff","Jeff","Jeffrey"]], "Jeff") = ["Jeffrey","Jeffrey","Geoff"]
val test5 = get_substitutions2( [["Fred","Frederick"], ["Elizabeth","Betty"], ["Freddie","Fred","F"]], "Fred") = ["F","Freddie","Frederick"]
val test6 = get_substitutions2([["Fred","Frederick"],["Jeff","Jeffrey"],["Geoff","Jeff","Jeffrey"]], "Jeff") = ["Jeffrey","Geoff","Jeffrey"]
val test7 = card_color (Clubs, Ace) = Black
val test8 = card_color (Spades, Num 10) = Black
val test9 = card_color (Diamonds, King) = Red
val test10 = card_color (Hearts, Queen) = Red
val test11 = card_value (Hearts,Num 5) = 5
val test12 = card_value (Clubs, Ace) = 11
val test13 = card_value (Diamonds, Jack) = 10
