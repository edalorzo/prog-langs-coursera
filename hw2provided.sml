(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)
(* Author: Edwin Dalorzo *)

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

(* 2a. Takes a card and returns its color (spades and clubs are black,
   diamonds and hearts are red). *)
fun card_color(c) = 
	case c of
		(Clubs, _) => Black
	  | (Spades, _) => Black
	  | (Diamonds,_) => Red
	  | (Hearts,_) => Red

(* 2b. Takes a card and returns its value (numbered cards have their
   number as the value, aces are 11, everything else is 10). *)
fun card_value(c) = 
	case c of
		(_,Num n) => n
	  | (_,Ace) => 11
	  | _ => 10

(* 2c. It returns a list that has all the elements of cs except c. *)
fun remove_card(cs: card list, c: card, e: exn) = 
	let 
		fun remove(source: card list, dest: card list) = 
			case source of
				[] => raise e
			  |	(x::xs) => if x=c then dest @ xs else remove(xs, dest @ [x])
	in
		remove(cs, [])
	end

(* 2d. Returns true if all the cards in the list are the same color. *)
fun all_same_color(cs: card list) = 
	case cs of
		  [] => true
	 | _::[] => true
	 | head::(neck::rest) => (card_color(head)=card_color(neck) andalso all_same_color(neck::rest))

(* 2e. Takes a list of cards and returns the sum of their values. *)
fun sum_cards(cs: card list) = 
	let
		fun sum(cards: card list, acc: int) = 
			case cards of
				[] => acc
			  | x::xs => sum(xs, acc + card_value(x))
	in
		sum(cs, 0)
	end

(* 2f. Computes the score of a list of cards held by the player. *)
fun score(cs: card list, obj: int) =
	let 
		val sum = sum_cards(cs)
		val prelim = if sum > obj then 3 * (sum-obj) else (obj-sum)
		val score = if all_same_color(cs) then prelim div 2 else prelim
	in
		score
	end

(* 2g. Runs the game. *)
fun officiate(cs: card list, ms: move list, obj: int) = 
	let
		fun discard(c: card, held: card list) = remove_card(held, c, IllegalMove)

		fun iterate(cards: card list, moves: move list, held: card list) = 
			let
				val score = score(held, obj)
			in
				if score > obj 
				then score
				else
					case moves of 
						[] => score
					  | (m::ms) => case m of 
					  				Discard c => iterate(cards, ms, discard(c, held))
					  			  | Draw => case cards of
					  						 [] => score
					  					   | (c::cs) => iterate(cs,ms,c::held)
			end
	in
		iterate(cs, ms, [])
	end

(* 3a1. Same as score but Aces can be worth 1 or 11. The minumum score is chosen. *)
fun score_challenge(cs: card list, obj: int) =
	let
		fun replace_as(cs: card list) =
			case cs of
				[] => []
			  |	(c::cs) => (case c of 
				 			(suit,Ace) => (suit, Num 1)
				 		  | _ => c)::replace_as(cs)
	in
		Int.min(score(cs, obj), score(replace_as(cs), obj))
	end


(* 3a2. Same as score but Aces can be worth 1 or 11. The minumum score is chosen. *)
fun officiate_challenge(cs: card list, ms: move list, obj: int) =
	let
		fun replace_as(cs: card list) =
			case cs of
				[] => []
			  |	(c::cs) => (case c of 
				 			(suit,Ace) => (suit, Num 1)
				 		  | _ => c)::replace_as(cs)
	in
		officiate(replace_as(cs), ms, obj)
	end

(* 3b. Creates an optimal move list. *)
fun careful_player(cs: card list, obj: int) = 
	let
		fun draw(cs: card list, held: card list, ms: move list) =
			case cs of
				[] => (cs, held, ms)
			  | (c::cs') => (cs',c::held, Draw::ms)

		fun find_discard(c: card, held: card list, tested: card list) =
			case held of
				[] => NONE
			  | (h::hs) => 	if score(c::(hs @ tested), obj) = 0 
			  				then SOME h 
			  				else find_discard(c, hs, h::tested)

		fun discard(cs: card list, held: card list, ms:move list) = 
			case cs of
				[] => ms
			  | (c::cs') => case find_discard(c, held, []) of
			  					NONE => ms
			  				  | SOME h => ms @ [(Discard h), Draw]

		fun run(cs: card list, held: card list, ms: move list) = 
			let
				val sum = sum_cards(held)
			in
				if (obj-10) > sum 
				then 
					let
						val (cs', held', ms') = draw(cs, held, ms)
						val score = score(held', obj)
					in
						if score=0 then ms' else run(cs', held', ms')
					end
				else discard(cs, held, ms)
			end
	in
		run(cs, [], [])
	end