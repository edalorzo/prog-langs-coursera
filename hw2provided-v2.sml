(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)

(* 1a. Return NONE if the string is not in the list, else return SOME lst where lst is identical 
to the argument list except the string is not in it. *)
fun all_except_option(item, items) =
	let 
		fun move(xs, ys) = 
			case xs of 
			  [] => NONE
			| (x::xs') => if same_string(x, item) then SOME (ys @ xs') else move(xs',x::ys)
	in
		move(items, [])
	end

(* 1b. The result has all the strings that are in some list in substitutions that also has s, 
	but s itself should not be in the result. *)
fun get_substitutions1(subs, s) = 
	case subs of 
		[] => [] |
		xs::xss => case all_except_option(s,xs) of
					NONE => get_substitutions1(xss, s) |
					SOME ys => ys @ get_substitutions1(xss, s)

(* 1c. Same as the previous one but using a tail-recursive helper function. *)
fun get_substitutions2(subs, s) = 
	let
		fun iter(src, dest) = 
			case src of 
				[] => dest 
			  | xs::xss => case all_except_option(s,xs) of
								NONE => iter(xss, dest)
							 |	SOME ys => iter(xss, dest @ ys)
	in
		iter(subs, [])
	end

(* 1d. The result is all the full names you can produce by substituting for the first name 
   (and only the first name) using substitutions. *)
fun similar_names(subs, name) = 
	let
		val {first,middle,last} = name
	    fun get_alternatives(xs) = 
	    	case xs of
	    		[] => []
	    	  | x::xs' => {first=x,middle=middle,last=last}::get_alternatives(xs')

	in
	    name::get_alternatives(get_substitutions2(subs, first))
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
fun card_color(card) = 
	case card of
		  (Clubs,_) => Black  
		| (Spades,_) => Black 
		| (Diamonds,_) => Red
		| (Hearts,_) => Red


(* 2b. Takes a card and returns its value (numbered cards have their
   number as the value, aces are 11, everything else is 10). *)
fun card_value(card) = 
	case card of
		(_,Ace) => 11
	  | (_,Num(n)) => n
	  | _ => 10

(* 2c. It returns a list that has all the elements of cs except c. *)
fun remove_card(cards, card:card, ex) = 
	case cards of
		[]=> raise ex
	  | c::cs => if card = c
	  			   then cs
	  			   else c::remove_card(cs,card,ex)


(* 2d. Returns true if all the cards in the list are the same color. *)
fun all_same_color(cards) = 
	case cards of
		  [] => true
		| _::[] => true
		| c1::c2::cs => card_color(c1) = card_color(c2) andalso all_same_color(c2::cs)

(* 2e. Takes a list of cards and returns the sum of their values. *)
fun sum_cards(cards) = 
	let
	    fun iter(cs, sum) = 
	    	case cs of
	    		[] => sum
	    	  | x::cs' => iter(cs', sum + card_value(x))
	in
	    iter(cards, 0)
	end

(* 2f. Computes the score of a list of cards held by the player. *)
fun score(cards, goal) = 
	let
	    val sum = sum_cards(cards)
	    val prelim = if sum > goal then 3 * (sum - goal) else (goal-sum)
	    val final_score = if all_same_color(cards) then prelim div 2 else prelim
	in
	    final_score
	end

(* 2g. Runs the game. *)
fun officiate(cards, moves, goal) = 
	let
	    fun play(cards, held, moves) = 
    		if sum_cards(held) > goal
    		then score(held, goal)
    		else
		    	case moves of
		    		[] => score(held, goal)
				  | (Discard(card)::rest) => play(cards, remove_card(held, card, IllegalMove) , rest)
		    	  | (Draw::rest) => 
		    	  		case cards of
		    	  			[] => score(held, goal)
		    	  		  | card::[] => score(card::held, goal)
		    	  		  | card::more => play(more, card::held, rest)
	in
	    play(cards,[], moves)
	end

(* Replaces Aces with Num(1) *)
fun replace_aces(cards) =
	case cards of
		[] => []
	  | (suit,Ace)::cs => (suit, Num(1))::replace_aces(cs)
	  | c::cs => c::replace_aces(cs)	

(* 3a1. Same as score but Aces can be worth 1 or 11. The minumum score is chosen. *)
fun score_challenge(cards, goal) =  Int.min(score(cards,goal), score(replace_aces(cards), goal))

(* 3a2. Same as officiate but Aces can be worth 1 or 11. The minumum score is chosen. *)
fun officiate_challenge(cards, moves, goal)	= Int.min(officiate(cards, moves, goal), officiate(replace_aces(cards), moves, goal))

(* 3b. Creates an optimal move list. *)
fun careful_player(cards, goal) = 
	let
		fun cheat(drawn, pending, reviewed) = 
			case pending of
				[] => NONE
			  | card::tail => if score(reviewed @ (drawn::tail), goal) = 0
			  				  then SOME card
			  				  else cheat(drawn, tail, card::reviewed)

	    fun moves(cards, held) = 
    		if score(held,goal) = 0
    		then []
    		else case cards of
    				[] => []
    			  | c::cs => case cheat(c, held, []) of
    			  			 	SOME(card) => Discard(card)::Draw::[]
    			  			  | NONE => if sum_cards(held) < (goal-10) 
    			  			  			then Draw::moves(cs,c::held) 
    			  			  			else []
	    	
	in
	    moves(cards,[])
	end

val r1 = score([],18)
val r2 = score_challenge([],18)
val r3 = officiate([], [Draw,Draw,Draw,Draw,Draw],42)=21
val r4 = officiate([(Clubs,Ace),(Hearts,Num(8))],[Draw,Draw,Discard((Clubs,Ace)),Discard(Hearts,Num(8)),Draw],50)