(*1*)

(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)

(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

(* put your solutions for problem 2 here *)

fun all_except_option (x, xs) =
    case xs of
	[] => NONE
      | (x'::xs')  => if same_string(x, x') 
		    then SOME(xs')
		    else case all_except_option(x, xs') of
			     NONE => NONE
			   | SOME res => SOME (x'::res)


fun get_substitutions1 (xss, x) = 
    case xss of
	[] => []
      | (xs::xss')  => case all_except_option(x, xs) of
			   NONE => get_substitutions1(xss', x)
			 | SOME subs  => subs @ get_substitutions1(xss', x)


fun get_substitutions2 (xss, x) =
    let
	fun iter(yss, acc) =
	    case yss of
		[] => acc
	      | ys::yss' => iter(yss', (case all_except_option(x, ys) of
					     NONE =>  acc
					   | SOME subs => acc @ subs))
    in 
	iter(xss, []) 
    end


fun similar_names (xss, person) = 
    let
	val {first=first, middle=middle, last=last} = person
	fun names ns =
	    case ns of
		[] => []
	      | n::ns' => {first=n, middle=middle, last=last} :: names(ns')
    in
	person::names(get_substitutions1(xss, first))
    end


fun card_color (c) = 
    case c of
	(Clubs, _) => Black
      | (Spades,_)  => Black
      | (Diamonds,_) => Red
      | (Hearts,_) => Red 


fun card_value (c) =
    case c of
	(_, Num n) => n
      | (_, Ace)  => 11
      | _ => 10
			 

fun remove_card (cs, c:card, e) =
    case cs of
	[] => raise e
      | c'::cs' => if(c = c')
		   then cs'
		   else c'::remove_card(cs', c, e)
	

fun all_same_color (cs) = 
    case cs of
	[] => true
      | c::[]  => true 
      | (h::n::rest) => card_color(h) = card_color(n) andalso all_same_color(n::rest) 


fun sum_cards (cs) =
    let
	fun iter(xs,sum) =
	    case xs of
		[] => sum
	      | x::xs' => iter(xs', card_value(x) + sum)
    in 
	iter(cs, 0)
    end


fun score (held, goal) =
    let
	val sum = sum_cards(held)
	val pre = if sum > goal then 3 * (sum - goal) else (goal-sum)
    in
	if not(all_same_color(held)) then pre else pre div 2
    end
			   

fun officiate(cards, moves, goal) =
    let
	fun run(cards, held, moves) =
	    case moves of
		[] => score(held, goal)
	      | (Discard card)::ms => run(cards, remove_card(held, card, IllegalMove), ms)
	      | Draw::ms => case cards of
				[] => score(held, goal)
			      | c::cs  => 
				let 
				    val held = c::held
				in 
				    if sum_cards(held) > goal 
				    then score(held, goal)
				    else run(cs, held, ms)
				end  
    in
	run(cards, [], moves)
    end			   

fun replace_aces (cards) =
    case cards of
	[] => []
      | (suit, Ace)::cs => (suit, Num 1) :: replace_aces(cs)
      | card::cs => card :: replace_aces(cs)

fun score_challenge (held, goal) = Int.min(score(held, goal), score(replace_aces(held), goal))
fun officiate_challenge (cards, moves, goal)	= Int.min(officiate(cards, moves, goal), officiate(replace_aces(cards), moves, goal))

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

val res = get_substitutions2([["Fred","Frederick"],["Elizabeth","Betty"],["Freddie","Fred","F"]],"Fred")

(*
val officiate_t =
  [ officiate([], [Draw,Draw,Draw,Draw,Draw],42)=21
  ,( officiate([(Clubs,Jack),(Spades,Num(8))], [Draw,Discard(Hearts,Jack)] ,42) handle IllegalMove => 9999 ) = 9999
  , officiate([(Clubs,Ace),(Spades,Ace),(Clubs,Ace),(Spades,Ace)], [Draw,Draw,Draw,Draw,Draw],42)=3
  , officiate([(Clubs,Ace),(Spades,Ace),(Clubs,Ace),(Spades,Ace)], [Draw,Draw,Draw,Draw,Draw],30)=4
  , officiate([(Clubs,Ace),(Spades,Ace),(Clubs,Ace),(Spades,Ace)], [Draw,Draw,Draw,Draw,Draw],22)=16
  , officiate([(Clubs,Ace),(Spades,Ace),(Clubs,Ace),(Spades,Ace)], [Draw,Draw,Draw,Draw,Draw],100)=28
  , officiate([(Clubs,Ace),(Spades,Ace),(Clubs,Ace),(Spades,Ace)], [Draw,Draw,Draw,Draw,Draw],44)=0
  , officiate([(Diamonds,Ace),(Spades,Ace),(Clubs,Ace),(Spades,Ace)], [Draw,Draw,Draw,Draw,Draw],30)=9
  , officiate([(Clubs,Ace),(Hearts,Ace),(Clubs,Ace),(Spades,Ace)], [Draw,Draw,Draw,Draw,Draw],22)=33
  , officiate([(Clubs,Ace),(Spades,Ace),(Diamonds,Ace),(Spades,Ace)], [Draw,Draw,Draw,Draw,Draw],100)=56
  , officiate([(Clubs,Ace),(Spades,Ace),(Clubs,Ace),(Hearts,Ace)], [Draw,Draw,Draw,Draw,Draw],44)=0
  ,  officiate([(Clubs,Ace),(Diamonds,Ace),(Clubs,Ace),(Hearts,Ace)], [Draw,Draw],30)=8
  ,  officiate([(Clubs,Ace),(Diamonds,Ace),(Clubs,Ace),(Hearts,Ace)], [Draw,Draw],22)=0
  ,  officiate([(Clubs,Ace),(Diamonds,Ace),(Clubs,Ace),(Hearts,Ace)], [Draw,Draw],11)=33
  ,  officiate([(Clubs,Queen),(Diamonds,Ace),(Clubs,Ace),(Hearts,Ace)], [Draw,Discard(Clubs,Queen),Draw,Draw],11)=33
  ,  officiate([(Clubs,Queen),(Diamonds,Ace),(Clubs,Ace),(Hearts,Ace)], [Draw,Discard(Clubs,Queen),Draw,Draw],22)=0
  ,  officiate([(Clubs,Queen),(Diamonds,Ace),(Clubs,Ace),(Hearts,Ace)], [Draw,Discard(Clubs,Queen),Draw,Draw],30)=8
  ,  officiate([(Clubs,Queen),(Diamonds,Ace),(Hearts,Ace),(Diamonds,Ace)], [Draw,Discard(Clubs,Queen),Draw,Draw],11)=16
  ,  officiate([(Clubs,Queen),(Diamonds,Ace),(Hearts,Ace),(Diamonds,Ace)], [Draw,Discard(Clubs,Queen),Draw,Draw],22)=0
  ,  officiate([(Clubs,Queen),(Diamonds,Ace),(Hearts,Ace),(Diamonds,Ace)], [Draw,Discard(Clubs,Queen),Draw,Draw],30)=4
  ,  officiate([(Clubs,Queen),(Diamonds,Ace),(Hearts,Ace),(Diamonds,Ace)], [Draw,Draw,Discard(Clubs,Queen),Draw],11)=30
  ,  officiate([(Clubs,Queen),(Diamonds,Ace),(Hearts,Ace),(Diamonds,Ace)], [Draw,Draw,Discard(Clubs,Queen),Draw],22)=0
  ,  officiate([(Clubs,Queen),(Diamonds,Ace),(Hearts,Ace),(Diamonds,Ace)], [Draw,Draw,Discard(Clubs,Queen),Draw],30)=4
  ]

val res = List.all (fn x => x = true) officiate_t

val cards1 = [(Clubs,Jack),(Spades,Num(8))]
val cards2 = [(Clubs,Ace),(Spades,Ace),(Clubs,Ace),(Spades,Ace)]
val cards3 = [(Clubs,Ace),(Diamonds,King)]

val score_t =
  [ score(cards3,21)=0
  , score(cards3,25)=4
  , score(cards3,17)=12
  , score(cards2,44)=0
  , score(cards2,48)=2
  , score(cards2,40)=6
  , score([(Clubs,Ace),(Spades,Ace),(Clubs,Ace),(Spades,Ace)],42)=3
]

val test09 = [
    sum_cards([(Spades,Num 2), (Hearts, Num 10)]) = 12,
    sum_cards([(Hearts, Queen), (Diamonds, King)]) = 20,
    sum_cards([(Clubs, Ace), (Diamonds, Ace)]) = 22,
    sum_cards([]) = 0
]

val test08 = [
    all_same_color([(Spades,Ace),(Clubs,King),(Spades,Queen)]),
    all_same_color([(Diamonds,Ace),(Hearts,King),(Hearts,Queen)]),
    all_same_color([(Spades,Ace)]),
    all_same_color([]),
    not (all_same_color([(Spades,Ace),(Clubs,King),(Hearts,Queen)])),
    not (all_same_color([(Diamonds,Ace),(Hearts,King),(Spades,Queen)]))
] 

val test07 = [
    remove_card([(Spades,Ace),(Hearts,Jack),(Diamonds, Num 10),(Clubs, King)], (Spades, Ace), IllegalMove) = [(Hearts,Jack),(Diamonds, Num 10),(Clubs, King)],
    remove_card([(Spades,Ace),(Hearts,Jack),(Diamonds, Num 10),(Clubs, King)], (Hearts, Jack), IllegalMove) = [(Spades,Ace),(Diamonds, Num 10),(Clubs, King)],
    remove_card([(Spades,Ace),(Hearts,Jack),(Diamonds, Num 10),(Clubs, King)], (Diamonds, Num 10), IllegalMove) = [(Spades,Ace),(Hearts,Jack),(Clubs, King)],
    remove_card([(Spades,Ace),(Hearts,Jack),(Diamonds, Num 10),(Clubs, King)], (Clubs, King), IllegalMove) = [(Spades, Ace), (Hearts,Jack),(Diamonds, Num 10)],
    (remove_card([(Spades,Ace),(Hearts,Jack),(Diamonds, Num 10),(Clubs, King)], (Clubs, Jack), IllegalMove) handle IllegalMove => []) = []
]

val test06 = [
    card_value((Hearts, Num 8)) = 8,
    card_value((Spades, Ace)) = 11,
    card_value((Diamonds, Queen)) = 10
]

val test05 = [
    card_color((Clubs, Ace)) = Black,
    card_color((Spades, Jack)) = Black,
    card_color((Hearts, Num 10)) = Red,
    card_color((Hearts, Queen)) = Red
]	

val test04 = [
    similar_names([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]], {first="Fred", middle="W", last="Smith"}) = [{first="Fred", last="Smith", middle="W"},
            {first="Fredrick", last="Smith", middle="W"},
            {first="Freddie", last="Smith", middle="W"},
            {first="F", last="Smith", middle="W"}]
]
*)

(*
val test03 = [
    get_substitutions2([["Fred","Frederick"],["Elizabeth","Betty"],["Freddie","Fred","F"]],"Fred") = ["Frederick","Freddie","F"],
    get_substitutions2([["Fred","Frederick"],["Jeff","Jeffrey"],["Geoff","Jeff","Jeffrey"]], "Jeff") = ["Jeffrey", "Geoff", "Jeffrey"]
]

val test02 = [
    get_substitutions1([["Fred","Frederick"],["Elizabeth","Betty"],["Freddie","Fred","F"]],"Fred") = ["Frederick","Freddie","F"],
    get_substitutions1([["Fred","Frederick"],["Jeff","Jeffrey"],["Geoff","Jeff","Jeffrey"]], "Jeff") = ["Jeffrey", "Geoff", "Jeffrey"]
]
*)

(*
val test01 = [
    all_except_option("One", ["One","Two","Three"]) = SOME(["Two","Three"]),
    all_except_option("Two", ["One","Two","Three"]) = SOME(["One","Three"]),
    all_except_option("Three", ["One","Two","Three"]) = SOME(["One","Two"]),
    all_except_option("Zero", ["One","Two","Three"]) = NONE
]
*)
