(* Dan Grossman, Coursera PL, HW2 Provided Tests *)

(* These are just two tests for problem 2; you will want more.

   Naturally these tests and your tests will use bindings defined 
   in your solution, in particular the officiate function, 
   so they will not type-check if officiate is not defined.
 *)

fun provided_test1 () = (* correct behavior: raise IllegalMove *)
    let val cards = [(Clubs,Jack),(Spades,Num(8))]
	val moves = [Draw,Discard(Hearts,Jack)]
    in
	officiate(cards,moves,42)
    end

fun provided_test2 () = (* correct behavior: return 3 *)
    let val cards = [(Clubs,Ace),(Spades,Ace),(Clubs,Ace),(Spades,Ace)]
	val moves = [Draw,Draw,Draw,Draw,Draw]
    in
 	officiate(cards,moves,42)
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
