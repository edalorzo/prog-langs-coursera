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


fun check_tests ts =
  List.map (fn t => if t then print "OK\n" else print "FAIL") ts

val all_except_option_t =
  [ all_except_option ("a",[]) = NONE
  , all_except_option ("a",["b"]) = NONE
  , all_except_option ("a",["b","c"]) = NONE
  , all_except_option ("a",["a"]) = SOME []
  , all_except_option ("a",["a","b"]) = SOME ["b"]
  , all_except_option ("a",["b","a","c"]) = SOME ["b","c"]
  ]

val get_substitutions1_t =
  [ get_substitutions1 ([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]],"Fred") = ["Fredrick","Freddie","F"]
  , get_substitutions1 ([[]],"Fred") = []
  , get_substitutions1 ([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]],"Stranger") = []
  , get_substitutions1 ([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]],"Fredrick") = ["Fred"]
  , get_substitutions1 ([["Fred","Fredrick"],["Jeff","Jeffrey"],["Geoff","Jeff","Jeffrey"]],"Jeff") = ["Jeffrey","Geoff","Jeffrey"]
  ]

val get_substitutions2_t =
  [ get_substitutions2 ([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]],"Fred") = ["Fredrick","Freddie","F"]
  , get_substitutions2 ([[]],"Fred") = []
  , get_substitutions2 ([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]],"Stranger") = []
  , get_substitutions2 ([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]],"Fredrick") = ["Fred"]
  , get_substitutions2 ([["Fred","Fredrick"],["Jeff","Jeffrey"],["Geoff","Jeff","Jeffrey"]],"Jeff") = ["Jeffrey","Geoff","Jeffrey"]
  ]


val similar_names_t = let
    val names1 = [["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]]
    val names2 = [["Fred","Fredrick"],["Jeff","Jeffrey"],["Geoff","Jeff","Jeffrey"]]
  in
    [ similar_names (names1, {first="Fred", middle="W", last="Smith"} ) = [{first="Fred", last="Smith", middle="W"}, {first="Fredrick", last="Smith", middle="W"}, {first="Freddie", last="Smith", middle="W"}, {first="F", last="Smith", middle="W"}]
    , similar_names (names2, {first="Jeff", middle="W", last="Smith"} ) = [{first="Jeff", last="Smith", middle="W"}, {first="Jeffrey", last="Smith", middle="W"}, {first="Geoff", last="Smith", middle="W"}, {first="Jeffrey", last="Smith", middle="W"}]
    , similar_names(names1, {first="Jeff", middle="W", last="Smith"} ) = [{first="Jeff", middle="W", last="Smith"}]
    ]
  end

val cards1 = [(Clubs,Jack),(Spades,Num(8))]
val cards2 = [(Clubs,Ace),(Spades,Ace),(Clubs,Ace),(Spades,Ace)]
val cards3 = [(Clubs,Ace),(Diamonds,King)]

val card_color_t =
  [ card_color ((Clubs,Jack)) = Black
  , card_color ((Spades,Jack)) = Black
  , card_color ((Diamonds,Ace)) = Red
  , card_color ((Hearts,Ace)) = Red
  ]

val card_value_t =
  [ card_value((Clubs,Jack))=10
  , card_value((Clubs,Queen))=10
  , card_value((Clubs,King))=10
  , card_value((Clubs,Ace))=11
  , card_value((Clubs,Num(2)))=2
  , card_value((Clubs,Num(3)))=3
  , card_value((Clubs,Num(10)))=10
  ]

val remove_card_t =
  [ remove_card(cards1,(Clubs,Jack),IllegalMove)=[(Spades,Num(8))]
  , remove_card(cards2,(Spades,Ace),IllegalMove)=[(Clubs,Ace),(Clubs,Ace),(Spades,Ace)]
  , remove_card(cards2,(Clubs,Ace),IllegalMove)=[(Spades,Ace),(Clubs,Ace),(Spades,Ace)]
  , remove_card(cards1,(Spades,Num(8)),IllegalMove)=[(Clubs,Jack)]
  , (remove_card(cards2,(Spades,Num(8)),IllegalMove) handle IllegalMove => []) = []
  ]

val all_same_color_t =
[ all_same_color(cards1)=true
, all_same_color(cards2)=true
, all_same_color([(Clubs,Jack),(Spades,Num(8)),(Hearts,King)])=false
, all_same_color([(Clubs,Jack),(Hearts,King),(Spades,Num(8))])=false
, all_same_color([(Hearts,King),(Clubs,Jack),(Spades,Num(8))])=false
, all_same_color(cards3)=false
]

val sum_cards_t =
  [ sum_cards(cards1)=18
  , sum_cards(cards2)=44
  , sum_cards(cards3)=21
  ]

val score_t =
  [ score(cards3,21)=0
  , score(cards3,25)=4
  , score(cards3,17)=12
  , score(cards2,44)=0
  , score(cards2,48)=2
  , score(cards2,40)=6
  , score([(Clubs,Ace),(Spades,Ace),(Clubs,Ace),(Spades,Ace)],42)=3
  ]

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


val all_tests = List.concat
  [ all_except_option_t
  , get_substitutions1_t
  , get_substitutions2_t
  , similar_names_t
  , card_color_t
  , card_value_t
  , remove_card_t
  , all_same_color_t
  , sum_cards_t
  , officiate_t
  ]

val tests = List.all (fn x => x = true) all_tests