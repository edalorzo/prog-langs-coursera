
val tests1 =  
	[
		only_capitals [] = [],
		only_capitals ["one","two","three"] = [],
		only_capitals ["One","two","Three"] = ["One","Three"]
	]

val tests2 =  
	[
		longest_string1 [] = "",
		longest_string1 ["one"] = "one",
		longest_string1 ["one","two"] = "one",
		longest_string1 ["one","two","three"] = "three",
		longest_string1 ["one","two","three","fives"] = "three"
	]	

val tests3 = 
	[
		longest_string2 [] = "",
		longest_string2 ["one"] = "one",
		longest_string2 ["one","two"] = "two",
		longest_string2 ["one","two","three"] = "three",
		longest_string2 ["one","two","three","fives"] = "fives"
	]	

val tests4 = 
	[
		longest_string_helper (fn (x,y) => x > y) [] = "",
		longest_string_helper (fn (x,y) => x > y) ["one"] = "one",
		longest_string_helper (fn (x,y) => x > y) ["one","two"] = "one",
		longest_string_helper (fn (x,y) => x > y) ["one","two","three"] = "three",
		longest_string_helper (fn (x,y) => x > y) ["one","two","three","fives"] = "three"
	]		

val tests5 =  
	[
		longest_string3 [] = "",
		longest_string3 ["one"] = "one",
		longest_string3 ["one","two"] = "one",
		longest_string3 ["one","two","three"] = "three",
		longest_string3 ["one","two","three","fives"] = "three"
	]	

val tests6 = 
	[
		longest_string4 [] = "",
		longest_string4 ["one"] = "one",
		longest_string4 ["one","two"] = "two",
		longest_string4 ["one","two","three"] = "three",
		longest_string4 ["one","two","three","fives"] = "fives"
	]		

val tests7 = 
	[
		longest_capitalized [] = "",
		longest_capitalized ["one"] = "",
		longest_capitalized ["one","two"] = "",
		longest_capitalized ["one","Two","three"] = "Two",
		longest_capitalized ["one","two","Three","Fives"] = "Three"
	]			

val tests8 = 
	[
		rev_string "" = "",
		rev_string "one" = "eno",
		rev_string "rotor" = "rotor"
	]				

val tests9 = 
	[
		((first_answer (fn n => if n mod 2 = 0 then SOME n else NONE) []) handle NoAnswer => 0) = 0,
		((first_answer (fn n => if n mod 2 = 0 then SOME n else NONE) [3,5,7]) handle NoAnswer => 0) = 0,
		((first_answer (fn n => if n mod 2 = 0 then SOME n else NONE) [2]) handle NoAnswer => 0) = 2,
		((first_answer (fn n => if n mod 2 = 0 then SOME n else NONE) [2,4]) handle NoAnswer => 0) = 2
	]					

(*Converts a string of numbers into a option list of numbers (i.e. "123" into SOME [1,2,3] and "123a" into NONE*)
val q8 = (fn xs => SOME (map valOf xs) handle Option => NONE) o map Int.fromString o map Char.toString o explode
val tests10 = 
	[
		all_answers q8 [] = SOME [],
		all_answers q8 ["123"] = SOME [1,2,3],
		(*Notice that this test can fail to other depending on how they use the @ operator in the function*)
		all_answers q8 ["123","456"] = SOME [4,5,6,1,2,3],
		all_answers q8 ["123a"] = NONE,
		(*these two are really weird cases*)
		all_answers (fn x => SOME [x]) ([]: int list) = SOME [],
		all_answers (fn x => NONE:int list option) ([]: int list) = SOME []
	]						

val tests11 = 
	[
		
		count_wildcards(UnitP) = 0,
		count_wildcards(Wildcard) = 1,
		count_wildcards(ConstructorP("Test", Wildcard)) = 1,
		count_wildcards(TupleP([Wildcard, Wildcard, Wildcard])) = 3,
		count_wildcards(ConstructorP("Test", TupleP([Wildcard, Wildcard]))) = 2,
		count_wildcards(ConstructorP("Test", TupleP([Wildcard, Variable "x", Wildcard, Variable "y"]))) = 2,
		count_wildcards(ConstructorP("Test", TupleP([ConstructorP("More",Wildcard),ConstructorP("EvenMore",Wildcard)]))) = 2,
		count_wildcards(TupleP([TupleP([ConstP 37, Wildcard, ConstructorP("Test", TupleP([Wildcard, ConstP 10, TupleP([Wildcard, Wildcard])]))])])) = 4
	]

val tests12 = 
	[
		count_wild_and_variable_lengths(UnitP) = 0,
		count_wild_and_variable_lengths(Variable "x") = 1,
		count_wild_and_variable_lengths(ConstructorP("Test", TupleP([Wildcard,Variable "one"]))) = 4,
		count_wild_and_variable_lengths(ConstructorP("Test", TupleP([ConstructorP("More",Wildcard),ConstructorP("EvenMore",Variable "five")]))) = 5,
		count_wild_and_variable_lengths(TupleP([TupleP([ConstP 37, Wildcard, ConstructorP("Test", TupleP([Variable "x", ConstP 10, TupleP([Wildcard, Variable "y"])]))])])) = 4
	]

val tests13 = 
	[
		count_some_var("x", UnitP) = 0,
		count_some_var("x", Variable "x") = 1,
		count_some_var("x", ConstructorP("Test",Variable "x")) = 1,
		count_some_var("x", TupleP([Wildcard, UnitP, ConstP 10, Variable "y", Variable "x"])) = 1,
		count_some_var("x", TupleP([ConstructorP("Test",TupleP([Variable "x", Variable "y", Variable "z"])), Variable "x"])) = 2
	]

val tests14 = 
	[
		match (Const 10, Wildcard) = SOME [],
		match (Unit, Wildcard) = SOME [],
		match (Constructor("Test", Unit), Wildcard) = SOME [],
		match (Tuple [Unit, Const 10], Wildcard) = SOME [],
		match (Unit, UnitP) = SOME [],
		match (Const 10, ConstP 10) = SOME [],
		match (Const 10, ConstP 20) = NONE,
		match (Const 10, Variable "x") = SOME [("x",Const 10)],
		match (Constructor("Test", Const 35), ConstructorP("Test", Variable "y")) = SOME [("y",Const 35)],
		match (Constructor("Test", Const 35), ConstructorP("Fail", Variable "y")) = NONE,
		(*Tuples of unequal size should not match*)
		match (Tuple([Const 1, Const 2]), TupleP([Variable "x", Variable "y", Variable "z"])) = NONE,
		match (
				Tuple [Const 1, Const 2, Const 3, Const 4], 
				TupleP [Variable "w",Variable "x", Variable "y", Variable "z"]
			  ) = SOME [("z",Const 4),("y",Const 3),("x",Const 2),("w",Const 1)],
		match (
				Tuple [Const 1, Const 2, Const 3, Const 4], 
				TupleP [ConstP 1,Variable "x", ConstP 3, Variable "z"]
			  ) = SOME [("z",Const 4),("x",Const 2)],
		match (
				Constructor("A", Tuple([Unit, Const 10, Const 20, Tuple([Unit, Constructor("B", Const 30)])])), 
				ConstructorP("A", TupleP([UnitP, Variable "x", ConstP 20, TupleP([UnitP, ConstructorP("B", Variable "y")])]))
			  ) = SOME [("y", Const 30), ("x", Const 10)]
	]

val tests15 = 
	[
		first_match (Const 10, [ConstP 10, Variable "x"]) = SOME [],
		first_match (Const 10, [Variable "x", Variable "y"]) = SOME [("x",Const 10)],
		first_match (Const 10, [UnitP, ConstructorP("Test", ConstP 10), Wildcard, Variable "y", ConstP 10]) = SOME [],
		first_match (Const 10, [UnitP, ConstructorP("Test", ConstP 10), Variable "y", Wildcard, ConstP 10]) = SOME [("y", Const 10)],
		first_match (Const 10, [UnitP]) = NONE
	]

val types = [
				("Red","color",UnitT),
				("Blue","color",UnitT),
				("SUV","car",UnitT),
				("Sedan","car",Datatype "color"),
				("Truck","car", TupleT[Datatype "color", Datatype "color"]),
				(*polymorphic list*)
				("Empty","list",UnitT),
				("List","list", TupleT[Anything, Datatype "list"]),
				(*integer list*)
				("Vacio","lista", UnitT),
				("Lista","lista", TupleT[IntT,Datatype "lista"])
			]
val tests16 = 
	[
		typecheck_patterns(types, [Wildcard, Wildcard]) = SOME Anything,
		typecheck_patterns(types, [Wildcard, Variable "x"]) = SOME Anything,
		typecheck_patterns(types, [Wildcard, ConstP 10]) = SOME IntT,
		typecheck_patterns(types, [Wildcard, UnitP]) = SOME Anything,
		typecheck_patterns(types, [Wildcard, TupleP[Wildcard, Variable "x"]]) = SOME(TupleT[Anything, Anything]),
		typecheck_patterns(types, [Wildcard, ConstructorP("Red",UnitP)]) = SOME(Datatype("color")),
		typecheck_patterns(types, [Variable "x", Wildcard]) = SOME Anything,
		typecheck_patterns(types, [Variable "x", ConstP 10]) = SOME IntT,
		typecheck_patterns(types, [Variable "x", Variable "y"]) = SOME Anything,
		typecheck_patterns(types, [ConstP 10, Wildcard]) = SOME IntT,
		typecheck_patterns(types, [ConstP 10, Variable "x"]) = SOME IntT,
		typecheck_patterns(types, [ConstP 10, ConstP 11]) = SOME IntT,
		typecheck_patterns(types, [UnitP, Wildcard]) = SOME Anything,
		typecheck_patterns(types, [TupleP[Variable "x", ConstP 10], Wildcard]) = SOME(TupleT[Anything, IntT]),
		typecheck_patterns(types, [ConstructorP("Red",UnitP),ConstructorP("Blue",UnitP), Wildcard]) = SOME(Datatype("color")),
		typecheck_patterns(types, [Variable "x", UnitP]) = NONE,
		typecheck_patterns(types, [ConstructorP("Red",ConstP 10)]) = NONE,
		typecheck_patterns(types, [ConstructorP("Red",ConstP 10)]) = NONE,
		typecheck_patterns(types, [ConstructorP("Sedan",ConstructorP("Red",UnitP)),ConstructorP("Truck",TupleP[ConstructorP("Red",UnitP),ConstructorP("Blue",UnitP)])]) = SOME(Datatype "car"),
		typecheck_patterns(types, [TupleP[ConstructorP("Sedan",ConstructorP("Red",UnitP)), ConstP 10], TupleP [ConstructorP("Truck", TupleP[ ConstructorP("Red",UnitP), ConstructorP("Blue",UnitP) ] ),Variable "x"]]) = SOME(TupleT[Datatype "car",IntT]),
		typecheck_patterns(types, [TupleP[Variable "x", Variable "x"],TupleP[Wildcard, Wildcard]]) = SOME(TupleT[Anything, Anything]),
		typecheck_patterns(types, [TupleP[Wildcard, Wildcard],TupleP[Wildcard, TupleP[Wildcard, Wildcard]]]) = SOME(TupleT[Anything, TupleT[Anything,Anything]]),
		typecheck_patterns(types, [Variable "x", ConstructorP("Red",UnitP)]) = SOME(Datatype "color"),
		typecheck_patterns(types, [Wildcard, ConstructorP("SUV",UnitP), ConstructorP("Truck", TupleP([ConstructorP("Red",UnitP), ConstructorP("Blue",UnitP)]))]) = SOME(Datatype "car"),
		typecheck_patterns(types, [ConstructorP("Empty",UnitP), ConstructorP("List",TupleP[Variable "x", ConstructorP("Empty",UnitP)])]) = SOME(Datatype "list"),
		typecheck_patterns(types, [Wildcard, ConstructorP("Empty",UnitP), ConstructorP("List",TupleP[Variable "x", ConstructorP("Empty",UnitP)])]) = SOME(Datatype "list"),
		typecheck_patterns(types, [ConstructorP("Empty",UnitP), ConstructorP("List",TupleP[ConstP 10, ConstructorP("Empty",UnitP)])]) = SOME(Datatype "list"),
		typecheck_patterns(types, [Wildcard, ConstructorP("Empty",UnitP), ConstructorP("List",TupleP[ConstP 10, ConstructorP("Empty",UnitP)])]) = SOME(Datatype "list"),
		typecheck_patterns(types, [ConstructorP("Vacio",UnitP), ConstructorP("Lista",TupleP([Variable "x", ConstructorP("Vacio",UnitP)]))]) = SOME(Datatype "lista")
	]

val tests17 = [
	not(check_pat(TupleP[Variable "x", Variable "x"])),
	check_pat(TupleP[Variable "x", ConstructorP("Wild",Wildcard)]),
	not(check_pat(TupleP[TupleP [Variable "x", ConstructorP("Wild", Wildcard)],Variable "x"])),
	check_pat(TupleP[TupleP[Variable "x",ConstructorP("Wild", Wildcard)], Wildcard]),
	not(check_pat(TupleP[TupleP[TupleP[Variable "x", ConstructorP("Wild", Wildcard)],Wildcard],Variable "x"])),
	check_pat(ConstructorP("Egg",ConstructorP("Egg", ConstP 4))),
	check_pat(TupleP[ConstP 17, Wildcard, ConstP 4, ConstructorP("Egg", ConstP 4), ConstructorP("Egg", ConstructorP("Egg", ConstP 4))]),
	check_pat(TupleP[Wildcard,Wildcard]),
	check_pat(TupleP[ConstP 17,ConstP 4])
]			



val tests = tests1 @ tests2 @
			tests3 @ tests4 @
			tests5 @ tests6 @
			tests7 @ tests8 @
			tests9 @ tests10 @
			tests11 @ tests12 @
			tests13 @ tests14 @ 
			tests15 @ tests16 @ 
			tests17

val all_tests = List.all (fn x => x) tests
