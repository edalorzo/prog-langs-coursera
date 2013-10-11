(* Unit Testing *)

val tests01 = [
	is_older( (1978,1,19), (1978,1,20)) = true,
	is_older( (1978,1,19), (1978,2,19)) = true,
	is_older( (1978,1,19), (1979,1,19)) = true,
	is_older( (1978,2,19), (1978,2,19)) = false,
	is_older( (1978,2,19), (1978,2,18)) = false,
	is_older( (1978,2,19), (1978,1,18)) = false,
	is_older( (1978,2,19), (1977,2,18)) = false
]

val tests02 = [
	number_in_month( [(1978,1,19),(1978,2,19),(1995,2,21), (2012,2,29)] , 1 ) = 1,
	number_in_month( [(1978,1,19),(1978,2,19),(1995,2,21), (2012,2,29)] , 2 ) = 3
]

val tests03 = [
	number_in_months( [(1978,1,19),(1978,2,19),(1995,2,21), (2012,2,29)] , [1,2] ) = 4,
	number_in_months( [(1978,1,19),(1978,2,19),(1995,2,21), (2012,2,29)] , [5,6] ) = 0
]

val tests04 = [
	dates_in_month([(1978,1,19),(1978,2,19),(1995,2,21), (2012,2,29)], 1) = [(1978,1,19)],
	dates_in_month([(1978,1,19),(1978,2,19),(1995,2,21), (2012,2,29)], 2) = [(1978,2,19),(1995,2,21),(2012,2,29)],
	dates_in_month([(1978,1,19),(1978,2,19),(1995,2,21), (2012,2,29)], 5) = []
]

val tests05 = [
	dates_in_months([(1978,1,19),(1978,2,19),(1995,2,21), (2012,2,29)], [1,2]) = [(1978,1,19),(1978,2,19),(1995,2,21), (2012,2,29)],
	dates_in_months([(1978,1,19),(1978,2,19),(1995,2,21), (2012,2,29)], [5]) = []
]

val tests06 = [
	get_nth(["one","two","three","four","five","six","seven"], 1) = "one",
	get_nth(["one","two","three","four","five","six","seven"], 4) = "four",
	get_nth(["one","two","three","four","five","six","seven"], 7) = "seven"
]

val tests07 = [
	date_to_string( (1978, 1, 19) ) = "January 19, 1978",
	date_to_string( (2013, 1, 17) ) = "January 17, 2013"
]

val tests08 = [
	number_before_reaching_sum(15, [1,2,3,4,5,6,7,8,9,10]) = 4,
	number_before_reaching_sum(22, [1,2,3,4,5,6,7,8,9,10]) = 6,
	number_before_reaching_sum(15, [31,28,30]) = 0,
	number_before_reaching_sum(22, [2,8,1,7,3,9,4,5]) = 5
]

val tests09 = [
	what_month(17) = 1,
	what_month(31) = 1,
	what_month(35) = 2,
	what_month(365) = 12
]

val tests10 = [
	month_range(29, 34) = [1,1,1,2,2,2]
]

val tests11 = [
	oldest( [(1978,1,19),(2012,12,12),(2013,1,17),(1996,4,25)]) = SOME (1978,1,19),
	oldest([]) = NONE,
	oldest([(~4,2,3),(2,2,3),(5,2,3),(~3,2,3)]) = SOME (~4,2,3),
	oldest([(5,5,2),(5,10,2),(5,2,2),(5,12,2)]) = SOME (5,2,2),
	oldest([(5,12,15),(5,12,10),(5,12,1)]) = SOME (5,12,1)

]

val tests12 = [
	exists(1, [1,2,3,4,5,6]) = true,
	exists(4, [1,2,3,4,5,6]) = true,
	exists(6, [1,2,3,4,5,6]) = true,
	exists(8, [1,2,3,4,5,6]) = false,
	exists(0, [1,2,3,4,5,6]) = false
]

val tests13 = [
	dedup( [1,1,2,2,3,3,4,5,5,5,6,6,6,6,6,7,8,9,0,0]) = [1,2,3,4,5,6,7,8,9,0]
]

val tests14 = [
	number_in_months( [(1978,1,19),(1978,2,19),(1995,2,21), (2012,2,29)] , [1,2,1] ) = 5,
	number_in_months_challenge( [(1978,1,19),(1978,2,19),(1995,2,21), (2012,2,29)] , [1,2,1] ) = 4,
	dates_in_months([(1978,1,19),(1978,2,19),(1995,2,21), (2012,2,29)], [1,1]) = [(1978,1,19),(1978,1,19)],
	dates_in_months_challenge([(1978,1,19),(1978,2,19),(1995,2,21), (2012,2,29)], [1,1]) = [(1978,1,19)],
	reasonable_date((1978,1,19)) = true,
	reasonable_date((1996,2,29)) = true,
	reasonable_date((2012,2,29)) = true,
	reasonable_date((2013,2,29)) = false,
	reasonable_date((0,1,1)) = false,
	reasonable_date((2013,13,1)) = false
]

val tests = tests01 @ tests02 @
			tests03 @ tests04 @
			tests05 @ tests06 @
			tests07 @ tests08 @
			tests09 @ tests10 @
			tests11 @ tests12 @
			tests13 @ tests14

val all_tests = List.all (fn x => x) tests


