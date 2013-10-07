val test01 = get_year( (1978,1,19) ) = 1978
val test02 = get_month( (1978,12,19) ) = 12
val test03 = get_day( (1978,1,19) ) = 19
val test04 = is_older( (1978,1,19), (1978,1,20)) = true
val test05 = is_older( (1978,1,19), (1978,2,19)) = true
val test06 = is_older( (1978,1,19), (1979,1,19)) = true
val test07 = is_older( (1978,2,19), (1978,2,19)) = false
val test08 = is_older( (1978,2,19), (1978,2,18)) = false
val test09 = is_older( (1978,2,19), (1978,1,18)) = false
val test10 = is_older( (1978,2,19), (1977,2,18)) = false
val test11 = number_in_month( [(1978,1,19),(1978,2,19),(1995,2,21), (2012,2,29)] , 1 ) = 1
val test12 = number_in_month( [(1978,1,19),(1978,2,19),(1995,2,21), (2012,2,29)] , 2 ) = 3
val test13 = number_in_months( [(1978,1,19),(1978,2,19),(1995,2,21), (2012,2,29)] , [1,2] ) = 4
val test14 = number_in_months( [(1978,1,19),(1978,2,19),(1995,2,21), (2012,2,29)] , [5,6] ) = 0
val test15 = dates_in_month([(1978,1,19),(1978,2,19),(1995,2,21), (2012,2,29)], 1) = [(1978,1,19)]
val test16 = dates_in_month([(1978,1,19),(1978,2,19),(1995,2,21), (2012,2,29)], 2) = [(1978,2,19),(1995,2,21),(2012,2,29)]
val test17 = dates_in_month([(1978,1,19),(1978,2,19),(1995,2,21), (2012,2,29)], 5) = []
val test18 = dates_in_months([(1978,1,19),(1978,2,19),(1995,2,21), (2012,2,29)], [1,2]) = [(1978,1,19),(1978,2,19),(1995,2,21), (2012,2,29)]
val test19 = dates_in_months([(1978,1,19),(1978,2,19),(1995,2,21), (2012,2,29)], [5]) = []
val test20 = get_nth(["one","two","three","four","five","six","seven"], 1) = "one"
val test21 = get_nth(["one","two","three","four","five","six","seven"], 4) = "four"
val test22 = get_nth(["one","two","three","four","five","six","seven"], 7) = "seven"
val test23 = date_to_string( (1978, 1, 19) ) = "January 19, 1978"
val test24 = date_to_string( (2013, 1, 17) ) = "January 17, 2013"
val test25 = number_before_reaching_sum(15, [1,2,3,4,5,6,7,8,9,10]) = 4
val test26 = number_before_reaching_sum(15, [31,28,30]) = 0
val test27 = what_month(17) = 1
val test28 = what_month(31) = 1
val test29 = what_month(35) = 2
val test30 = what_month(365) = 12
val test31 = month_range(29, 34) = [1,1,1,2,2,2]
val test32 = oldest( [(1978,1,19),(2012,12,12),(2013,1,17),(1996,4,25)]) = SOME (2013,1,17)
val test33 = oldest([]) = NONE
val test34 = elem([1,2,3,4,5,6], 1) = true
val test35 = elem([1,2,3,4,5,6], 4) = true
val test36 = elem([1,2,3,4,5,6], 6) = true
val test37 = elem([1,2,3,4,5,6], 8) = false
val test38 = elem([1,2,3,4,5,6], 0) = false
val test39 = remove_duplicates( [1,1,2,2,3,3,4,5,5,5,6,6,6,6,6,7,8,9,0,0]) = [1,2,3,4,5,6,7,8,9,0]
val test40 = number_in_months( [(1978,1,19),(1978,2,19),(1995,2,21), (2012,2,29)] , [1,2,1] ) = 5
val test41 = number_in_months_challenge( [(1978,1,19),(1978,2,19),(1995,2,21), (2012,2,29)] , [1,2,1] ) = 4
val test42 = dates_in_months([(1978,1,19),(1978,2,19),(1995,2,21), (2012,2,29)], [1,1]) = [(1978,1,19),(1978,1,19)]
val test43 = dates_in_months_challenge([(1978,1,19),(1978,2,19),(1995,2,21), (2012,2,29)], [1,1]) = [(1978,1,19)]
val test44 = reasonable_date((1978,1,19)) = true
val test45 = reasonable_date((1996,2,29)) = true
val test46 = reasonable_date((2012,2,29)) = true
val test47 = reasonable_date((2013,2,29)) = false
val test48 = reasonable_date((0,1,1)) = false
val test49 = reasonable_date((2013,13,1)) = false