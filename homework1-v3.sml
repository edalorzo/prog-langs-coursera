
fun is_older(dt1: (int * int * int), dt2: (int * int * int)) =
    #1 dt1 < #1 dt2 
    orelse (#1 dt1 = #1 dt2 andalso #2 dt1 < #2 dt2) 
    orelse (#1 dt1 = #1 dt2 andalso #2 dt1 = #2 dt2 andalso #3 dt1 < #3 dt2)

fun number_in_month(dates: (int * int * int) list, m: int) =
    if null dates then 0 
    else (if #2 (hd dates) = m then 1 else 0) + number_in_month(tl dates, m) 

fun number_in_months(dates: (int * int * int) list, ms: int list) =
    if null ms then 0
    else number_in_month(dates, hd ms) + number_in_months(dates, tl ms)

fun dates_in_month(dates: (int * int * int) list, m: int) =
    if null dates then []
    else 
	let
	    val this = hd dates 
	in
	    if #2 this = m 
	    then this :: dates_in_month(tl dates, m) 
	    else dates_in_month(tl dates, m) 
	end

fun dates_in_months(ds : (int * int* int) list, ms: int list) =
    if null ms then []
    else dates_in_month(ds, hd ms) @ dates_in_months(ds, tl ms)
			  

fun get_nth(xs: string list, n: int) =
    if(n=1) then hd xs
    else get_nth(tl xs, n-1)


fun date_to_string(dt: (int * int * int)) =
    let
	val months = ["January",
		      "February",
		      "March",
		      "April",
		      "June",
		      "July",
		      "August",
		      "September",
		      "October",
		      "November",
		      "December"] 
    in
	get_nth(months, #2 dt) ^ " " ^ Int.toString(#3 dt) ^ ", " ^ Int.toString(#1 dt)
    end


fun number_before_reaching_sum(sum: int, xs: int list) = 
	if null xs
	then 0
	else 
		let
		    val acc = sum - hd xs
		in
		    if acc <= 0 then 0 else 1 + number_before_reaching_sum(acc, tl xs)
		end
	

fun what_month(n: int) =
    let
	 val months = [31,28,31,30,31,30,31,31,30,31,30,31]
    in
	number_before_reaching_sum(n, months) + 1 
    end


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
