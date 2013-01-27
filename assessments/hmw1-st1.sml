(**#1 **)
fun is_older ( d1: int * int * int, d2: int * int * int ) =
    let
	fun countdays (date: int * int * int ) = 
	    (# 1 date)*365 + (#2 date)*30 + #3 date
    in
	if countdays(d1) < countdays(d2)
	then true
	else false
    end

(**#2 **)
fun number_in_month ( ms: (int * int * int)  list, month : int ) =
    if null ms
    then 0
    else if(#2(hd ms)= month)
    then 1 + number_in_month(tl ms,month)
    else number_in_month(tl ms,month)

(**#3 **)
fun number_in_months ( ms: (int * int * int)  list, months : int list ) =
    if  null months
    then 0
    else number_in_month (ms, hd months) + number_in_months(ms, tl months)

(**#4 **) 
fun dates_in_month (ms: (int * int * int) list, month:int) =
    if null ms
    then []
    else if ( #2(hd ms) = month  ) 
    then (hd ms)::dates_in_month(tl ms, month)
    else dates_in_month( tl ms, month)

(**#5 **)
fun dates_in_months (ms: (int * int * int) list, months:int list) =
    if null months 
    then []
    else dates_in_month(ms, hd months )@dates_in_months(ms, tl months)

(**#6 **)
fun get_nth (strs: string list, n:int) =
    if n = 1 orelse null strs
    then hd strs
    else get_nth(tl strs, n-1)

(**#7 **)
fun date_to_string (date: int * int * int) =
    let
	val months = ["January", "February", "March", "April","May", "June", 
		      "July", "August", "September", "October", "November", "December"]
    in
	get_nth(months, #2 date ) ^ " " ^ Int.toString(#3 date) ^ ", " ^ Int.toString(#1 date) 
    end

(**#8 **)
fun number_before_reaching_sum (sum:int, is:int list) =
    if  hd is >= sum
    then 0
    else 1 + number_before_reaching_sum(sum - hd is, tl is)

(**#9 **)
fun what_month (day: int) =
    let
	val months = [31,28,31,30,31,30,31,31,30,31,30,31]
    in
	1 + number_before_reaching_sum(day,months )
    end

(**#10 **)
fun month_range (day1:int, day2:int) =
    if day1 > day2
    then []
    else what_month(day1)::month_range(day1+1,day2)

(**#11 **)
fun oldest (dates: (int * int * int) list) =
    if null dates
    then NONE
    else
	let
	    fun old (dates: (int * int * int) list) = 
		if null (tl dates)
		then hd dates
		else 
		    let
			val dt_old = old( tl dates)
		    in
			if is_older (hd dates, dt_old)
			then hd dates
			else dt_old
		    end 
	in
	    SOME ( old(dates))
	end
	    
(**#12 **)

fun is_unique (month: int, months: int list ) =
    if null months
    then true
    else if (hd months) = month
    then false
    else is_unique(month, tl months)

fun number_in_months_challenge (ms: (int * int * int) list, months: int list ) =
    if null months
    then 0
    else if is_unique( hd  months,tl months) 
    then number_in_month(ms, hd months) + number_in_months_challenge(ms, tl months)
    else number_in_months_challenge(ms,tl months)

fun dates_in_months_challenge (ms: (int * int * int) list, months:int list) =
    if null months 
    then []
    else if is_unique(hd months, tl months)
    then dates_in_month(ms, hd months )@dates_in_months_challenge(ms, tl months)
    else dates_in_months_challenge(ms, tl months)
								 
(**#13 **)
fun reasonable_date (date: int * int * int) =
    if (#1 date) <=0
    then false
    else if(#2 date <1) orelse (#2 date > 12) 
    then false
    else if(#3 date) <1 orelse (#3 date)>31(**verificar se o dia e condizente com o mes **)
    then false
    else if is_unique(#2 date,[1,3,5,7,8,10,12]) 
    then true
    else if is_unique(#2 date,[4,6,9,11]) andalso (#3 date)<>31 
    then true
    else if (( ((#1 date) mod 400) = 0) orelse ( (( (#1 date) mod 4) = 0) andalso ( not ( ((#1 date) mod 100) = 0))))
    then  if (#3 date<=29) 
	  then true
 	  else false
    else if (#3 date) < 29 
    then true
    else false