
fun is_older(d1: int*int*int, d2:int*int*int) = 
	let
	    val y1 = #1 d1
	    val m1 = #2 d1
	    val d1 = #3 d1
	    val y2 = #1 d2
	    val m2 = #2 d2
	    val d2 = #3 d2
	    val x = y1 * 365 + (m1 - 1) * 30 + d1
	    val y = y2 * 365 + (m2 - 1) * 30 + d2
	in
	    x < y
	end

fun number_in_month(ds: (int*int*int) list, month:int) = 
	if null ds
	then 0
	else
		if #2 (hd ds) = month 
		then 1 + number_in_month(tl ds, month)
		else number_in_month(tl ds, month)

fun number_in_months(ds: (int*int*int) list, ms:int list) = 
	if null ms 
	then 0
	else number_in_month(ds, hd ms) + number_in_months(ds, tl ms)


fun dates_in_month(ds: (int*int*int) list, month:int) =
	if null ds
	then []
	else 
		let
		    val current = hd ds
		in
		    if #2 current = month
		    then current :: dates_in_month(tl ds, month)
		    else dates_in_month(tl ds, month)
		end


fun dates_in_months(ds: (int*int*int) list, ms:int list) = 
	if null ms
	then []
	else dates_in_month(ds, hd ms) @ dates_in_months(ds, tl ms)

	
fun get_nth(xs: string list, n: int) = 
	if n = 1
	then hd xs
	else get_nth(tl xs, n-1)


fun date_to_string(d: int*int*int) = 
	let
	    val day = Int.toString(#3 d)
	    val year = Int.toString(#1 d)
	    val months = ["January","February","March","April","May","June",
						   "July","August","September","October","November","December"]
		val month = get_nth(months, #2 d)						   
	in
	    month ^ " " ^ day ^ ", " ^ year
	end

fun number_before_reaching_sum(sum: int, xs: int list) = 
	let
	    fun iter(xs': int list, pos:int, acc: int) =
	    	let
	    	    val acc = (hd xs') + acc
	    	in
	    		if acc >= sum then pos
	    		else iter(tl xs', pos+1, acc)
	    	end
	    	
	in
	    iter(xs, 0, 0)
	end


fun what_month(day: int) =
	let
	    val months = [31,28,31,30,31,30,31,31,30,31,30,31]
	in
	    number_before_reaching_sum(day, months) + 1
	end

fun month_range(day1:int, day2:int) = 
	if day1 > day2 then []
	else what_month(day1) :: month_range(day1+1, day2)


fun oldest(ds: (int*int*int) list)  = 
	let
	    fun iter(ds': (int*int*int) list, oldest: int*int*int) = 
	   		if null ds' 
	   		then oldest 	
	   		else 
	   			let
	   			    val current = hd ds'
	   			in
	   			    if is_older(current, oldest)
	   			    then iter(tl ds', current)
	   			    else iter(tl ds', oldest)
	   			end
	in
	    if null ds then NONE else SOME( iter(tl ds, hd ds) )
	end

(* checks if x exists within xs *)
fun exists(x: int, xs: int list) =
	if null xs 
	then false
	else hd xs = x orelse exists(x, tl xs)

(* removes duplicate elements from xs*)
fun dedup(xs: int list) =
	if null xs
	then []
	else 
		let
		    val head = hd xs
		    val tail = tl xs
		in
		    if exists(head, tail)
		    then dedup(tail)
		    else head::dedup(tail)
		end

fun number_in_months_challenge(ds: (int*int*int) list, ms:int list) = number_in_months(ds, dedup(ms))
fun dates_in_months_challenge(dates: (int * int * int) list, months: int list) = dates_in_months(dates, dedup(months))

fun reasonable_date(date: (int * int * int)) =
	let
		val month_days = [31,28,31,30,31,30,31,31,30,31,30,31]
		val key_day = #3 date
		val key_month = #2 date
		val key_year = #1 date
		fun is_valid_year() = key_year > 0
		fun is_valid_month() = key_month >= 1 andalso key_month <= 12
		fun is_leap_year() = key_year mod 400 = 0 orelse (key_year mod 100 <> 0 andalso key_year mod 4 = 0)
		fun max_month_days()  = 
			let
				fun get_by_index(months: int list, index: int) = 
					if index = key_month then 
						if is_leap_year() andalso key_month = 2 
							then (hd months) + 1
							else (hd months)
					else get_by_index(tl months, index + 1)
				in
					get_by_index(month_days, 1)
				end
		fun is_valid_day() = key_day > 0 andalso key_day <= max_month_days()
	in
		is_valid_year() andalso is_valid_month() andalso is_valid_day()
	end
