(* First Assignment *)

(* Bindings 
val is_older = fn : (int * int * int) * (int * int * int) -> bool
val number_in_month = fn : (int * int * int) list * int -> int
val number_in_months = fn : (int * int * int) list * int list -> int
val dates_in_month = fn : (int * int * int) list * int -> (int * int * int) list
val dates_in_months = fn : (int * int * int) list * int list -> (int * int * int) list
val get_nth = fn : string list * int -> string
val date_to_string = fn : int * int * int -> string
val number_before_reaching_sum = fn : int * int list -> int
val what_month = fn : int -> int
val month_range = fn : int * int -> int list
val oldest = fn : (int * int * int) list -> (int * int * int) option
*)

(* function 01 : is_older *)
fun is_older (first_date : int*int*int, second_date : int*int*int) = 
	((#1 first_date) < (#1 second_date))  orelse 
	(((#1 first_date) = (#1 second_date)) andalso ((#2 first_date) < (#2 second_date))) orelse
	(((#1 first_date) = (#1 second_date)) andalso ((#2 first_date) = (#2 second_date)) andalso ((#3 first_date) < (#3 second_date)))

(* function 02 : number_in_month *)						
fun number_in_month( dates : (int*int*int) list , month : int) =
	if null dates then
	0
	else
		if (#2 (hd(dates)) = month) then
			1 + number_in_month( tl dates , month)
		else
			number_in_month( tl(dates), month)

(* function 03 : number_in_months *)
fun number_in_months( dates : (int*int*int) list , months : int list) =
	if null months then
	0
	else
		number_in_month( dates, hd(months)) +  number_in_months( dates, tl(months)) 

(* function 04 : dates_in_month *)
fun dates_in_month( dates : (int*int*int) list, month : int) =
	if null dates then
	[]
	else
		if number_in_month([hd(dates)], month) = 1 then
			hd(dates)::dates_in_month( tl(dates), month)
		else
			dates_in_month( tl(dates), month)

(* function 05 : dates_in_months *)
fun dates_in_months( dates : (int*int*int) list, months : int list) =
	if null dates then
		[]
	else
		if null months then
			[]
		else
			dates_in_month( dates , hd(months)) @ dates_in_months( dates , tl(months))

(* function 06 : get_nth *)
fun get_nth( stringlist : string list, index : int ) =
	if null stringlist then
		""
	else
		if index = 1 then
			hd(stringlist)
		else
			get_nth(tl(stringlist),index-1)

(* function 07 : date_to_string *)
fun date_to_string( date : int*int*int ) =
	get_nth(["January","February","March","April","May","June","July","August","September","October","November","December"], #2 date) ^
	" " ^ Int.toString(#3 date) ^ ", " ^ Int.toString(#1 date)
			
(* function 08 : number_before_reaching_sum *)
fun number_before_reaching_sum(sum_to_reach : int, int_list : int list) =
	if null int_list orelse hd int_list >= sum_to_reach then
	0
	else
		if null [tl(int_list)] then
			0 
		else 
			if hd(int_list) + hd(tl(int_list)) >= sum_to_reach then
				1
			else
				1 + number_before_reaching_sum(sum_to_reach - hd(int_list) , tl(int_list)) 

(* function 09 : what_month *)
fun what_month(day_of_year : int) =
	if day_of_year <= hd([31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]) then
		1
	else
		number_before_reaching_sum( day_of_year , [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31])  + 1
				
(* function 10 : month_range *)
fun month_range(day1 : int, day2 : int) =
	if day1 > day2 then
		[]
	else
		what_month(day1):: month_range(day1+1, day2)
		
(* function 11 : oldest *)
fun oldest( list_of_dates : (int*int*int) list ) =
	if null list_of_dates then
		NONE
	else
	let val option_answer = oldest(tl(list_of_dates))
	in if isSome option_answer andalso is_older(valOf option_answer, hd(list_of_dates))
		then option_answer
		else SOME (hd(list_of_dates))
	end

(* Here comes the challenge !!! *)
			
(* function 12-1 : number_in_months_challenge *)
fun number_in_months_challenge( dates : (int*int*int) list , months : int list) =
	if null months then
	0
	else
	let
		(* in_list return true in the int_value exists in int_list list *)
		fun in_list( int_list : int list, int_value : int) =
			if null int_list then
				false
			else
				if int_value = hd(int_list) then
					true
				else 
					in_list(tl(int_list), int_value)
		(* Remove duplicate integers in int_list *)
		fun remove_duplicates(int_list : int list) =
			if null int_list then
				[]
			else
				if in_list(tl(int_list), hd(int_list)) then
					remove_duplicates(tl(int_list))
				else
					hd(int_list)::remove_duplicates(tl(int_list))
		val months_no_duplicate = remove_duplicates(months)
	in
		(* Just have to follow the hint... *)
		number_in_month( dates, hd(months_no_duplicate)) +  number_in_months_challenge( dates, tl(months_no_duplicate)) 
	end
	
(* function 12-2 : dates_in_months_challenge *)
fun dates_in_months_challenge( dates : (int*int*int) list, months : int list) =
	if null dates then
		[]
	else
		if null months then
			[]
		else
		let
			(* in_list return true in the int_value exists in int_list list *)
			fun in_list( int_list : int list, int_value : int) =
				if null int_list then
					false
				else
					if int_value = hd(int_list) then
						true
					else 
						in_list(tl(int_list), int_value)
			(* Remove duplicate integers in int_list *)
			fun remove_duplicates(int_list : int list) =
				if null int_list then
					[]
				else
					if in_list(tl(int_list), hd(int_list)) then
						remove_duplicates(tl(int_list))
					else
						hd(int_list)::remove_duplicates(tl(int_list))
			val months_no_duplicate = remove_duplicates(months)
		in
			dates_in_month( dates , hd(months_no_duplicate)) @ dates_in_months_challenge( dates , tl(months_no_duplicate))
		end

(* function 13 : reasonable_date *)
fun reasonable_date( date : int*int*int) =
	(* year control & month control *)
	if #1 date <=0 orelse #2 date < 1 orelse #2 date >12 then
		false
	else
		let
			fun get_nth( intlist : int list, index : int ) =
				if null intlist then
					0
				else
					if index = 1 then
						hd(intlist)
					else
						get_nth(tl(intlist),index-1)

			val number_of_days = 	if (#1 date mod 400 = 0 orelse #1 date mod 4 = 0) andalso (#1 date mod 100 <> 0) then
										[31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
									else
										[31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
		in 
			(* day control *)
			if (#3 date > 0) andalso (#3 date <= get_nth(number_of_days, #2 date)) then
				true
			else
				false
		end
			