fun is_older (date1 : int*int*int, date2: int*int*int) = 
  let
    val y1 = #1 date1
    val m1 = #2 date1
    val d1 = #3 date1
    val y2 = #1 date2
    val m2 = #2 date2
    val d2 = #3 date2
  in
    y1 < y2 orelse 
    (y1 = y2 andalso 
        (m1 < m2 orelse
        (m1 = m2 andalso d1 < d2)))
  end

fun number_in_month (dates : (int*int*int) list, month : int) =
  if null dates
  then 0
  else
    let
      val num = number_in_month(tl dates, month)
    in
      if #2 (hd dates) = month then 1 + num else num
    end

fun number_in_months (dates : (int*int*int) list, months : int list) =
  if null dates orelse null months
  then 0
  else
    number_in_month(dates, hd months) + number_in_months(dates, tl months)

fun dates_in_month (dates : (int*int*int) list, month : int) = 
  if null dates
  then []
  else
    let
      val taildates = dates_in_month(tl dates, month)
    in
      if #2 (hd dates) = month
      then (hd dates) :: taildates
      else taildates
    end

fun dates_in_months(dates : (int*int*int) list, months : int list) = 
  if null dates orelse null months
  then []
  else
    dates_in_month(dates, hd months) @ dates_in_months(dates, tl months)

fun get_nth (strs : string list, num : int) = 
  if num > 1
  then get_nth(tl strs, num - 1)
  else hd strs

fun date_to_string (date : int*int*int) =
  let
    val month_words = ["January", "February", "March", "April",
                       "May", "June", "July", "August",
                       "September", "October", "November", "December"]
    val year  = Int.toString(#1 date)
    val month = get_nth(month_words, #2 date)
    val day   = Int.toString(#3 date)
  in
    month ^ " " ^ day ^ ", " ^ year
  end

fun number_before_reaching_sum (sum : int, alist : int list) =
  if null alist orelse sum <= hd alist
  then 0
  else
     1 + number_before_reaching_sum(sum - (hd alist), tl alist)

fun what_month (day_of_year : int) =
    1 + number_before_reaching_sum(day_of_year,
        [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31])

fun month_range (day1 : int, day2 : int) =
  if day1 > day2
  then []
  else
    what_month(day1) :: month_range(day1+1, day2)

fun oldest (dates : (int*int*int) list) =
  if null dates
  then NONE
  else if null (tl dates)
       then SOME (hd dates)
       else
         let
           fun older(date1 : int*int*int , date2 : int*int*int) =
             if is_older(date1, date2) then date1 else date2
         in
           SOME (older(hd dates, valOf(oldest(tl dates))))
         end

(* helper: remove duplicates from an int list *)
fun rm_dup (alist : int list) =
  if null alist
  then []
  else
    let
      val tl_lst = rm_dup(tl alist)
      fun in_lst (num : int, lst: int list) =
        if null lst then false else num = hd lst orelse in_lst(num, tl lst)
    in
      if in_lst(hd alist, tl_lst) then tl_lst else hd alist :: tl_lst
    end

fun number_in_months_challenge (dates : (int*int*int) list, months : int list) =
  number_in_months(dates, rm_dup(months))

fun dates_in_months_challenge (dates : (int*int*int) list, months : int list) =
  dates_in_months(dates, rm_dup(months))

fun reasonable_date(date : int*int*int) =
let
  val y = #1 date
  val m = #2 date
  val d = #3 date
  val max_days_per_month = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
  val max_days_per_month_leap = [31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]

  fun leap_year(y : int) = 
    y mod 400 = 0 orelse (y mod 4 = 0 andalso y mod 100 <> 0)

  fun get_nth_int (ints : int list, num : int) = 
    if num > 1
    then get_nth_int(tl ints, num - 1)
    else hd ints
in
  y > 0 andalso
  (m >= 1 andalso m <= 12) andalso
  (d >= 1 andalso
    if leap_year(y)
     then d <= get_nth_int(max_days_per_month_leap, m)
     else d <= get_nth_int(max_days_per_month, m))
end