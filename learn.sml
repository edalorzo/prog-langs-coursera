type name = {first : string, middle : string, last : string}
fun full_name name (person: name) = #first person ^ " " ^ #middle person ^ " " ^ #last person

datatype Boolean = True | False
(*
fun myand(True, x)  = x
  | myand(False,_) = False
*)

fun get_true() = (print "get_true\n"; True)
fun get_false() = (print "get_false\n"; False)

fun myand(left: unit -> Boolean, right: unit -> Boolean) = 
	case left() of
		True => right()
	  | False => False

fun factorial(xs:int list, acc: int) =
	if null xs then acc
	else factorial(tl xs, acc * (hd xs))



fun is_older(date1 : int*int*int, date2 : int*int*int) =
	if (#1 date1) = (#1 date2) andalso (#2 date1) = (#2 date2) andalso (#3 date1) = (#3 date2)
	then false
	else if(#1 date1) < (#1 date2)
	then true
	else if(#2 date1) < (#2 date2)
	then true
	else if(#3 date1) < (#3 date2)
	then true
	else false
	
	
fun oldest(xs:(int*int*int) list) =
	if null xs 
		then NONE
	else if is_older(hd xs, hd(tl xs))
		 then hd xs
		 else SOME (oldest(tl(tl xs), (hd (tl xs))))	