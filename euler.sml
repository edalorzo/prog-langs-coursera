(* Euler 17 *)

exception InvalidInput

type Number = (int * int *int *int)

fun to_number n =
	let 
		fun iter x = 
			if x = 0
			then []
			else (x mod 10) :: iter (x div 10)
	in
		case iter(n) of
			[]					=> (0,0,0,0)
		  | (w::[])				=> (0,0,0,w)
		  | (w::x::[])			=> (0,0,x,w)
		  | (w::x::y::[])		=> (0,y,x,w)
		  | (w::x::y::z::[])	=> (z,y,x,w)
		  | _					=> raise InvalidInput
	end

fun to_string n =
	let
		val units_words = ["zero","one","two","three","four","five","six","seven","eight","nine"]
		val tenths_words = ["ten","eleven","twelve","thirteen","fourteen","fifteen","sixteen","seventeen","eighteen","nineteen"]
		val dozens_words = ["ten","twenty","thirty","forty","fifty","sixty","seventy","eighty","ninety"]
		
		val (t,h,d,u) = n
		
		val thousands = if t > 0 then List.nth(units_words,t) ^ " thousand" else ""
		val hundreds =  if h > 0 then List.nth(units_words,h) ^ " hundred" else ""
		val tenths = if d > 0 
					then case (d,u) of
								(1,u) => List.nth(tenths_words,u)
					  		  | (d,_) => List.nth(dozens_words,d-1)
					else ""
		val units = if u > 0 andalso d <> 1
					then List.nth(units_words,u)
					else ""

	in
		thousands 
		^  (if size thousands > 0 andalso size hundreds > 0 then " " else "") ^ 
		hundreds 
		^ (if size hundreds > 0 andalso (size tenths > 0 orelse size units > 0) then " and " else "") ^ 
		tenths 
		^ (if size tenths > 0 andalso size units > 0 then "-" else "")  ^
		units
	end

val trim = implode o List.filter (fn c => not(c=(#" ") orelse c=(#"-"))) o explode
val count_letters = size o trim o to_string o to_number

fun solve(n) = 
	if n = 0
	then 0
	else (count_letters n) + solve(n-1)

val t1 = count_letters 342 = 23
val t2 = count_letters 115 = 20
val t3 = solve(5) = 19
val answer17 = solve(1000)


(* Euler 50 *)
fun divides d n = n mod d = 0
fun even n = divides 2 n

fun is_prime n = 
	let
		fun ldf d n = 
			if divides d n then d
			else if d * d > n then n
			else ldf (d+2) n

		fun ld n = if even n then 2 
				   else ldf 3 n
	in
		if n < 1 then raise InvalidInput
		else if n = 1 then false
		else ld n = n
	end

fun next_prime p = 
	let
		val candidate = p + 1
	in
		if is_prime candidate then candidate
		else next_prime candidate
	end	

fun next_prime_sum last prime sum count treshold = 
	let
		val next = next_prime prime
		val sum = next + sum
	in
		if count > treshold then last
		else if is_prime sum 
		then next_prime_sum (SOME sum) next sum (count+1) treshold
		else next_prime_sum last next sum (count+1) treshold
	end


val r = next_prime_sum NONE 2 2 0 21
