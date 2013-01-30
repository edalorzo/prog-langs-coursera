(* Returns the head of a list. *)
fun head(xs) =
	case xs of
		[] => raise List.Empty
	  | (x::_) => x

(* Returns the tail of a list. *)
fun tail(xs) =
	case xs of
		[] => raise List.Empty
	  | (_::xs') => xs'

(* Determines if a list is empty *)
fun null(xs) =
	case xs of 
		[] => true
	   | _ => false

(* Returns the las element of a list. *)
fun last(xs) =
	case xs of
		[] => raise List.Empty
  	 | (x::[]) => x
  	 | (_::xs') => last(xs')

(* Returns all elements but the last one. *)
fun init(xs) = 
	case xs of 
		[] => raise List.Empty
	  | (x::[]) => []
	  | (x::xs') => x::init(xs')


(* Returns an element of a list based on its index. *)
fun nth(xs, i) =
	if i < 0 
	then raise Subscript
	else
		case xs of
			[] => raise Subscript
		  | (x::xs') => if i=0 then x else nth(xs',i-1)

(* Returns the first n elements of a list. *)
fun take(xs, n) =
	if n < 0 
	then raise Subscript
	else 
		case xs of 
			[] => []
		  | (x::xs') => if n > 0 then x::take(xs',n-1) else []

(* Drops n elements from the list and returns the remaining. *)
fun drop(xs, n) =
	if n < 0 then raise Subscript
	else if n = 0 then xs
	else 
		case xs of 
			[] => []
		  | (_::xs') => drop(xs', n-1)

(* Returns the length of a list. *)
fun len(xs) =
	case xs of
		[] => 0
	 | (_::xs') => 1 + len(xs')


(* Reverses the elements of a list. *)
fun reverse(xs) =
	case xs of
		[] => []
	 | (x::xs') => reverse(xs') @ [x]

(* Appends ys to xs. *)
fun append(xs, ys) = 
	case ys of 
		[] => xs
	  | (y::ys') => append(xs @ [y], ys')


(* Determines the sum of a list of elements *)
fun sum(xs) =
	case xs of
		[] => 0
	 | (x::xs') => x + sum(xs')


(* Determines the product of a list of elements *)
fun product(xs) =
	case xs of
		[] => 1
	 | (x::xs') => x * product(xs')

(* Concatenates a list of lists. [[1],[2,3],[4]] becomes [1,2,3,4]*)
fun concat(xss) = 
	case xss of
		[] => []
	  | (xs::xss') => xs @ concat(xss')

(* Returns the maximum element of a list. *)
fun maximum(xs)= 
	case xs of 
		[] => NONE
	  | (head::[]) => SOME head
	  | (head::neck::rest) =>	if head > neck
	  							then maximum (head::rest) 
	  							else maximum (neck::rest)

(* Returns the minimum element of a list. *)
fun minimum(xs)= 
	case xs of 
		[] => NONE
	  | (head::[]) => SOME head
	  | (head::neck::rest) =>	if head < neck
	  							then minimum (head::rest) 
	  							else minimum (neck::rest)	  							

(* Replicates an element x a number of n times and returns a list. *)
fun replicate(x, n) = 
	if n < 1
	then []
	else x::replicate(x, n-1)


(* Attempts to find x in xs. If found then SOME x else NONE *)
fun lookup(x, xs) = 
	case xs of 
		[] => NONE
	  | (x'::xs') => if x = x' 
	  				 then SOME x 
	  				 else lookup(x,xs')


fun print1s(s) = ((print(s^"\n");true))
fun printsl([]) = true
        | printsl(h::t) = print1s(h) andalso printsl(t)
	  				 