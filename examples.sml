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

(* Returns the las element of a list. *)
fun last(xs) =
	case xs of
		[] => raise List.Empty
  	 | (x::[]) => x
  	 | (_::xs') => last(xs')

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


fun concat(xs, ys) = 
	case ys of 
		[] => xs
	  | (y::ys') => concat(xs @ [y], ys')

