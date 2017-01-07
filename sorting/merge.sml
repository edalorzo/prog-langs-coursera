(*
fun merge(nil, M) = M
|	merge(L, nil) = L
|	merge(L as x::xs, M as y::ys) =
		if x < y then x::merge(xs, M)
		else y::merge(L, ys);
*)

fun merge(L,R) =
  case (L,R) of
      	(nil,ys) 		=> 	ys 
      | (xs,nil) 		=> 	xs 
      | (x::xs,y::ys) 	=>  if x < y then x::merge(xs,R)
							else y::merge(L, ys);
		

fun split(L) =
	case L of
			[]				=>  ([],[])
		|	(x::[])			=>	([x],[])
		|	(x::y::[])		=> 	([x],[y])
		|	(x::y::xs)		=>	let 
									val (XS,YS) = split(xs)
								in
									(x::XS,y::YS)
								end;

fun mergeSort(L) =
	case L of
		  []		=> []
		| [x]		=> [x]
		| (x::xs)	=> let 
							val (M,N) = split(L)
							val M = mergeSort(M)
							val N = mergeSort(N)
						in
							merge(M,N)
						end;				