datatype 'label btree = Empty | Node of 'label * 'label btree * 'label btree

fun lower(cs) = 
	case cs of 
	  	[] => []
	  | (c::cs') => (Char.toLower c)::lower(cs')

fun lt(x,y)	= implode(lower(explode x)) < implode(lower(explode y))

fun lookup(x,t) =
	case t of
		Empty => false
	  | Node(y, left, right) => if lt(x,y) then lookup(x,left)
	  						   	else if lt(y,x) then lookup(x, right)
	  						   	else (* x=y *) true


fun insert(x, t) =
	case t of
		Empty => Node(x, Empty, Empty)
	 |  Node(y, left, right) => if lt(x,y) then Node(y, insert(x, left), right)
	 							else if lt(y,x) then Node(y, left, insert(x, right))
	 							else (* x=y *) t

fun delete(x, t) =
	case t of
		Empty => Empty |
		Node(y, left, right) => if lt(x,y) then Node(y, delete(x, left), right)
								else if lt(y,x) then Node(y, left, insert(x, right))
								else (* x=y *)
									case (left, right) of
										(Empty, r) => r |
										(l, Empty) => l |
										(l,r) =>
												let 
													val (z,r1) = deletemin(r)
												in 
													Node(z,l,r1)
												end
