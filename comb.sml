
fun comb1(n,m) =
    let
	exception BadN;
	exception BadM;
    in
	if n <= 0 then raise BadN 
	else if m < 0 orelse m > n then raise BadM
	else if m = 0 orelse m = n then 1
	else comb1(n-1, m) + comb1(n-1, m-1)
    end;
			  

fun comb(_,[n,m]) =
    let
	val nInt = valOf(Int.fromString(n));
	val mInt = valOf(Int.fromString(m));
	val answer = comb1(nInt, mInt);
    in
	(
	  print(Int.toString(answer));
	  print("\n");
	  OS.Process.success
	)
    end;

SMLofNJ.exportFn("combfile", comb);
