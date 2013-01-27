val marcelo="Pelotudo"

fun sum(xs: int list) =
	if nul xs then 0
	else hd xs + sum(tl xs)
;
