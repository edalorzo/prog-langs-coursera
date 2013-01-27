
fun same_string(s1 : string, s2 : string) =
    s1 = s2

fun all_except_option(buscar: string, lista: string list)=
	case lista of 
		[] => NONE
		|x::xs' => if same_string(x,xs') then SOME xs' else all_except_option(buscar,xs')