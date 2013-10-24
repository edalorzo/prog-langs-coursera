

datatype item = String of string | Number of int


fun zip(vs, ps) =
    case (vs,ps) of
       (v::vv,p::pp) => (v,p) :: zip(vv,pp)
     | _ => []