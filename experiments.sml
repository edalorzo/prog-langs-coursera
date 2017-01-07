
signature A = 
sig
    type t
    val T: int -> t
    val S: string -> t
end

datatype def = T of int | S of string

structure AData :> A =
struct
type t = def
end

val one = AData.T(10)
val two = AData.S("Hello")


