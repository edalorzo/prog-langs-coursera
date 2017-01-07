structure Printf =
   struct
      fun $ (_, f) = f (fn p => p ()) ignore
      fun fprintf out f = f (out, id)
      val printf = fn z => fprintf TextIO.stdOut z
      fun one ((out, f), make) g =
         g (out, fn r =>
            f (fn p =>
               make (fn s =>
                     r (fn () => (p (); TextIO.output (out, s))))))
      fun ` x s = one (x, fn f => f s)
      fun spec to x = one (x, fn f => f o to)
      val B = fn z => spec Bool.toString z
      val I = fn z => spec Int.toString z
      val R = fn z => spec Real.toString z
   end
