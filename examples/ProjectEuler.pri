(* Project Euler Problem 1 *)

val pe1 = fn (limit)
   let inner = fn (x, accum)
                  if x < limit
                  then if x mod 3 == 0 or x mod 5 == 0
                       then inner(x + 1, accum + x)
                       else inner(x + 1, accum)
                  else accum
   in inner(0, 0);

pe1(1000);  (* 233168 *)

(* Project Euler Problem 2 *)

val fib = fn (n)
   let inner = fn (iter, result, next)
                  if iter == 0
                  then result
                  else inner(iter - 1, next, result + next)
   in inner(n, 0, 1);

Sum(Filter(Even, Map(fib, 0..40)));  (* 82790070 *)

(* Project Euler Problem 3 *)

val prime = fn (x)
   let inner = fn (y)
                  if x > y
                  then if x mod y == 0
                       then false
                       else inner(y + 1)
                  else true
   in inner(2);

val factor = fn (x, y) x mod y == 0;

val pe3 = fn (n)
   let primeFactor = fn (x) prime(x) and factor(n, x) in
   Last(Filter(primeFactor, 2..n));

pe3(6000);