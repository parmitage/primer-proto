(* this version will overflow stack on larger values *)

val fac1 = fn (n)
   if n == 0
   then 1
   else n * fac1 (n - 1);

fac1(10);

(* this version uses an accumulator *)

val fac2 = fn (n)
   let inner = fn (i, acc)
                  if i == 0
                  then acc
                  else inner (i - 1, acc * i)
   in inner (n, 1);

fac2(10);