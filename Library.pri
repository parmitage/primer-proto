val Empty = fn (xs) Head(xs) == [];

val Assert = fn (id, act, exp)
   if act != exp then Show(id)
   else true;

val Map = fn (f, xs)
   let inner = fn (xs, accum)
      if Head(xs) != []
      then inner(Tail(xs), f(Head(xs)) :: accum)
      else Reverse(accum) in
   inner(xs, []);

val FoldL = fn (f, init, xs)
   if Head(xs) != []
   then FoldL(f, f(init, Head(xs)), Tail(xs))
   else init;

val Filter = fn (f, xs)
   let inner = fn (ys, accum)
      if Head(ys) != []
      then if f(Head(ys))
           then inner(Tail(ys), accum ++ [Head(ys)])
           else inner(Tail(ys), accum)
      else accum
   in inner(xs, []);

val FoldR = fn (f, init, xs)
   if Head(xs) != []
   then f(Head(xs), FoldR(f, init, Tail(xs)))
   else init;

val Reverse = fn (xs)
   let Inner = fn (xs, accum)
      if Head(xs) != []
      then Inner(Tail(xs), Head(xs) :: accum)
      else accum
   in Inner(xs, []);

val Find = fn (a, xs)
   if Head(xs) == a
   then Head(xs)
   else if Tail(xs) != []
        then Find(a, Tail(xs))
        else false;

val FindByFn = fn (a, f, xs)
   if f(Head(xs)) == a
   then Head(xs)
   else if Tail(xs) != []
        then FindByFn(a, f, Tail(xs))
        else false;

val Replace = fn (a, b, xs)
   if Head(xs) != []
   then if Head(xs) == a
        then b :: Replace(a, b, Tail(xs))
        else Head(xs) :: Replace(a, b, Tail(xs))
   else [];

val Sum = fn (xs)
   let inner = fn (xs, accum)
      if Head(xs) != []
      then inner(Tail(xs), Head(xs) + accum)
      else accum
   in inner(xs, 0);

val Product = fn (xs)
   let Inner = fn (xs)
                  if Head(xs) != []
                  then Head(xs) * Inner(Tail(xs))
                  else 1
   in if Empty(xs)
      then 0
      else Inner(xs);

val Any = fn (pred, xs)
   if Head(xs) != []
   then if pred(Head(xs))
        then true
        else Any(pred, Tail(xs))
   else false;

val All = fn (pred, xs)
   if Head(xs) != []
   then if pred(Head(xs))
        then All(pred, Tail(xs))
        else false
   else true;

val Take = fn (n, xs)
   let Inner = fn (c, xs, ac)
      if Head(xs) != [] and c < n
      then Inner(c + 1, Tail(xs), Head(xs) :: ac)
      else Reverse(ac)
   in Inner(0, xs, []);

val TakeWhile = fn (f, xs)
   if Head(xs) != [] and f(Head(xs))
   then Head(xs) :: TakeWhile(f, Tail(xs))
   else [];

val Drop = fn (n, xs)
   let Inner = fn (a, xs)
      if Head(xs) != []
      then if a < n
           then Inner(a + 1, Tail(xs))
           else xs
      else xs
   in if n >= Length(xs)
      then []
      else Inner(0, xs);

val DropWhile = fn (f, xs)
   if Head(xs) != []
   then if f(Head(xs))
        then DropWhile(f, Tail(xs))
        else xs
   else Tail(xs);

val Partition = fn (f, xs)
   let inner = fn (ts, fs, xs)
      if Head(xs) != []
      then if f(Head(xs))
           then inner(Head(xs) :: ts, fs, Tail(xs))
           else inner(ts, Head(xs) :: fs, Tail(xs))
      else [ts, fs]
   in inner([], [], xs);

### Simple QuickSort - fast for short lists
val Sort = fn (xs)
   let Lt = fn (a) a < Head(xs) in
   let Gte = fn (a) a >= Head(xs) in
   if Head(xs) != []
   then Sort(Filter(Lt, Tail(xs))) ++ [Head(xs)] ++ Sort(Filter(Gte, Tail(xs)))
   else [];

### Tail recursive QuickSort using CPS - OK for longer lists
val QSort = fn (xs)
   let loop = fn (xs, cont)
      if Head(xs) == []
      then cont([])
      else let x = Head(xs) in
           let xss = Tail(xs) in
           let parts = Partition(fn (y) x > y, xss) in
           loop(parts at 0, fn (lacc)
           loop(parts at 1, fn (racc) cont(lacc ++ (x :: racc))))
   in loop(xs, fn (x) x);

val SortBy = fn (xs, f)
   let Lt = fn (a) f(a) < f(Head(xs)) in
   let Gte = fn (a) f(a) >= f(Head(xs)) in
   if Head(xs) != []
   then SortBy(Filter(Lt, Tail(xs)), f) ++ [Head(xs)] ++ SortBy(Filter(Gte, Tail(xs)), f)
   else [];

val QSortBy = fn (xs, f)
   let loop = fn (xs, cont)
      if Head(xs) == []
      then cont([])
      else let x = Head(xs) in
           let xss = Tail(xs) in
           let parts = Partition(fn (y) f(x) > f(y), xss) in
           loop(parts at 0, fn (lacc)
           loop(parts at 1, fn (racc) cont(lacc ++ (x :: racc))))
   in loop(xs, fn (x) x);

val Zip = fn (xs, ys)
   if Head(xs) != [] and Head(ys) != []
   then [Head(xs), Head(ys)] :: Zip(Tail(xs), Tail(ys))
   else [];

val Intersperse = fn (sep, xs)
   if Head(xs) == []
   then []
   else if Empty(Tail(xs))
        then [Head(xs)]
        else Head(xs) :: sep :: Intersperse(sep, Tail(xs));

val Min = fn (xs) Head(Sort(xs));
val Max = fn (xs) Head(Reverse(Sort(xs)));
val Last = fn (xs) Head(Reverse(xs));
val Odd = fn (n) n mod 2 != 0;
val Even = fn (n) n mod 2 == 0;
val BitSet = fn (n, b) (n & (1 << b)) > 0;

val Collect = fn (f, n)
   let Inner = fn (f, c)
      if c < n
      then f() :: Inner(f, c + 1)
      else []
   in Inner(f, 0);

val MapPair = fn (f, xs)
   let inner = fn (f, xs, ac)
      if Length(xs) >= 2
      then inner(f, Drop(2, xs), f(Head(xs), xs at 1) :: ac)
      else Reverse(ac)
   in inner(f, xs, []);