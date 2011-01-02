Empty = fn (xs) Head(xs) == [];

Assert = fn (id, act, exp)
   if act != exp then Show(id)
   else true;

Map = fn (f, xs)
   if Head(xs) == []
   then []
   else f(Head(xs)) :: Map(f, Tail(xs));

FoldL = fn (f, init, xs)
   if Head(xs) != []
   then FoldL(f, f(init, Head(xs)), Tail(xs))
   else init;

Filter = fn (f, xs)
   if Head(xs) != []
   then if f(Head(xs))
        then Head(xs) :: Filter(f, Tail(xs))
        else Filter(f, Tail(xs))
   else [];

FoldR = fn (f, init, xs)
   if Head(xs) != []
   then f(Head(xs), FoldR(f, init, Tail(xs)))
   else init;

Reverse = fn (xs)
   let Inner = fn (xs, accum)
                  if Head(xs) != []
                  then Inner(Tail(xs), Head(xs) :: accum)
                  else accum
   in Inner(xs, []);

Find = fn (a, xs)
   if Head(xs) == a
   then Head(xs)
   else if Tail(xs) != []
        then Find(a, Tail(xs))
        else false;

FindByFn = fn (a, f, xs)
   if f(Head(xs)) == a
   then Head(xs)
   else if Tail(xs) != []
        then FindByFn(a, f, Tail(xs))
        else false;

Replace = fn (a, b, xs)
   if Head(xs) != []
   then if Head(xs) == a
        then b :: Replace(a, b, Tail(xs))
        else Head(xs) :: Replace(a, b, Tail(xs))
   else [];

Sum = fn (xs)
  if Head(xs) != []
  then Head(xs) + Sum(Tail(xs))
  else 0;

Product = fn (xs)
   let Inner = fn (xs)
                  if Head(xs) != []
                  then Head(xs) * Inner(Tail(xs))
                  else 1
   in if Empty(xs)
      then 0
      else Inner(xs);

Any = fn (pred, xs)
   if Head(xs) != []
   then if pred(Head(xs))
        then true
        else Any(pred, Tail(xs))
   else false;

All = fn (pred, xs)
   if Head(xs) != []
   then if pred(Head(xs))
        then All(pred, Tail(xs))
        else false
   else true;

Take = fn (n, xs)
   let Inner = fn (a, xs)
                  if Head(xs) != [] and a < n
                  then Head(xs) :: Inner(a + 1, Tail(xs))
                  else []
   in Inner(0, xs);

TakeWhile = fn (f, xs)
   if Head(xs) != [] and f(Head(xs))
   then Head(xs) :: TakeWhile(f, Tail(xs))
   else [];

Drop = fn (n, xs)
   let Inner = fn (a, xs)
                   if Head(xs) != []
                   then if a < n
                        then Inner(a + 1, Tail(xs))
                        else xs
                   else xs
   in if n >= Length(xs)
      then []
      else Inner(0, xs);

DropWhile = fn (f, xs)
   if Head(xs) != []
   then if f(Head(xs))
        then DropWhile(f, Tail(xs))
        else xs
   else Tail(xs);

Sort = fn (xs)
   let Lt = fn (a) a < Head(xs) in
   let Gte = fn (a) a >= Head(xs) in
   if Head(xs) != []
   then Sort(Filter(Lt, Tail(xs))) ++ [Head(xs)] ++ Sort(Filter(Gte, Tail(xs)))
   else [];

SortBy = fn (xs, f)
   let Lt = fn (a) f(a) < f(Head(xs)) in
   let Gte = fn (a) f(a) >= f(Head(xs)) in
   if Head(xs) != []
   then SortBy(Filter(Lt, Tail(xs)), f) ++ [Head(xs)] ++ SortBy(Filter(Gte, Tail(xs)), f)
   else [];

Zip = fn (xs, ys)
   if Head(xs) != [] and Head(ys) != []
   then [Head(xs), Head(ys)] :: Zip(Tail(xs), Tail(ys))
   else [];

Intersperse = fn (sep, xs)
   if Head(xs) == []
   then []
   else if Empty(Tail(xs))
        then [Head(xs)]
        else Head(xs) :: sep :: Intersperse(sep, Tail(xs));

Min = fn (xs) Head(Sort(xs));
Max = fn (xs) Head(Reverse(Sort(xs)));
Last = fn (xs) Head(Reverse(xs));
Odd = fn (n) n mod 2 != 0;
Even = fn (n) n mod 2 == 0;
BitSet = fn (n, b) (n & (1 << b)) > 0;

Collect = fn (f, n)
   let Inner = fn (f, c)
                  if c < n
                  then f() :: Inner(f, c + 1)
                  else []
   in Inner(f, 0);

MapPair = fn (f, xs)
   if Length(xs) >= 2
   then f(Head(xs), xs at 1) :: MapPair(f, Drop(2, xs))
   else [];