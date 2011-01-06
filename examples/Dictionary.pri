val add = fn (d, k, v) d ++ [[k, v]];

val get = fn (d, k)
   let v = FindByFn(k, fn (x) x at 0, d) in
   if v != false then Head(Tail(v))
   else [];

val update = fn (ds, k, v)
   if Head(ds) != []
   then if Head(Head(ds)) == k
        then add(Tail(ds), k, v)
        else Head(ds) :: update(Tail(ds), k, v)
   else Head(ds);

val remove = fn (ds, k)
   if Head(ds) != []
   then if Head(Head(ds)) == k
        then Tail(ds)
        else Head(ds) :: remove(Tail(ds), k)
   else Head(ds);

val d = [];
val d1 = add(d, "one", 1);
val d2 = add(d1, "two", 2);
val d3 = add(d2, "three", 3);

d3;
get(d3, "one");
get(d3, "four");
update(d3, "one", 5);
update(d3, "two", 5);
update(d3, "three", 5);
update(d3, "four", 5);
remove(d3, "one");
remove(d3, "two");
remove(d3, "three");
remove(d3, "four");