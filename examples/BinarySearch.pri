BinarySearch = fn (xs, x)
   let Inner = fn (xs, x, s, e)
                  let diff = e - s in
                  let mid = (diff / 2) as int in
                  let idx = s + mid in
                  let val = xs at idx in
                  if s <= e
                  then if x == val
                       then idx
                       else if x < val
                            then Inner(xs, x, s, e - 1)
                            else Inner(xs, x, s + 1, e)
                  else false
   in Inner(xs, x, 0, Length(xs) - 1);

listToSearch = 1..1000;

BinarySearch(listToSearch, 500);
BinarySearch(listToSearch, 1);
BinarySearch(listToSearch, 999);
BinarySearch(listToSearch, 1001);
BinarySearch(listToSearch, -1);