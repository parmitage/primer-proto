let rec take_while p lst = match lst with 
  | [] -> []
  | x::xs -> if p x then x :: (take_while p xs) else []

let rec intersperse sep lst = match lst with
    [] -> []
  | x::[] -> [x]
  | x::xs -> x :: sep :: (intersperse sep xs)

let (--) i j = 
  let rec aux n acc =
    if n < i then acc else aux (n-1) (n :: acc)
  in aux j []
