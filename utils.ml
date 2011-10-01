open Str

let rec take_while p lst = match lst with 
  | [] -> []
  | x::xs -> if p x then x :: (take_while p xs) else []

let rec intersperse sep lst = match lst with
  | [] -> []
  | x::[] -> [x]
  | x::xs -> x :: sep :: (intersperse sep xs)

let (--) i j = 
  let rec aux n acc =
    if n < i then acc else aux (n-1) (n :: acc)
  in aux j []

let rec zip l1 l2 = match l1, l2 with
    x::xs, y::ys -> (x, y) :: zip xs ys
  | [], [] -> []
  | _, _ -> []

let map f l =
  let rec inner l accum = match l with
    | x::xs -> inner xs ((f x) :: accum)
    | []    -> List.rev accum
  in inner l [] ;;

let last l = List.hd (List.rev l)

let rec replace_one_by fn a b l = match l with
    x::xs ->
      if fn a x
      then b :: xs
      else x :: replace_one_by fn a b xs
  | [] -> []

let split_str sep str =
  Str.split (Str.regexp_string sep) str

let error msg = Format.printf "@[error: %s@]@." msg

let (%) = Printf.sprintf
