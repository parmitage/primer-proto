(* TODO / NOTE
   - Association list for bindings / environments?
   - Put take_while in a utils module? In its own file?
   - Environment.lookup is inefficient as it does a double scan of the environment
   - Nicer marker for environment frames
   - Move modules into separate files
   - apply needs to evaluate where clause
   - evlis is inefficient - doesn't need to evaluate all lists
   - Does OCaml intern strings or should I intern symbols?
*)

exception Type_mismatch
exception Unbound_symbol
exception Already_bound

type expression =
    Symbol of string
  | Int of int
  | Float of float
  | Char of char
  | List of expression list
  | Closure of expression list * expression * expression * binding list
  | Apply of expression * expression list
  | Add of expression * expression
  | Sub of expression * expression
  | Mul of expression * expression
  | Div of expression * expression
and binding = { symbol: string; value: expression; }

(* pretty printer *)
let rec pprint exp =
  match exp with
      Symbol s -> print_string s; exp
    | Int i -> print_int i; exp
    | Float f -> print_float f; exp
    | Char c -> print_char c; exp
    | List l -> exp
    | Closure(p, b, w, e) -> print_string "#<closure>"; exp
    | Add(x, y) -> print_string "#<operator>"; exp
    | Sub(x, y) -> print_string "#<operator>"; exp
    | Mul(x, y) -> print_string "#<operator>"; exp
    | Div(x, y) -> print_string "#<operator>"; exp
    | Apply(s, a) -> print_string "#<funcall>"; exp

let rec take_while p lst = match lst with 
  | [] -> []
  | x::xs -> if p x then x :: (take_while p xs) else []

module Environment =
struct
  let symbol_eq sym bind = match bind with {symbol; value} -> symbol = sym
  let symbol_bound sym env = 
    try
      match List.find (symbol_eq sym) env with
          {symbol; value} -> true
    with
        Not_found -> false
  let lookup sym env =
    if symbol_bound sym env
    then match List.find (symbol_eq sym) env with {symbol; value} -> value
    else raise Unbound_symbol
  let top env =
    take_while (fun b -> match b with {symbol; value} -> symbol <> "env") env
  let extend e s v =
    let current = top e in
    let bound = symbol_bound s current in
    if bound
    then raise Already_bound
    else {symbol = s; value = v} :: e
  let create env = {symbol = "env"; value = Int(-1)} :: env
  let bind p a e =
    List.append (List.map2
                   (fun sym a -> match sym with
                       Symbol s -> {symbol = s; value = a}
                     | _ -> raise Type_mismatch) p a) e
end

module Operators =
struct
  let add lhs rhs =
    match lhs, rhs with
        Int x, Int y -> Int(x + y)
      | Int x, Float y -> Float(float_of_int x +. y)
      | Float x, Int y -> Float(x +. float_of_int y)
      | Float x, Float y -> Float(x +. y)
      | _ -> raise Type_mismatch
  let sub lhs rhs =
    match lhs, rhs with
        Int x, Int y -> Int(x - y)
      | Int x, Float y -> Float(float_of_int x -. y)
      | Float x, Int y -> Float(x -. float_of_int y)
      | Float x, Float y -> Float(x -. y)
      | _ -> raise Type_mismatch
  let mul lhs rhs =
    match lhs, rhs with
        Int x, Int y -> Int(x * y)
      | Int x, Float y -> Float(float_of_int x *. y)
      | Float x, Int y -> Float(x *. float_of_int y)
      | Float x, Float y -> Float(x *. y)
      | _ -> raise Type_mismatch
  let div lhs rhs =
    match lhs, rhs with
        Int x, Int y -> Float(float_of_int x /. float_of_int y)
      | Int x, Float y -> Float(float_of_int x /. y)
      | Float x, Int y -> Float(x /. float_of_int y)
      | Float x, Float y -> Float(x /. y)
      | _ -> raise Type_mismatch
end

(* evaluate an expression in an environment *)
let rec eval exp env =
  match exp with
      Symbol s -> Environment.lookup s env
    | Int i -> exp
    | Float f -> exp
    | Char c -> exp
    | List l -> List(evlis l env)
    | Closure(args, body, where, env) -> exp
    | Apply(s, a) -> apply s (evlis a env) env
    | Add(lhs, rhs) -> Operators.add (eval lhs env) (eval rhs env)
    | Sub(lhs, rhs) -> Operators.sub (eval lhs env) (eval rhs env)
    | Mul(lhs, rhs) -> Operators.mul (eval lhs env) (eval rhs env)
    | Div(lhs, rhs) -> Operators.div (eval lhs env) (eval rhs env)
and apply sym args e =
  match sym with
      Symbol s ->
        let f = Environment.lookup s e in
        begin match f with
            Closure(p, b, w, ce) ->
              eval b (Environment.bind p args (Environment.create ce))
          | _ -> raise Type_mismatch
        end
    | _ -> raise Type_mismatch
and evlis lst env =
  List.map (fun exp -> eval exp env) lst

(* test program *)
let x = { symbol = "x"; value = Int(400) } ;;
let y = { symbol = "y"; value = Float(3.14) } ;;

(* TODO try adding symbols *)
let body = Mul(Symbol("x"), Int(12)) ;;
let func = Closure(Symbol("x") :: [], body, Symbol("a"), []) ;;
let f1 = { symbol = "f1"; value = func } ;;
let a = Apply(Symbol("f1"), Symbol("y") :: []) ;;

let e = f1 :: x :: y :: [] ;;
let result = eval a e ;;
pprint(result) ;;
