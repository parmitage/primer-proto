(* TODO
   - Closure type ctor should take a list of expressions as 1st parameter
   - Apply type ctor should take a list of expressions as 2nd parameter
   - Association list for bindings / environments?
   - Put take_while in a utils module? In its own file?
   - Should all environment related functions go into a module?
   - environment_lookup is inefficient as it does a double scan of the environment
   - Nicer marker for environment frames
   - Apply case in eval needs to evaluate its arguments
   - apply needs to bind arguments
   - apply needs to evaluate where clause
   - evlis is inefficient - doesn't need to evaluate all lists
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
  | Closure of expression * expression * expression * binding list
  | Apply of expression * expression
  | Add of expression * expression
and binding = { symbol: string; value: expression; }

(* pretty printer *)
let rec pprint exp =
  match exp with
      Symbol s -> print_string s; exp
    | Int i -> print_int i; exp
    | Float f -> print_float f; exp
    | Char c -> print_char c; exp
    | List l -> exp
    | Closure(args, body, where, env) -> print_string "#<closure>"; exp
    | Add(lhs, rhs) -> print_string "#<operator>"; exp
    | Apply(s, a) -> print_string "#<funcall>"; exp

let rec take_while p lst = match lst with 
  | [] -> []
  | x::xs -> if p x then x :: (take_while p xs) else []

(* is a symbol represented by this binding *)
let symbol_eq sym bind = match bind with {symbol; value} -> symbol = sym

(* is a symbol bound in an environment *)
let symbol_bound sym env = 
  try
    match List.find (symbol_eq sym) env with
        {symbol; value} -> true
  with
      Not_found -> false

(* return the value bound to a symbol in an environment *)
let environment_lookup sym env =
  if symbol_bound sym env
  then match List.find (symbol_eq sym) env with
      {symbol; value} -> value
  else raise Unbound_symbol

(* return the most inner environment *)
let environment_top env =
  take_while (fun b -> match b with {symbol; value} -> symbol <> "env") env

(* add a binding to an environment *)
let environment_extend e s v =
  let current = environment_top e in
  let bound = symbol_bound s current in
  if bound
  then raise Already_bound
  else {symbol = s; value = v} :: e

(* add an environment marker to an existing environment *)
let environment_new env = {symbol = "env"; value = Int(-1)} :: env

let add lhs rhs =
  match lhs, rhs with
      Int x, Int y -> Int(x + y)
    | Int x, Float y -> Float(float_of_int x +. y)
    | Float x, Int y -> Float(x +. float_of_int y)
    | Float x, Float y -> Float(x +. y)
    | _ -> raise Type_mismatch

(* evaluate an expression in an environment  *)
let rec eval exp env =
  match exp with
      Symbol s -> environment_lookup s env
    | Int i -> exp
    | Float f -> exp
    | Char c -> exp
    | List l -> List(evlis l env)
    | Closure(args, body, where, env) -> exp
    | Apply(s, a) -> apply s a env
    | Add(lhs, rhs) -> add (eval lhs env) (eval rhs env)
and apply sym args e =
  match sym with
      Symbol s ->
        let f = environment_lookup s e in
        begin match f with
            Closure(a, b, w, ce) ->
              eval b (environment_new ce)
          | _ -> raise Type_mismatch
        end
    | _ -> raise Type_mismatch
and evlis lst env =
  List.map (fun exp -> eval exp env) lst

(* test program *)
let x = { symbol = "x"; value = Int(12) } ;;
let y = { symbol = "y"; value = Float(3.14) } ;;

(* TODO try adding symbols *)
let body = Add(Int(14), Int(12)) ;;
let func = Closure(Symbol("x"), body, Symbol("a"), []) ;;
let f1 = { symbol = "f1"; value = func } ;;
let a = Apply(Symbol("f1"), Int(12)) ;;

let e = f1 :: x :: y :: [] ;;
let result = eval a e ;;
pprint(result) ;;
