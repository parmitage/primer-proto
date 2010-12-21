(* TODO / NOTE
   - Better matching for symbols and definitions (symbol_eq isn't really symbol equality)
   - Environment.lookup is inefficient as it does a double scan of the environment
   - Nicer marker for environment frames
   - evlis is inefficient - doesn't need to evaluate all lists
   - lambda
   - car, cdr, cons, append
   - strings
   - proper printer
   - Does OCaml intern strings or should I (for symbols)?
*)

exception Type_mismatch
exception Unbound_symbol
exception Attempted_redefinition

type expression =
    Symbol of string
  | Int of int
  | Float of float
  | Char of char
  | Bool of bool
  | List of expression list
  | Closure of expression list * expression * definition list * definition list
  | Apply of expression * expression list
  | Add of expression * expression
  | Sub of expression * expression
  | Mul of expression * expression
  | Div of expression * expression
  | Equal of expression * expression
and definition = Def of expression * expression

let rec pprint exp =
  match exp with
      Symbol s -> Format.print_string s; exp
    | Int i -> Format.print_int i; exp
    | Float f -> Format.print_float f; exp
    | Char c -> Format.print_char c; exp
    | Bool b -> Format.print_bool b; exp
    | List l -> exp
    | Closure(p, b, w, e) -> Format.print_string "#<closure>"; exp
    | Add(x, y) -> Format.print_string "#<operator>"; exp
    | Sub(x, y) -> Format.print_string "#<operator>"; exp
    | Mul(x, y) -> Format.print_string "#<operator>"; exp
    | Div(x, y) -> Format.print_string "#<operator>"; exp
    | Equal(x, y) -> Format.print_string "#<operator>"; exp
    | Apply(s, a) -> Format.print_string "#<funcall>"; exp

let rec take_while p lst = match lst with 
  | [] -> []
  | x::xs -> if p x then x :: (take_while p xs) else []

module Environment =
struct
  let symbol_eq sym def =
    match def with
        Def(s, v) -> match s, sym with
            Symbol str1, Symbol str2 -> str1 = str2
          | _ -> raise Type_mismatch
  let symbol_bound sym env = List.exists (symbol_eq sym) env
  let lookup sym env =
    if symbol_bound sym env
    then match List.find (symbol_eq sym) env with Def(s, v) -> v
    else raise Unbound_symbol
  let top env =
    take_while
      (fun b -> match b with
          Def(s, v) -> match s with
              Symbol str -> str <> "env"
            | _ -> raise Type_mismatch) env
  let create env = Def(Symbol "env", Int(-1)) :: env
  let bind p a e = List.append (List.map2 (fun sym a -> Def(sym, a)) p a) e
end

module Operator =
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
  let equal lhs rhs =
    match lhs, rhs with
        Int x, Int y -> Bool(x == y)
      | Float x, Float y -> Bool(x == y)
      | Int x, Float y -> Bool(float_of_int x == y)
      | Float x, Int y -> Bool(x == float_of_int y)
      | Char x, Char y -> Bool(x == y)
      | Bool x, Bool y -> Bool(x == y)
      | Symbol x, Symbol y -> Bool(x == y)
      | _, _ -> Bool(false)
end

let rec eval exp env =
  match exp with
      Symbol s -> Environment.lookup exp env
    | Int i -> exp
    | Float f -> exp
    | Char c -> exp
    | Bool b -> exp
    | List l -> List(evlis l env)
    | Closure(args, body, where, env) -> exp
    | Apply(s, a) -> apply s (evlis a env) env
    | Add(lhs, rhs) -> Operator.add (eval lhs env) (eval rhs env)
    | Sub(lhs, rhs) -> Operator.sub (eval lhs env) (eval rhs env)
    | Mul(lhs, rhs) -> Operator.mul (eval lhs env) (eval rhs env)
    | Div(lhs, rhs) -> Operator.div (eval lhs env) (eval rhs env)
    | Equal(lhs, rhs) -> Operator.equal (eval lhs env) (eval rhs env)
and apply sym args e =
  match sym with
      Symbol s ->
        let f = Environment.lookup sym e in
        begin match f with
            Closure(p, b, w, ce) ->
              eval b (Environment.bind p args (List.append w (Environment.create ce)))
          | _ -> raise Type_mismatch
        end
    | _ -> raise Type_mismatch
and evlis lst env = List.map (fun exp -> eval exp env) lst

(*
  Ideally the parser should generate two lists:

  1. A list of top level definitions. These will be turned into bindings and
  interned in the top level environment.

  2. A list of expressions. Each one of these should be evaluated in turn in
  the newly created top level environment.
*)

(* test program *)
let x = Def(Symbol("x"), Int(400)) ;;
let y = Def(Symbol("y"), Float(3.14)) ;;

let body = Equal(Symbol("x"), Symbol("z")) ;;
let where = Def(Symbol("z"), Int(4)) :: [] ;;
let func = Closure(Symbol("x") :: [], body, where, []) ;;
let f1 = Def(Symbol("f1"), func) ;;
let a = Apply(Symbol("f1"), Symbol("x") :: []) ;;

let e = f1 :: x :: y :: [] ;;
let result = eval a e ;;
pprint(result) ;;
