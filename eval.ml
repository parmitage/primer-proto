(* TODO
   - lexer / parser
   - car, cdr, cons, append
   - if
   - strings
   - length
   - at
   - range
   - show
   - is
   - as
   - loadlib
   - error
   - proper printer
   - bitwise ops
   - symbol interning (or will OCaml do it?)
   - newline, tab
   - rnd
   - Better matching for symbols and definitions (symbol_eq isn't really symbol equality)
   - Environment.lookup is inefficient as it does a double scan of the environment
   - Nicer marker for environment frames
   - evlis is inefficient - doesn't need to evaluate everything
*)

exception Type_mismatch
exception Unbound_symbol
exception Attempted_redefinition

type binop = Add | Sub | Mul | Div | Mod | Eq | Ne | Lt | Gt | Gte | Lte | And | Or
type uniop = Not | Neg

type expression =
    Symbol of string
  | Int of int
  | Float of float
  | Char of char
  | Bool of bool
  | List of expression list
  | Lambda of expression list * expression * definition list
  | Closure of expression list * expression * definition list * definition list
  | Apply of expression * expression list
  | BinOp of binop * expression * expression
  | UniOp of uniop * expression
and definition = Def of expression * expression

let rec pprint exp =
  match exp with
      Symbol s -> Format.print_string s; exp
    | Int i -> Format.print_int i; exp
    | Float f -> Format.print_float f; exp
    | Char c -> Format.print_char c; exp
    | Bool b -> Format.print_bool b; exp
    | List l -> exp
    | Lambda(p, b, w) -> Format.print_string "#<lambda>"; exp
    | Closure(p, b, w, e) -> Format.print_string "#<closure>"; exp
    | BinOp(o, x, y) -> Format.print_string "#<binop>"; exp
    | UniOp(o, x) -> Format.print_string "#<uniop>"; exp
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

let unary_op oper arg =
  match oper, arg with
      Not, Bool x -> Bool(not x)
    | Neg, Int x -> Int(-x)
    | Neg, Float x -> Float(-1.0 *. x)
    | _ -> raise Type_mismatch

let binary_op oper lhs rhs =
  match oper, lhs, rhs with
      Add, Int x, Int y -> Int(x + y)
    | Add, Int x, Float y -> Float(float_of_int x +. y)
    | Add, Float x, Int y -> Float(x +. float_of_int y)
    | Add, Float x, Float y -> Float(x +. y)
    | Sub, Int x, Int y -> Int(x - y)
    | Sub, Int x, Float y -> Float(float_of_int x -. y)
    | Sub, Float x, Int y -> Float(x -. float_of_int y)
    | Sub, Float x, Float y -> Float(x -. y)
    | Mul, Int x, Int y -> Int(x * y)
    | Mul, Int x, Float y -> Float(float_of_int x *. y)
    | Mul, Float x, Int y -> Float(x *. float_of_int y)
    | Mul, Float x, Float y -> Float(x *. y)
    | Div, Int x, Int y -> Float(float_of_int x /. float_of_int y)
    | Div, Int x, Float y -> Float(float_of_int x /. y)
    | Div, Float x, Int y -> Float(x /. float_of_int y)
    | Div, Float x, Float y -> Float(x /. y)
    | Mod, Int x, Int y -> Int(x mod y)
    | Eq, Int x, Int y -> Bool(x == y)
    | Eq, Float x, Float y -> Bool(x == y)
    | Eq, Int x, Float y -> Bool(float_of_int x == y)
    | Eq, Float x, Int y -> Bool(x == float_of_int y)
    | Eq, Char x, Char y -> Bool(x == y)
    | Eq, Bool x, Bool y -> Bool(x == y)
    | Eq, Symbol x, Symbol y -> Bool(x == y)
    | Eq, _, _ -> Bool(false)
    | Ne, Int x, Int y -> Bool(x != y)
    | Ne, Float x, Float y -> Bool(x != y)
    | Ne, Int x, Float y -> Bool(float_of_int x != y)
    | Ne, Float x, Int y -> Bool(x != float_of_int y)
    | Ne, Char x, Char y -> Bool(x != y)
    | Ne, Bool x, Bool y -> Bool(x != y)
    | Ne, Symbol x, Symbol y -> Bool(x != y)
    | Ne, _, _ -> Bool(true)
    | Lt, Int x, Int y -> Bool(x < y)
    | Lt, Float x, Float y -> Bool(x < y)
    | Lt, Int x, Float y -> Bool(float_of_int x < y)
    | Lt, Float x, Int y -> Bool(x < float_of_int y)
    | Gt, Int x, Int y -> Bool(x > y)
    | Gt, Float x, Float y -> Bool(x > y)
    | Gt, Int x, Float y -> Bool(float_of_int x > y)
    | Gt, Float x, Int y -> Bool(x > float_of_int y)
    | Lte, Int x, Int y -> Bool(x <= y)
    | Lte, Float x, Float y -> Bool(x <= y)
    | Lte, Int x, Float y -> Bool(float_of_int x <= y)
    | Lte, Float x, Int y -> Bool(x <= float_of_int y)
    | Gte, Int x, Int y -> Bool(x >= y)
    | Gte, Float x, Float y -> Bool(x >= y)
    | Gte, Int x, Float y -> Bool(float_of_int x >= y)
    | Gte, Float x, Int y -> Bool(x >= float_of_int y)
    | And, Bool x, Bool y -> Bool(x && y)
    | Or, Bool x, Bool y -> Bool(x || y)
    | _ -> raise Type_mismatch

let rec eval exp env =
  match exp with
      Symbol s -> Environment.lookup exp env
    | Int i -> exp
    | Float f -> exp
    | Char c -> exp
    | Bool b -> exp
    | List l -> List(evlis l env)
    | Lambda(p, b, w) -> Closure(p, b, w, env)
    | Closure(p, b, w, e) -> exp
    | Apply(s, a) -> apply s (evlis a env) env
    | UniOp(o, arg) -> unary_op o (eval arg env)
    | BinOp(o, lhs, rhs) -> binary_op o (eval lhs env) (eval rhs env)
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

let body = BinOp(Add, Symbol("x"), Symbol("z")) ;;
let where = Def(Symbol("z"), Int(4)) :: [] ;;
let func = Closure(Symbol("x") :: [], body, where, []) ;;
let f1 = Def(Symbol("f1"), func) ;;
let a = Apply(Symbol("f1"), Symbol("x") :: []) ;;

let e = f1 :: x :: y :: [] ;;
let result = eval a e ;;
pprint(result) ;;
