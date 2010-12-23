(* TODO
   - lexer
   - parser
   - loadlib
   - is
   - string equality tests
   - symbol interning (or will OCaml do it?)
   - newline, tab
   - unify strings and lists as in primer1?
   - better error handling
   - will tail calls be eliminated in apply/condition?
   - better matching for symbols and definitions (symbol_eq isn't really symbol equality)
   - environment.lookup is inefficient as it does a double scan of the environment
   - nicer marker for environment frames
   - evlis is inefficient - doesn't need to evaluate everything
*)

exception Type_mismatch
exception Symbol_unbound
exception Symbol_redefined

type binop = Add | Sub | Mul | Div | Mod | Eq | Ne | Lt | Gt | Gte | Lte | And | Or
type uniop = Not | Neg | Bnot
type bitop = Band | Bor | Xor | LShift | RShift

type expression =
    Symbol of string
  | Int of int
  | Float of float
  | Char of char
  | Bool of bool
  | String of string
  | List of expression list
  | Empty
  | If of expression * expression * expression
  | Lambda of expression list * expression * definition list
  | Closure of expression list * expression * definition list * definition list
  | Apply of expression * expression list
  | BinOp of binop * expression * expression
  | UniOp of uniop * expression
  | BitOp of bitop * expression * expression
  | Head of expression list
  | Tail of expression list
  | Cons of expression * expression
  | Append of expression * expression
  | Length of expression
  | At of expression list * expression
  | Show of expression
  | Range of expression * expression
  | Rnd of expression
  | Cast of expression * expression
and definition = Def of expression * expression

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
    else raise Symbol_unbound
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
    | Bnot, Int x -> Int(lnot x)
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

let bitwise_op oper lhs rhs =
  match oper, lhs, rhs with
      Band, Int x, Int y -> Int(x land y)
    | Bor, Int x, Int y -> Int(x lor y)
    | Xor, Int x, Int y -> Int(x lxor y)
    | LShift, Int x, Int y -> Int(x lsl y)
    | RShift, Int x, Int y -> Int(x lsr y)
    | _ -> raise Type_mismatch

let head exp = match exp with
    List l -> begin match l with
        [] -> Empty
      | x::xs -> x
    end
  | _ -> raise Type_mismatch

let tail exp = match exp with
    List l -> begin match l with
        [] -> Empty
      | x::xs -> List(xs)
    end
  | _ -> raise Type_mismatch

let cons atom lst = match lst with
    List xs -> List(atom :: xs)
  | _ -> raise Type_mismatch

let append lst1 lst2 = match lst1, lst2 with
    List xs1, List xs2 -> List(List.append xs1 xs2)
  | _ -> raise Type_mismatch

let length exp = match exp with
    List xs -> Int(List.length xs)
  | _ -> raise Type_mismatch

let at xs i = match i with
    Int i -> List.nth xs i
  | _ -> raise Type_mismatch

let rec pprint exp =
  match exp with
    | Symbol s -> Format.print_string s; exp
    | Int i -> Format.print_int i; exp
    | Float f -> Format.print_float f; exp
    | Char c -> Format.print_char c; exp
    | Bool b -> Format.print_bool b; exp
    | String s -> Format.print_string s; exp
    | List l -> pprint_list l; exp
    | Empty -> Format.print_string "[]"; exp
    | Lambda(p, b, w) -> Format.print_string "#<lambda>"; exp
    | Closure(p, b, w, e) -> Format.print_string "#<closure>"; exp
    | _ -> Format.print_string "#<builtin>"; exp
and pprint_list l =
  Format.print_char '[';
  ignore (List.map pprint (intersperse (String ", ") l)) ;
  Format.print_char ']'

let range f t = match f, t with
  | Int x, Int y -> List(List.map (fun i -> Int(i)) (x -- y))
  | _, _ -> raise Type_mismatch

let random exp = match exp with
  | Int i -> Int(Random.int i)
  | _ -> raise Type_mismatch

let cast f t = match f, t with
  | Int i, Float f -> Float(float_of_int i)
  | Int i, String s -> String(string_of_int i)
  | Int i, Bool b -> Bool(if i <= 0 then false else true)
  | Float f, Int i -> Int(int_of_float f)
  | Float f, String s -> String(string_of_float f)
  | Bool b, Int i -> Int(if b then 1 else 0)
  | Bool b, Float f -> Float(if b then 1.0 else 0.0)
  | Bool b, String s -> String(if b then "true" else "false")
  | Char c, Int i -> Int(int_of_char c)
  | Char c, Float f -> Float(float_of_int (int_of_char c))
  | Char c, String s -> String(String.make 1 c)
  | _ -> raise Type_mismatch

let rec eval exp env =
  match exp with
    | Symbol s -> eval (Environment.lookup exp env) env
    | Int i -> exp
    | Float f -> exp
    | Char c -> exp
    | Bool b -> exp
    | String s -> exp
    | List l -> List(evlis l env)
    | Empty -> exp
    | If(p, c, a) -> condition exp env
    | Lambda(p, b, w) -> Closure(p, b, w, env)
    | Closure(p, b, w, e) -> exp
    | Apply(s, a) -> apply s (evlis a env) env
    | UniOp(o, arg) -> unary_op o (eval arg env)
    | BinOp(o, lhs, rhs) -> binary_op o (eval lhs env) (eval rhs env)
    | BitOp(o, lhs, rhs) -> bitwise_op o (eval lhs env) (eval rhs env)
    | Head xs -> head exp
    | Tail xs -> tail exp
    | Cons(x, xs) -> cons x xs
    | Append(xs1, xs2) -> append xs1 xs2
    | Length exp -> length (eval exp env)
    | At(xs, i) -> at xs i
    | Show exp -> pprint exp
    | Range(f, t) -> range f t
    | Rnd i -> random i
    | Cast(f, t) -> cast f t
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
and condition exp env =
  match exp with
      If(p, c, a) -> begin match (eval p env) with
          Bool b -> if b then (eval c env) else (eval a env)
        | _ -> raise Type_mismatch
      end
    | _ -> raise Type_mismatch

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

let iff = If(Bool(false), BinOp(Add, Symbol("x"), Symbol("z")), Int(-1))
let where = Def(Symbol("z"), Int(4)) :: [] ;;
let func = Closure(Symbol("x") :: [], iff, where, []) ;;
let f1 = Def(Symbol("f1"), func) ;;
let a = Apply(Symbol("f1"), Symbol("x") :: []) ;;

let e = f1 :: x :: y :: [] ;;
let result = eval a e ;;
pprint(result) ;;

let xs = Def(Symbol("xs"), Cons(Int(-1), List([Int(0) ; Char('a') ; Float(10.3) ; Int(12)]))) ;;
let e1 = xs :: e ;;
let op = Length(Symbol("xs")) ;;
let lxs = eval op e1 ;;
pprint(List([Int(0) ; Char('a') ; Float(10.3) ; Int(12)])) ;;
pprint(List([Int(0) ; Char('a') ; List([Float(10.3) ; Int(4)]) ; Int(12)])) ;;

let xs2 = Range(Int(1), Int(10)) ;;
pprint(eval xs2 e) ;;

let i = Int 12 ;;
let fl = Float 12.0 ;;
let cast = Cast(i, fl) ;;
let fl2 = (eval cast []) ;;
