(* TODO
   - definition_eq being passed two symbols - currently matched as a hack...
   - strings are not tokenised
   - primer syntax modified to fit parser...
   - staggering number of parser conflicts!
   - loadlib
   - string equality tests
   - interned type names (int, char, etc)
   - is operator
   - symbol interning (or will OCaml do it?)
   - newline, tab
   - unify strings and lists as in primer1?
   - will tail calls be eliminated in apply/condition?
   - environment.lookup is inefficient as it does a double scan of the environment
   - evlis is inefficient - doesn't need to evaluate everything
   - move (--) into utils module
   - move operators and specials into a module?
   - better error reporting
*)

open Type

let (--) i j = 
  let rec aux n acc =
    if n < i then acc else aux (n-1) (n :: acc)
  in aux j []

let rec pprint exp =
  match exp with
    | Symbol s -> Format.print_string s
    | Int i -> Format.print_int i
    | Float f -> Format.print_float f
    | Char c -> Format.print_char c
    | Bool b -> Format.print_bool b
    | String s -> Format.print_string s
    | List l -> pprint_list l
    | Empty -> Format.print_string "[]"
    | Lambda(p, b) -> Format.print_string "#<lambda>"
    | Closure(p, b, e) -> Format.print_string "#<closure>"
    | _ -> Format.print_string "#<builtin>"
and pprint_list l =
  Format.print_char '[';
  ignore (List.map pprint (Utils.intersperse (String ", ") l)) ;
  Format.print_char ']'

module Environment =
struct
  let marker = Symbol "env"
  let symbol_eq sym1 sym2 = match sym1, sym2 with
      Symbol str1, Symbol str2 -> str1 = str2
    | _ -> pprint sym1; pprint sym2; raise Type_mismatch
  let definition_eq sym def = match sym, def with
      Symbol str1, Def(sym2, exp) -> symbol_eq sym sym2
    | Symbol s1, Symbol s2 -> s1 = s2 (* should never match...! *)
    | _ -> raise Type_mismatch
  let symbol_bound sym env = List.exists (fun b -> definition_eq sym b) env
  let lookup sym env =
    if symbol_bound sym env
    then match List.find (fun b -> definition_eq sym b) env with
        Def(s, v) -> v
      | _ -> raise Type_mismatch
    else raise Symbol_unbound
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

let at lst idx = match lst, idx with
  | List l, Int i -> List.nth l i
  | _ -> raise Type_mismatch

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
    | Let(d, e) -> plet d e env
    | Lambda(p, b) -> Closure(p, b, env)
    | Closure(p, b, e) -> exp
    | Def(s, e) -> exp
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
    | Show exp -> show exp env
    | Range(f, t) -> range f t
    | Rnd i -> random i
    | Cast(f, t) -> cast f t
and apply sym args env =
  match sym with
      Symbol s ->
        let f = Environment.lookup sym env in
        begin match f with
            Closure(p, b, ce) ->
              eval b (Environment.bind p args ce)
          | _ -> raise Type_mismatch
        end
    | _ -> raise Type_mismatch
and evlis lst env = List.map (fun exp -> eval exp env) lst
and plet def exp env = match def with
    Def(s, v) -> eval exp (Def(s, (eval v env)) :: env)
  | _ -> raise Type_mismatch
and condition exp env =
  match exp with
      If(p, c, a) -> begin match (eval p env) with
          Bool b -> if b then (eval c env) else (eval a env)
        | _ -> raise Type_mismatch
      end
    | _ -> raise Type_mismatch
and show exp env =
  let result = eval exp env in
  pprint result; Format.print_newline(); result

let error msg = Format.printf "@[error: %s@]@." msg
let interactive = Array.length Sys.argv == 1

let lexbuf =
  if interactive
  then Lexing.from_channel stdin
  else Lexing.from_channel (open_in Sys.argv .(1))

let rec repl env =
  if interactive then Format.print_string "> "; Format.print_flush();
  let result = Parser.main Lexer.token lexbuf in
  try match result with
      Def(s, e) -> repl (Def(s, (eval e env)) :: env)
    | _ -> ignore(show (eval result env) env); repl env
  with
      Symbol_unbound -> error "unbound symbol"; repl env
    | Type_mismatch -> error "type mismatch"; repl env

let _ = repl [] ;;
