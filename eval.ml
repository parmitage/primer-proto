(* TODO
   - tests
   - examples
   - rng not seeded
   - symbol interning
   - environments are inefficient due to deep lookups
   - environment.lookup is inefficient as it does a double scan of the environment
   - move (--) into utils module
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
    | _ -> raise Type_mismatch
  let definition_eq sym def = match sym, def with
      Symbol str1, Def(sym2, exp) -> symbol_eq sym sym2
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

let rec eq lhs rhs = match lhs, rhs with 
  | Int x, Int y -> x == y
  | Float x, Float y -> x == y
  | Int x, Float y -> float_of_int x == y
  | Float x, Int y -> x == float_of_int y
  | Char x, Char y -> x == y
  | Bool x, Bool y -> x == y
  | Symbol x, Symbol y -> x == y
  | List xs1, List xs2 -> list_eq xs1 xs2
  | String s1, String s2 -> s1 = s2
  | _, _ -> false
and list_eq xs ys = match xs, ys with
    x::xs, y::ys -> eq x y && list_eq xs ys
  | [], [] -> true
  | _, _ -> false

let rec binary_op oper lhs rhs = match oper, lhs, rhs with
  | Add, Int x, Int y -> Int(x + y)
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
  | Eq, _, _ -> Bool(eq lhs rhs)
  | Ne, _, _ -> Bool(not (eq lhs rhs))
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
  | App, List xs1, List xs2 -> List(List.append xs1 xs2)
  | App, String s1, String s2 -> String(s1 ^ s2)
  | Rge, Int x, Int y -> List(List.map (fun i -> Int(i)) (x -- y))
  | Cons, _, List xs -> List(lhs :: xs)
  | Cons, Char c, String s -> String(String.make 1 c ^ s)
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
        [] -> List []
      | x::xs -> x
    end
  | String s -> Char(String.get s 0)
  | _ -> raise Type_mismatch

let tail exp = match exp with
    List l -> begin match l with
        [] -> List []
      | x::xs -> List(xs)
    end
  | String s -> String(String.sub s 1 ((String.length s) - 1))
  | _ -> raise Type_mismatch

let length exp = match exp with
    List xs -> Int(List.length xs)
  | String s -> Int(String.length s)
  | _ -> raise Type_mismatch

let at lst idx = match lst, idx with
  | List l, Int i -> List.nth l i
  | _ -> raise Type_mismatch

let random exp = match exp with
  | Int i -> Int(Random.int i)
  | _ -> raise Type_mismatch

let cast f t = match f, t with
  | Int i, Type TFloat -> Float(float_of_int i)
  | Int i, Type TString -> String(string_of_int i)
  | Int i, Type TBool -> Bool(if i <= 0 then false else true)
  | Float f, Type TInt -> Int(int_of_float f)
  | Float f, Type TString -> String(string_of_float f)
  | Bool b, Type TInt -> Int(if b then 1 else 0)
  | Bool b, Type TFloat -> Float(if b then 1.0 else 0.0)
  | Bool b, Type TString -> String(if b then "true" else "false")
  | Char c, Type TInt -> Int(int_of_char c)
  | Char c, Type TFloat -> Float(float_of_int (int_of_char c))
  | Char c, Type TString -> String(String.make 1 c)
  | _ -> raise Invalid_cast

let is lhs typ = match lhs, typ with
  | Int _, Type TInt -> Bool(true)
  | Float _, Type TFloat -> Bool(true)
  | Char _, Type TChar -> Bool(true)
  | Bool _, Type TBool -> Bool(true)
  | String _, Type TString -> Bool(true)
  | List _, Type TList -> Bool(true)
  | _, _ -> Bool(false)

let show exp = pprint exp; Format.print_newline(); exp

let is_primitive_list l =
  List.for_all
    (fun e -> match e with
      | Int _ -> true
      | Float _ -> true
      | Char _ -> true
      | Bool _ -> true
      | String _ -> true
      | _ -> false) l

let rec eval exp env =
  match exp with
    | Symbol s -> eval (Environment.lookup exp env) env
    | Int i -> exp
    | Float f -> exp
    | Char c -> exp
    | Bool b -> exp
    | String s -> exp
    | List l -> if is_primitive_list l then exp else List(evlis l env)
    | If(p, c, a) -> condition exp env
    | Let(d, e) -> plet d e env
    | Lambda(p, b) -> Closure(p, b, env)
    | Closure(p, b, e) -> exp
    | Def(s, e) -> eval e env
    | Apply(s, a) -> apply (eval s env) (evlis a env) env
    | UniOp(o, arg) -> unary_op o (eval arg env)
    | BinOp(o, lhs, rhs) -> binary_op o (eval lhs env) (eval rhs env)
    | BitOp(o, lhs, rhs) -> bitwise_op o (eval lhs env) (eval rhs env)
    | Head xs -> head (eval xs env)
    | Tail xs -> tail (eval xs env)
    | Length exp -> length (eval exp env)
    | At(xs, i) -> at (eval xs env) (eval i env)
    | Is(x, t) -> is (eval x env) (eval t env)
    | Show exp -> show (eval exp env)
    | Rnd i -> random (eval i env)
    | Cast(f, t) -> cast (eval f env) (eval t env)
    | Type _ -> exp
and apply f args env = match f with
    Closure(p, b, ce) -> eval b (Environment.bind p args ce)
  | _ -> raise Type_mismatch
and evlis lst env = List.map (fun exp -> eval exp env) lst
and plet def exp env = match def with
    Def(s, v) -> eval exp (Def(s, v) :: env)
  | _ -> raise Type_mismatch
and condition exp env =
  match exp with
      If(p, c, a) -> begin match (eval p env) with
          Bool b -> if b then (eval c env) else (eval a env)
        | _ -> raise Type_mismatch
      end
    | _ -> raise Type_mismatch

let initial_toplevel env = 
  Def(Symbol("int"), Type(TInt))
  :: Def(Symbol("float"), Type(TFloat))
  :: Def(Symbol("char"), Type(TChar))
  :: Def(Symbol("bool"), Type(TBool))
  :: Def(Symbol("string"), Type(TString))
  :: Def(Symbol("list"), Type(TList))
  :: Def(Symbol("lambda"), Type(TLambda))
  :: Def(Symbol("newline"), Char('\n'))
  :: Def(Symbol("tab"), Char('\t'))
  :: env    

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
      Def(s, e) -> repl (Def(s, e) :: env)
    | _ -> ignore(show (eval result env)); repl env
  with
      Symbol_unbound -> error "unbound symbol"; repl env
    | Type_mismatch -> error "type mismatch"; repl env
    | Invalid_cast -> error "invalid cast"; repl env
    | Lexer.Eof -> exit 0

let rec load buf env =
  try let result = Parser.main Lexer.token buf in
      match result with
          Def(s, e) -> load buf (Def(s, e) :: env)
        | _ -> load buf env
  with Lexer.Eof -> env

let _ = repl
  (load (Lexing.from_channel (open_in "Library.pri"))
     (initial_toplevel [])) ;;
