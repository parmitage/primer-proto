(* -- compiler backend for JavaScript 1.6 -- *)

open Type
open Utils

let comma = Symtbl.intern(",")
let type_int = Symtbl.intern("int")
let type_float = Symtbl.intern("float")
let type_char = Symtbl.intern("char")
let type_bool = Symtbl.intern("bool")
let type_string = Symtbl.intern("string")
let type_list = Symtbl.intern("list")
let type_lambda = Symtbl.intern("lambda")

let quote str = "\"" ^ str ^ "\""

let emit exp =
  match exp with
    | Int i    -> string_of_int i
    | Float f  -> string_of_float f
    | Bool b   -> string_of_bool b
    | Char c   -> "\"" ^ String.make 1 c ^ "\""
    | String s -> quote s
    | Symbol s -> Symtbl.lookup_sym s
    | Any      -> "true"
    | _        -> raise Type_mismatch

let uniop_sym op =
  match op with
    | Not  -> "!"
    | Neg  -> "-"
    | Bnot -> "~"

let binop_sym op =
  match op with
    | Add  -> "+"
    | Sub  -> "-"
    | Mul  -> "*"
    | Div  -> "/"
    | Mod  -> "%"
    | Lt   -> "<"
    | Gt   -> ">"
    | Gte  -> ">="
    | Lte  -> "<="
    | And  -> "&&"
    | Or   -> "||"
    | _    -> raise Type_mismatch

let bitop_sym op =
  match op with
    | Band   -> "&"
    | Bor    -> "|"
    | Xor    -> "^"
    | LShift -> "<<"
    | RShift -> ">>"

(* TODO this makes no explicit attempt to eliminate tail calls yet all tested
   JS engines seem to handle deeply recursive calls without blowing stack? *)
let rec eval exp str = 
  match exp with
    | Int _ | Float _
    | Char _ | Bool _ | Any
    | String _ | Symbol _ -> str ^ emit exp
    | List l              -> str ^ list l
    | If(p, c, a)         -> str ^ condition p c a
    | Let(s, e1, e2)      -> str ^ plet s e1 e2
    | Lambda(p, b)        -> str ^ lambda p b
    | Def(s, e)           -> str ^ def s e
    | Apply(s, a)         -> str ^ apply s a
    | UniOp(o, arg)       -> str ^ uniop o arg
    | BinOp(o, lhs, rhs)  -> str ^ binop lhs o rhs
    | BitOp(o, lhs, rhs)  -> str ^ bitop lhs o rhs
    | Head xs             -> str ^ head xs
    | Tail xs             -> str ^ tail xs
    | Length xs           -> str ^ length xs
    | At(l, x)            -> str ^ index l x
    | Show x              -> str ^ show x
    | Rnd x               -> str ^ rnd x
    | Is(exp, typ)        -> str ^ is exp typ
    | Cast(exp, typ)      -> str ^ cast exp typ
    | Match(t, c)         -> str ^ matches t c
    | _                   -> raise Type_mismatch

and eval1 exp =
  eval exp ""

and def sym exp =
  "var " ^ eval1 sym ^ " = " ^ eval1 exp ^ ";"

(* JavaScript 1.6 doesn't have let blocks so simulate with a lambda *)
and plet sym exp1 exp2 =
  "(function () {\n   var " ^ eval1 sym ^ " = " ^ eval1 exp1 ^
    ";\n   return " ^ eval1 exp2 ^ ";\n})()\n"

and lambda params body =
  "function (" ^
   String.concat "," (Utils.map eval1 params) ^
    ") {\n   return " ^ eval1 body ^ ";\n}"

and uniop op exp =
  uniop_sym op ^ eval1 exp

(* not all Primer operators have a direct Javascript equivalent *)
and binop lhs op rhs =
  match op with
    | Eq   -> eq lhs rhs
    | Ne   -> neq lhs rhs
    | Rge  -> range lhs rhs
    | Cons -> cons lhs rhs
    | App  -> append lhs rhs
    | _    -> "(" ^ eval1 lhs ^ (binop_sym op) ^ eval1 rhs ^ ")"

(* the precedence of & and << is inverted between Primer and JS *)
and bitop lhs op rhs =
  "(" ^ eval1 lhs ^ (bitop_sym op) ^ eval1 rhs ^ ")"

(* JavaScript doesn't support array equality *)
and eq lhs rhs =
  "equals(" ^ eval1 lhs ^ "," ^ eval1 rhs ^ ")"

and neq lhs rhs =
  "!equals(" ^ eval1 lhs ^ "," ^ eval1 rhs ^ ")"

(* can't take the head of an empty array in JavaScript *)
and head l =
  "head(" ^ eval1 l ^ ")"

and tail l =
  eval1 l ^ ".slice(1)"

and length l =
  eval1 l ^ ".length"

and index l x =
  eval1 l ^ "[" ^ eval1 x ^ "]"

and apply sym args =
  eval1 sym ^ "(" ^
    String.concat "," (Utils.map eval1 args) ^ ")"

and list l =
  "[" ^ String.concat "," (Utils.map eval1 l) ^ "]"

(* TODO show should return its argument - does document.write ? *)
and show x =
  "document.write(" ^ eval1 x ^ " + \"<br/>\")"

and rnd x =
  "Math.floor(Math.random() * (" ^
    eval1 (BinOp(Add, x, Int(1))) ^ "))"

(* all conditionals are two legged so compile to ternary operators *)
and condition pred cond alt =
  "(" ^ eval1 pred ^ ") ? (" ^ eval1 cond ^ ") : (" ^ eval1 alt ^ ")"

and range lhs rhs =
  "range(" ^ eval1 lhs ^ "," ^ eval1 rhs ^ ")"

and cons lhs rhs =
  "cons(" ^ eval1 lhs ^ "," ^ eval1 rhs ^ ")"

and append lhs rhs =
  eval1 lhs ^ ".concat(" ^ eval1 rhs ^ ")"

and is lhs typ =
  let js_type_name =
    match typ with
      | Symbol s     ->
        begin match (Symtbl.lookup_sym s) with
          | "int"  | "float"  -> quote "number"
          | "char" | "string" -> quote "string"
          | "bool"            -> quote "boolean"
          | "list"            -> quote "object"
          | _                 -> quote "undefined"
        end
      | _                     -> quote "undefined"
  in
  "(typeof " ^ eval1 lhs ^ " == " ^ js_type_name ^ ")"

and cast exp typ =
  match typ with
    | Symbol s ->
      begin match exp, (Symtbl.lookup_sym s) with
        | _,        "string" -> "String(" ^ eval1 exp ^ ")"
        | _,        "bool"   -> "Boolean(" ^ eval1 exp ^ ")"
        | Char _,   "int"    -> "charCode(" ^ eval1 exp ^ ")"
        | String s, "int"    -> "Math.round(Number(" ^ eval1 exp ^ "))"
        | _,        "int"    -> "Math.round(Number(" ^ eval1 exp ^ "))"
        | _,        "float"  -> eval1 exp
        | _,        "char"   -> eval1 exp ^ "[0]"
        | _,        _        -> raise Invalid_cast
      end
    | _        -> raise Invalid_cast

and matches t c =
  let rec pattern p t =
    match p, t with
      | Any :: [], _ ->
        "true"
      | Any :: pr, th :: tr ->
        "true && " ^ pattern pr tr 
      | ph :: [], th :: [] ->
        eval1 ph ^ " == " ^ eval1 th
      | ph :: pr, th :: tr ->
        eval1 ph ^ " == " ^ eval1 th ^ " && " ^ pattern pr tr
      | _                  -> ""
  in
  match c with
    | ch :: cr ->
      begin match ch with
        | Pattern(p, e) -> 
          pattern p t ^ " ? " ^ eval1 e ^ " : " ^ matches t cr
      end
    | ch      ->
      quote "Non-exhaustive pattern"

let prelude =
  let path = Filename.concat
    Utils.base_library_directory "primer.js"
  in Utils.read_file_as_string path
