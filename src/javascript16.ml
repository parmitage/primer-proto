(* -- compiler backend for JavaScript 1.6 -- *)

(* TODO move the prelude into a primer.js file inside lib? *)
(* TODO tail recursion elimination *)
(* TODO as (Cast) and is (Is) operators *)
(* TODO match statement *)

open Type
open Utils

let comma = Symtbl.intern(",")

let emit exp =
  match exp with
    | Int i    -> string_of_int i
    | Float f  -> string_of_float f
    | Bool b   -> string_of_bool b
    | Char c   -> "'" ^ String.make 1 c ^ "'"
    | String s -> "\"" ^ s ^ "\""
    | Symbol s -> Symtbl.lookup_sym s
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

let rec eval exp str = 
  match exp with
    | Int _ | Float _
    | Char _ | Bool _
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
    | _                   -> raise Type_mismatch

and eval1 exp =
  eval exp ""

and def sym exp =
  "var " ^ eval1 sym ^ " = " ^ eval1 exp ^ ";"

and plet sym exp1 exp2 =
  "(function () {\n   var " ^ eval1 sym ^ " = " ^ eval1 exp1 ^
    ";\n   return " ^ eval1 exp2 ^ ";\n})()\n"

and lambda params body =
  "function (" ^
   String.concat "," (Utils.map eval1 params) ^
    ") {\n   return " ^ eval1 body ^ ";\n}"

and uniop op exp =
  uniop_sym op ^ eval1 exp

and binop lhs op rhs =
  match op with
    | Eq   -> eq lhs rhs
    | Ne   -> neq lhs rhs
    | Rge  -> range lhs rhs
    | Cons -> cons lhs rhs
    | App  -> concat lhs rhs
    | _    -> eval1 lhs ^ (binop_sym op) ^ eval1 rhs

and bitop lhs op rhs =
  eval1 lhs ^ (bitop_sym op) ^ eval1 rhs

and eq lhs rhs =
  "equals(" ^ eval1 lhs ^ "," ^ eval1 rhs ^ ")"

and neq lhs rhs =
  "!equals(" ^ eval1 lhs ^ "," ^ eval1 rhs ^ ")"

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

and show x =
  "document.write(" ^ eval1 x ^ " + \"<br/>\")"

and rnd x =
  "Math.floor(Math.random() * (" ^
    eval1 (BinOp(Add, x, Int(1))) ^ "))"

and condition pred cond alt =
  "(" ^ eval1 pred ^ ") ? (" ^ eval1 cond ^ ") : (" ^ eval1 alt ^ ")"

and range lhs rhs =
  "range(" ^ eval1 lhs ^ "," ^ eval1 rhs ^ ")"

and cons lhs rhs =
  "cons(" ^ eval1 lhs ^ "," ^ eval1 rhs ^ ")"

and concat lhs rhs =
  eval1 lhs ^ ".concat(" ^ eval1 rhs ^ ")"

let prelude () =
  "var equals = function (a, b)
   {
     if (typeof(a) != typeof(b))
        return false;

     if (typeof(a) != 'object')
        return a === b;

     if(!a || !b)
       return false;

     if(a.length == b.length)
     {
        for(var i = 0; i < a.length;i++)
        {
           if(typeof a[i] == 'object') {
              if(!equals(a[i], b[i]))
                 return false;
           }
           else if(a[i] != b[i])
              return false;
        }
    
        return true;
     }
     else
       return false;
    };

    var head = function (xs) {
       if (xs.length == 0)
          return [];

       return xs[0];
    };

    var cons = function (x, l)
    {
       var l2 = l.slice(0);
       l2.unshift(x);
       return l2;
    };

    var range = function (start, end)
    {
       var array = [];
       for (var i = start; i <= end; i += 1) {
          array.push(i);
       }
    
       return array;
    };";
