exception Type_mismatch
exception Symbol_unbound
exception Symbol_redefined
exception Invalid_cast
exception Internal_error

type typename = TInt | TFloat | TChar | TBool | TList | TString | TLambda
type uniop = Not | Neg | Bnot
type bitop = Band | Bor | Xor | LShift | RShift
type binop = Add | Sub | Mul | Div | Mod | Eq | Ne | Lt | Gt | Gte | Lte | And
             | Or | App | Rge | Cons

type expression =
  | Symbol of int
  | Int of int
  | Float of float
  | Char of char
  | Bool of bool
  | String of string
  | List of expression list
  | If of expression * expression * expression
  | Let of expression * expression * expression
  | Lambda of expression list * expression
  | Closure of expression list * expression * expression list
  | Apply of expression * expression list
  | BinOp of binop * expression * expression
  | UniOp of uniop * expression
  | BitOp of bitop * expression * expression
  | Head of expression
  | Tail of expression
  | Length of expression
  | At of expression * expression
  | Is of expression * expression
  | Show of expression
  | Rnd of expression
  | Cast of expression * expression
  | Def of expression * expression
  | Type of typename

module Symtbl = struct
  let index = ref 0
  let symtab : (string, int) Hashtbl.t = Hashtbl.create 100
  let intern str =
    let idx = !index in
    if not (Hashtbl.mem symtab str)
    then Hashtbl.add symtab str idx ; 
    index := !index + 1;
    Hashtbl.find symtab str
end

module Environment = struct
  let symbol_eq s1 s2 = match s1, s2 with
    | Symbol sym1, Symbol sym2 -> sym1 = sym2
    | _ -> raise Type_mismatch
  let definition_eq sym def = match def with
    | Def(sym2, exp) -> symbol_eq sym sym2
    | _ -> raise Type_mismatch
  let symbol_bound sym env = List.exists (fun b -> definition_eq sym b) env
  let lookup sym env =
    if symbol_bound sym env
    then match List.find (fun b -> definition_eq sym b) env with
      | Def(s, v) -> v
      | _ -> raise Type_mismatch
    else raise Symbol_unbound
  let bind p a e = List.append (List.map2 (fun sym a -> Def(sym, a)) p a) e
end
