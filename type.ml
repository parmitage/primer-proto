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
