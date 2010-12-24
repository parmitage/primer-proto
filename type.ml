exception Type_mismatch
exception Symbol_unbound
exception Symbol_redefined

type binop = Add | Sub | Mul | Div | Mod | Eq | Ne | Lt | Gt | Gte | Lte | And | Or
type uniop = Not | Neg | Bnot
type bitop = Band | Bor | Xor | LShift | RShift

type expression =
  | Symbol of string
  | Int of int
  | Float of float
  | Char of char
  | Bool of bool
  | String of string
  | List of expression list
  | Empty
  | If of expression * expression * expression
  | Lambda of expression list * expression * expression list
  | Closure of expression list * expression * expression list * expression list
  | Apply of expression * expression list
  | BinOp of binop * expression * expression
  | UniOp of uniop * expression
  | BitOp of bitop * expression * expression
  | Head of expression
  | Tail of expression
  | Cons of expression * expression
  | Append of expression * expression
  | Length of expression
  | At of expression * expression
  | Show of expression
  | Range of expression * expression
  | Rnd of expression
  | Cast of expression * expression
  | Def of expression * expression

