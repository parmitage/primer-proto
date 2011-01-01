exception Type_mismatch
exception Symbol_unbound
exception Symbol_redefined
exception Internal_error

type binop = Add | Sub | Mul | Div | Mod | Eq | Ne | Lt | Gt | Gte | Lte | And | Or | App | Rge | Cons
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
  | If of expression * expression * expression
  | Let of expression * expression
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
  | Show of expression
  | Rnd of expression
  | Cast of expression * expression
  | Def of expression * expression
