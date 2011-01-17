open Type
open Utils

let symbol_eq s1 s2 = match s1, s2 with
  | Symbol sym1, Symbol sym2 -> sym1 = sym2
  | _ -> raise Type_mismatch

let definition_eq sym def = match def with
  | Def(sym2, exp) -> symbol_eq sym sym2
  | _ -> raise Type_mismatch

let symbol_bound sym env = List.exists (fun b -> definition_eq sym b) env

let lookup sym env =
  try match List.find (fun b -> definition_eq sym b) env with
    | Def(s, v) -> v
    | _ -> raise Type_mismatch
  with _ -> raise Symbol_unbound

let bind params args env =
  let bind_one sym exp env = Def(sym, exp) :: env in
  let rebind_one sym exp env =
    replace_one_by definition_eq sym (Def(sym, exp)) env in
  List.fold_left
    (fun e (sym, exp) ->
      if symbol_bound sym e
      then rebind_one sym exp e
      else bind_one sym exp e)
    env (zip params args)
