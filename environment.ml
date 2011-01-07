open Type

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
