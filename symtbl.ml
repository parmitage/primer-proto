let index = ref 0

let symtab : (string, int) Hashtbl.t = Hashtbl.create 100

let intern str =
  let member = Hashtbl.mem symtab str in
  let idx = !index in
  if not member then Hashtbl.add symtab str idx;
  if not member then index := !index + 1;
  Hashtbl.find symtab str
