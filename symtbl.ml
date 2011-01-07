let index = ref 0

let symtab : (string, int) Hashtbl.t = Hashtbl.create 100

let intern str =
  let idx = !index in
  if not (Hashtbl.mem symtab str)
  then Hashtbl.add symtab str idx ; 
  index := !index + 1;
  Hashtbl.find symtab str
