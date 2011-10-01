let index = ref 0

(* forward lookups of string -> symbol *)
let symtab : (string, int) Hashtbl.t = Hashtbl.create 100

(* reverse lookups of symbol -> string *)
let stringtab : (int, string) Hashtbl.t = Hashtbl.create 100

let intern str =
  let member = Hashtbl.mem symtab str in
  let idx = !index in
  if not member
  then
    begin
      Hashtbl.add symtab str idx;
      Hashtbl.add stringtab idx str;
    end;
  if not member then index := !index + 1;
  Hashtbl.find symtab str

let lookup_sym sym = Hashtbl.find stringtab sym
