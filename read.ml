open Type
open Eval

let missing_base_lib =
  "Unable to load base library. Ensure PRIMER_LIBRARY_PATH is set."

let base_library =
  let lib_dir =
    try
      Sys.getenv "PRIMER_LIBRARY_PATH"
    with
      | Sys_error _ -> error missing_base_lib; exit 0
  in Filename.concat lib_dir "base.pri"

let interactive = Array.length Sys.argv == 1

let lexbuf =
  if interactive
  then Lexing.from_channel stdin
  else
    try
      Lexing.from_channel (open_in Sys.argv .(1))
    with
      | Sys_error _ -> error "file not found"; exit 0

let rec repl env =
  if interactive then Format.print_string "> "; Format.print_flush();
  try
    let result = Parser.main Lexer.token lexbuf in
    match result with
      | Def(s, e) -> repl (Def(s, e) :: env)
      | _ -> ignore(show (eval result env)); repl env
  with
    | Symbol_unbound -> error "unbound symbol"; repl env
    | Type_mismatch -> error "type mismatch"; repl env
    | Invalid_cast -> error "invalid cast"; repl env
    | Parsing.Parse_error -> error "parse error"; repl env
    | Lexer.Eof -> exit 0

let rec load buf env =
  try let result = Parser.main Lexer.token buf in
      match result with
        | Def(s, e) -> load buf (Def(s, e) :: env)
        | _ -> load buf env
  with Lexer.Eof -> env

let _ =
  Random.self_init();
  let toplevel = initial_toplevel [] in
  let prelude = load (Lexing.from_channel (open_in base_library)) toplevel in
  repl prelude ;;
