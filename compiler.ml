(* TODO base library *)
(* TODO output to JS lacks prelude *)
(* TODO what extension to use for LLVM *)
(* TODO C backend *)
(* TODO LLVM backend *)

open Printf
open Type
open Utils
open Javascript16

exception Target_unknown
exception Target_not_implemented

type target = JS | HTML | C | LLVM

let infile = if Array.length Sys.argv > 1 then Sys.argv .(1) else ""
let outfile = if Array.length Sys.argv > 2 then Sys.argv .(2) else ""

let lexbuf =
  try
    Lexing.from_channel (open_in infile)
  with
    | Sys_error _ -> error "file not found"; exit 0

let detect_target filename =
    let filename_and_extension = split_str "." filename in
    match Utils.last filename_and_extension
    with
      | "js"   -> JS
      | "html" -> HTML
      | "c"    -> C
      | "llvm" -> LLVM
      | _      -> raise Target_unknown

let check_args () =
  let argc = Array.length Sys.argv in
  match argc
  with
    | 3 -> Sys.file_exists infile
    | _ -> false
  
let rec compile_to_javascript chan =
  try
    let exp = Parser.main Lexer.token lexbuf in
    let result = Javascript16.eval exp "" in
    fprintf chan "%s\n" result;
    ignore (compile_to_javascript chan)
  with
    | Type_mismatch -> error "type mismatch"
    | Parsing.Parse_error -> error "parse error"
    | Lexer.Eof -> ()

let compile_to_html chan =
  let title = Printf.sprintf "Output of %s" infile in
  let header1 = Printf.sprintf "<html><head><title>%s</title>" title in
  let header2 = "<script language=javascript>" in
  let header3 = "</script></head>" in
  let body = "<body></body></html>" in
  fprintf chan "%s\n" (header1 ^ header2);
  fprintf chan "%s\n" (Javascript16.prelude ());
  compile_to_javascript chan;
  fprintf chan "%s\n" (header3 ^ body)
      
let rec compile () =
  match check_args ()
  with
    | true ->
      let outfile_name = outfile in
      let backend = detect_target outfile_name in
      let outfile_channel = open_out outfile_name in
      ignore begin match backend
        with 
          | JS   -> compile_to_javascript outfile_channel
          | HTML -> compile_to_html outfile_channel
          | C    -> raise Target_not_implemented
          | LLVM -> raise Target_not_implemented
      end;
      close_out outfile_channel
    | false ->
      Format.print_string "usage: prc in.pri out.{js|html|c}\n";
      exit 0

let _ = compile () ;;
