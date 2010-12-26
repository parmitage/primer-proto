{
open Type
open Parser
exception Eof
}
rule token = parse
  | "++"                                              { APPEND }
  | '+'                                               { PLUS }
  | '-'                                               { MINUS }
  | '*'                                               { TIMES }
  | '/'                                               { DIV }
  | '('                                               { LPAREN }
  | ')'                                               { RPAREN }
  | '['                                               { LSQUARE }
  | ']'                                               { RSQUARE }
  | ':'                                               { DEF }
  | "not"                                             { NOT }
  | '<'                                               { LT }
  | '>'                                               { GT }
  | ">="                                              { GE }
  | "<="                                              { LE }
  | "=="                                              { EQ }
  | "!="                                              { NE }
  | "and"                                             { AND }
  | "or"                                              { OR }
  | ".."                                              { RANGE }
  | "mod"                                             { MOD }
  | ','                                               { COMMA }
  | ';'                                               { SEMICOLON }
  | '&'                                               { B_AND }
  | '|'                                               { B_OR }
  | '^'                                               { B_XOR }
  | '~'                                               { B_NOT }
  | "<<"                                              { B_LSHIFT }
  | ">>"                                              { B_RSHIFT }
  | "if"                                              { IF }
  | "then"                                            { THEN }
  | "else"                                            { ELSE }
  | "let"                                             { LET }
  | "in"                                              { IN }
  | "fn"                                              { LAMBDA }
  | "where"                                           { WHERE }
  | "true"                                            { TRUE }
  | "false"                                           { FALSE }
  | "end"                                             { END }
  | "is"                                              { IS }
  | "as"                                              { AS }
  | "at"                                              { AT }
  | "::"                                              { CONS }
  | "Head"                                            { HEAD }
  | "Tail"                                            { TAIL }
  | "Show"                                            { SHOW }
  | "Rnd"                                             { RND }
  | "Type"                                            { TYPE }
  | "Length"                                          { LENGTH }
  | '''['a'-'z''A'-'Z''0'-'9']''' as lxm              { CHAR(String.get lxm 0) }
  | "sss" as lxm                                      { STRING(lxm) }
  | [' ' '\t' '\n']                                   { token lexbuf } (* skip blanks *)
  | ['0'-'9']+ as lxm                                 { INT(int_of_string lxm) }
  | ['0'-'9']+"."['0'-'9']+ as lxm                    { FLOAT(float_of_string lxm) }
  | ['a'-'z''A'-'Z']['a'-'z''A'-'Z''0'-'9']* as lxm   { SYMBOL(lxm) }
  | eof                                               { print_newline(); flush stdout; exit 0 }
