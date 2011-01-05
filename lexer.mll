{
  open Type
  open Parser
  exception Eof
}

let digit = ['0'-'9']
let ident = ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_']*
let text = ['a'-'z' 'A'-'Z' '0'-'9' ' ' '-' '_' '<' '>' '=' '+' '*' '/' '&' '|'
            '%' '{' '}' '.' ';' ':' '(' ')' '^' '%' '$' ',' '\\' '[' ']' '!']*
let whitespace = [' ' '\t' '\n']

rule token = parse
  | "(*" text* "*)"               { token lexbuf }
  | "++"                          { APPEND }
  | '+'                           { PLUS }
  | '-'                           { MINUS }
  | '*'                           { TIMES }
  | '/'                           { DIV }
  | '('                           { LPAREN }
  | ')'                           { RPAREN }
  | '['                           { LSQUARE }
  | ']'                           { RSQUARE }
  | '='                           { DEF }
  | "not"                         { NOT }
  | '<'                           { LT }
  | '>'                           { GT }
  | ">="                          { GE }
  | "<="                          { LE }
  | "=="                          { EQ }
  | "!="                          { NE }
  | "and"                         { AND }
  | "or"                          { OR }
  | ".."                          { RANGE }
  | "mod"                         { MOD }
  | ','                           { COMMA }
  | ';'                           { SEMICOLON }
  | '&'                           { B_AND }
  | '|'                           { B_OR }
  | '^'                           { B_XOR }
  | '~'                           { B_NOT }
  | "<<"                          { B_LSHIFT }
  | ">>"                          { B_RSHIFT }
  | "if"                          { IF }
  | "then"                        { THEN }
  | "else"                        { ELSE }
  | "let"                         { LET }
  | "val"                         { VAL }
  | "in"                          { IN }
  | "fn"                          { LAMBDA }
  | "where"                       { WHERE }
  | "true"                        { TRUE }
  | "false"                       { FALSE }
  | "is"                          { IS }
  | "as"                          { AS }
  | "at"                          { AT }
  | "::"                          { CONS }
  | "Head"                        { HEAD }
  | "Tail"                        { TAIL }
  | "Show"                        { SHOW }
  | "Rnd"                         { RND }
  | "Type"                        { TYPE }
  | "Length"                      { LENGTH }
  | ''' text ''' as lxm           { CHAR(String.get lxm 1) }
  | '"' text* '"' as lxm          { STRING(String.sub lxm 1 (String.length lxm - 2)) }
  | whitespace                    { token lexbuf }
  | digit+ as lxm                 { INT(int_of_string lxm) }
  | digit+ '.' digit+ as lxm      { FLOAT(float_of_string lxm) }
  | ident as lxm                  { SYMBOL(lxm) }
  | eof                           { raise Eof }
