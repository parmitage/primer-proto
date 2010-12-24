%{
open Type
%}

%token <int> INT
%token PLUS MINUS TIMES DIV
%token LPAREN RPAREN
%token EOL
%left PLUS MINUS        /* lowest precedence */
%left TIMES DIV         /* medium precedence */
%nonassoc UMINUS        /* highest precedence */
%start main             /* the entry point */
%type <Type.expression> main

%%

main:
expr EOL                           { $1 }
  ;

expr:
  INT                              { Int $1 }
  | expr PLUS expr                 { BinOp(Add, $1, $3) }
  | expr MINUS expr                { BinOp(Sub, $1, $3) }
  | expr TIMES expr                { BinOp(Mul, $1, $3) }
  | expr DIV expr                  { BinOp(Div, $1, $3) }
;
