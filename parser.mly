%{
  open Type
%}

%token <int> INT
%token <float> FLOAT
%token <char> CHAR
%token <string> SYMBOL STRING
%token PLUS MINUS TIMES DIV
%token LPAREN RPAREN LSQUARE RSQUARE COMMA SEMICOLON
%token PROG DEF LAMBDA LET VAL IN IF THEN ELSE ELIF COND APPLY
%token LT GT GE LE NE EQ NOT AND OR MOD APPEND TRUE FALSE END LIST
%token HEAD TAIL SHOW RND TYPE IS AS LENGTH AT CONS WHERE RANGE
%token B_AND B_OR B_XOR B_NOT B_LSHIFT B_RSHIFT
%token EOL

%nonassoc ELSE
%left DEF LET VAL IN LPAREN RPAREN NOT B_NOT
%left AND OR APPEND
%left LT GT GE LE EQ NE RANGE
%left PLUS MINUS
%left TIMES DIV MOD AT AS IS
%left B_AND B_OR B_XOR B_LSHIFT B_RSHIFT
%right CONS
%nonassoc UMINUS

%start main
%type <Type.expression> main

%%

main:
expr SEMICOLON                                          { $1 }
  ;

expr:
  | LPAREN expr RPAREN                                  { $2 }
  | INT                                                 { Int $1 }
  | FLOAT                                               { Float $1 }
  | CHAR                                                { Char $1 }
  | TRUE                                                { Bool true }
  | FALSE                                               { Bool false }
  | STRING                                              { String $1 }
  | identifier                                          { $1 }
  | LET identifier DEF expr IN expr                     { Let($2, $4, $6) }
  | VAL identifier DEF expr                             { Def($2, $4) }
  | LAMBDA LPAREN list RPAREN expr                      { Lambda($3, $5) }
  | identifier LPAREN list RPAREN                       { Apply($1, $3) }
  | IF expr THEN expr ELSE expr                         { If($2, $4, $6) }
  | expr CONS expr                                      { BinOp(Cons, $1, $3) }
  | SHOW LPAREN expr RPAREN                             { Show $3 }
  | LENGTH LPAREN expr RPAREN                           { Length $3 }
  | HEAD LPAREN expr RPAREN                             { Head $3 }
  | TAIL LPAREN expr RPAREN                             { Tail $3 }
  | RND LPAREN expr RPAREN                              { Rnd $3 }
  | expr PLUS expr                                      { BinOp(Add, $1, $3) }
  | expr MINUS expr                                     { BinOp(Sub, $1, $3) }
  | expr TIMES expr                                     { BinOp(Mul, $1, $3) }
  | expr DIV expr                                       { BinOp(Div, $1, $3) }
  | expr LT expr                                        { BinOp(Lt, $1, $3) }
  | expr GT expr                                        { BinOp(Gt, $1, $3) }
  | expr GE expr                                        { BinOp(Gte, $1, $3) }
  | expr LE expr                                        { BinOp(Lte, $1, $3) }
  | expr NE expr                                        { BinOp(Ne, $1, $3) }
  | expr EQ expr                                        { BinOp(Eq, $1, $3) }
  | expr AND expr                                       { BinOp(And, $1, $3) }
  | expr OR expr                                        { BinOp(Or, $1, $3) }
  | expr B_AND expr                                     { BitOp(Band, $1, $3) }
  | expr B_OR expr                                      { BitOp(Bor, $1, $3) }
  | expr B_XOR expr                                     { BitOp(Xor, $1, $3) }
  | B_NOT expr                                          { UniOp(Bnot, $2) }
  | expr B_LSHIFT expr                                  { BitOp(LShift, $1, $3) }
  | expr B_RSHIFT expr                                  { BitOp(RShift, $1, $3) }
  | NOT expr                                            { UniOp(Not, $2) }
  | expr MOD expr                                       { BinOp(Mod, $1, $3) }
  | expr APPEND expr                                    { BinOp(App, $1, $3) }
  | expr RANGE expr                                     { BinOp(Rge, $1, $3) }
  | expr IS expr                                        { Is($1, $3) }
  | expr AT expr                                        { At($1, $3) }
  | expr AS expr                                        { Cast($1, $3) }
  | MINUS expr %prec UMINUS                             { UniOp(Neg, $2) }
  | LSQUARE list RSQUARE                                { List $2 }
;

identifier:
  SYMBOL                                                { Symbol $1 }
;

list:
  expr                                                  { [$1] }
  | expr COMMA list                                     { $1 :: $3 }
  | /* empty list */                                    { [] }
;
