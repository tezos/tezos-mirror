%{
%}

%token EOF
%token LBRACE RBRACE LBRACKET RBRACKET COMMA COLON
%token NULL
%token <bool> BOOL
%token <float> FLOAT
%token <string> STRING

%type <JSON_AST.t> json
%start json
%%

json:
| NULL { `Null }
| BOOL { `Bool $1 }
| FLOAT { `Float $1 }
| STRING { `String $1 }
| LBRACE fields RBRACE { `O $2 }
| LBRACE RBRACE { `O [] }
| LBRACKET items RBRACKET { `A $2 }
| LBRACKET RBRACKET { `A [] }

fields:
| STRING COLON json COMMA fields { ($1, $3) :: $5 }
| STRING COLON json { [ $1, $3 ] }

items:
| json COMMA items { $1 :: $3 }
| json { [ $1 ] }
