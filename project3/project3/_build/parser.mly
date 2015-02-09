%{
open Ast;;
%}

/* Declare your tokens here. */
%token EOF
%token <Range.t * int32> INT
%token <Range.t> X        /* X */
%token <Range.t> TIMES
%token <Range.t> PLUS
/*%token <Range.t> MINUS*/
%token <Range.t> SHL
%token <Range.t> SHR
%token <Range.t> SAR
%token <Range.t> LT
%token <Range.t> LTE
%token <Range.t> GT
%token <Range.t> GTE
%token <Range.t> EQ
%token <Range.t> NEQ
%token <Range.t> AND
%token <Range.t> OR
/*%token <Range.t> NEG*/UNARY_OR_BIN_MINUS
%token <Range.t> UNARY_OR_BIN_MINUS
%token <Range.t> LOGNOT
%token <Range.t> NOT
%token <Range.t> LPAREN   
%token <Range.t> RPAREN

/* ---------------------------------------------------------------------- */
%start toplevel
%type <Ast.exp> toplevel
%type <Ast.exp> exp

%left OR
%left AND
%left EQ NEQ
%left LT LTE GT GTE
%left SHL SHR SAR
%left PLUS UNARY_OR_BIN_MINUS
%left TIMES
%nonassoc LOGNOT NOT

%%

toplevel:
  | exp EOF { $1 }

/* Declare your productions here, starting with 'exp'. */

exp:
  | X   { Arg }
  | INT { Cint (snd $1) }
  | exp PLUS exp { Binop(Plus,$1,$3) }
  | exp UNARY_OR_BIN_MINUS exp { Binop(Minus,$1,$3) }
  | exp TIMES exp { Binop(Times,$1,$3) }
  | exp EQ exp { Binop(Eq,$1,$3) }
  | exp SHL exp { Binop(Shl,$1,$3) }
  | exp SHR exp { Binop(Shr,$1,$3) }
  | exp SAR exp { Binop(Sar,$1,$3) }
  | exp NEQ exp { Binop(Neq,$1,$3) }
  | exp LT exp { Binop(Lt,$1,$3) }
  | exp LTE exp { Binop(Lte,$1,$3) }
  | exp GT exp { Binop(Gt,$1,$3) }
  | exp GTE exp { Binop(Gte,$1,$3) }
  | exp AND exp { Binop(And,$1,$3) }
  | exp OR exp { Binop(Or,$1,$3) }
  | LOGNOT exp { Unop (Lognot,$2) }
  | NOT exp { Unop (Not,$2) }
  | UNARY_OR_BIN_MINUS exp { Unop (Neg,$2) }
  | LPAREN exp RPAREN { $2 }
  
