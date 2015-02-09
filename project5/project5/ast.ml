
type n = int32 (* Constant int *)
type b = bool (* Constant bool *)
type cstr = string (* Constant string *)
type 'a id = 'a * string (* Identifiers *)

type 
typ =  (* Types *)
   TBool (* bool *)
 | TInt (* int *)
 | TString (* string *)
 | TArray of typ (* array *)


type
'a unop = Neg of 'a | Lognot of 'a | Not of 'a


type
'a binop = Plus of 'a | Times of 'a | Minus of 'a
  | Eq of 'a | Neq of 'a
  | Lt of 'a | Lte of 'a | Gt of 'a | Gte of 'a
  | And of 'a | Or of 'a | IAnd of 'a | IOr of 'a
  | Shl of 'a | Shr of 'a | Sar of 'a


type
'a const = Cbool of 'a * b 
 | Cint of 'a * n 
 | Cstring of 'a * cstr


type 
'a exp =  (* Expressions *)
   Const of 'a const (* constant *)
 | Lhs of 'a lhs (* left-hand sides *)
 | New of typ * 'a exp * 'a id * 'a exp (* new array creation *)
 | Binop of 'a binop * 'a exp * 'a exp (* binary arithmetic *)
 | Unop of 'a unop * 'a exp (* unary arithmetic *)
 | Ecall of 'a id * ('a exp) list (* function call *)

and 'a lhs =  (* Left-hand sides *)
   Var of 'a id (* variable *)
 | Index of 'a lhs * 'a exp (* array index *)


type
'a init = Iexp of 'a exp
  | Iarray of 'a * (('a init) list)


type
'a vdecl = {v_ty : typ; v_id : 'a id; v_init : 'a init;}


type
'a opt_exp = ('a exp) option


type
'a vdecls = (('a vdecl) list)


type
rtyp = typ option


type 
'a stmt =  (* Statements *)
   Assign of 'a lhs * 'a exp (* assignment *)
 | Scall of 'a id * ('a exp) list (* function call *)
 | If of 'a exp * 'a stmt * ('a stmt) option (* if-then, optional else *)
 | While of 'a exp * 'a stmt (* while loop *)
 | For of 'a vdecls * 'a opt_exp * ('a stmt) option * 'a stmt (* for loop *)
 | Block of 'a block (* block *)

and 'a stmts = ('a stmt) list


and 'a block = ('a vdecls) * ('a stmts)


type
'a args = (typ * ('a id)) list


type
ftyp = typ list*rtyp


type
'a fdecl = rtyp*('a id)*('a args)*('a block)*('a exp) option


type 
gtyp =  (* Global types *)
   GFn of ftyp (* functions *)
 | GVal of typ (* values *)


type
'a gdecl = Gvdecl of 'a vdecl 
  | Gfdecl of 'a fdecl


type
'a lcontext = (string*typ) list


type
'a gcontext = (string*gtyp) list


type
'a prog = ('a gdecl) list



