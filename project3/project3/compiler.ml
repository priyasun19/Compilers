(* compiler.ml *)
(* A compiler for simple arithmetic expressions. *)

(******************************************************************************)

open Printf
open Ast
open X86simplified   

(* Note that Ast has similarly named constructors that must be
              disambiguated.  For example: Ast.Shl vs. X86simplified.Shl *)

(* Parse an AST from a preexisting lexbuf. 
 * The filename is used to generate error messages.
*)
let parse (filename : string) (buf : Lexing.lexbuf) : exp =
  try
    Lexer.reset_lexbuf filename buf;
    Parser.toplevel Lexer.token buf
  with Parsing.Parse_error ->
    failwith (sprintf "Parse error at %s."
        (Range.string_of_range (Lexer.lex_range buf)))



(* Builds a globally-visible X86 instruction block that acts like the C fuction:

   int program(int X) { return <expression>; }

   Follows cdecl calling conventions and platform-specific name mangling policy. *)

let rec emit_exp (e:exp) (stream : insn list) : insn list = 
 begin
	match e with 
        | Cint i -> Push (Imm i)::stream 
        | Arg -> Push (edx)::stream
	| Binop (op,e1,e2) -> 		
		let newstream = Pop (ecx)::(emit_exp e2 (emit_exp e1 stream))  in
		let resstream = 
		begin
		match op with
		Plus -> Add (ecx,stack_offset 0l)::newstream
		| Minus -> Sub (ecx,stack_offset 0l)::newstream
		| Times -> List.append [Mov (ecx,stack_offset 0l);Imul (stack_offset 0l,Ecx)] newstream
		| Eq -> List.append [Setb (Eq,stack_offset 0l);Mov (Imm 0l,stack_offset 0l);Cmp (ecx,stack_offset 0l)] newstream
		| Neq -> List.append [Setb (NotEq,stack_offset 0l);Mov (Imm 0l,stack_offset 0l);Cmp (ecx,stack_offset 0l)] newstream
		| Lt -> List.append [Setb (Slt,stack_offset 0l);Mov (Imm 0l,stack_offset 0l);Cmp (ecx,stack_offset 0l)] newstream
		| Lte -> List.append [Setb (Sle,stack_offset 0l);Mov (Imm 0l,stack_offset 0l);Cmp (ecx,stack_offset 0l)] newstream
		| Gt -> List.append [Setb (Sgt,stack_offset 0l);Mov (Imm 0l,stack_offset 0l);Cmp (ecx,stack_offset 0l)] newstream
		| Gte -> List.append [Setb (Sge,stack_offset 0l);Mov (Imm 0l,stack_offset 0l);Cmp (ecx,stack_offset 0l)] newstream
		| And -> And (ecx,stack_offset 0l)::newstream
		| Or -> Or (ecx,stack_offset 0l)::newstream
		| Shl -> Shl(ecx,stack_offset 0l)::newstream
		| Shr -> Shr (ecx,stack_offset 0l)::newstream
		| Sar -> Sar (ecx,stack_offset 0l)::newstream
		end in
		resstream
	| Unop (op,e1) -> begin
		let newstream = emit_exp e1 stream in
		match op with
		Neg -> Neg (stack_offset 0l)::newstream
		| Lognot -> List.append [Setb (Eq,stack_offset 0l);Mov (Imm 0l,stack_offset 0l);Cmp (Imm 0l,stack_offset 0l)] newstream
		| Not -> Not (stack_offset 0l)::newstream
		end
 end

let compile_exp (ast:exp) : Cunit.cunit =
  let block_name = (Platform.decorate_cdecl "program") in
  let init_instr = [Mov ((stack_offset 8l), edx);Mov (esp,ebp);Push (ebp)] in
  let body = (emit_exp ast init_instr) in
	[Cunit.Code ({label = mk_lbl_named block_name;insns = List.rev (List.append [Ret;Pop (ebp);Pop (eax)] body);global = true})]
