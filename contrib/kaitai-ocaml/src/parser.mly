%{
(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2023 Marigold, <contact@marigold.dev>                       *)
(* Copyright (c) 2023 Nomadic Labs, <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

(* Original implementation at
https://github.com/kaitai-io/kaitai_struct_compiler/blob/master/shared/src/main/scala/io/kaitai/struct/exprlang/Expressions.scala *)

open Types

%}

%token AND
%token OR
%token NOT
%token AS
%token SIZEOF
%token BITSIZEOF
%token <string> INT
%token <string> FLOAT
%token <string> STRING
%token <string> IDENT
%token COLON2
%token LBRACKET
%token RBRACKET
%token LPAREN
%token RPAREN
%token QUESTION
%token COLON
%token LSHIFT
%token RSHIFT
%token LT
%token GT
%token EQ
%token GTE
%token LTE
%token NOTEQ
%token ADD
%token SUB
%token MUL
%token DIV
%token MOD
%token BITOR
%token BITAND
%token BITXOR
%token COMMA
%token TILDE
%token DOT
%token EOF
%start <Ast.t> expression

%%

test:
  | or_test { $1 }
  | condition=or_test QUESTION ifTrue=test COLON ifFalse=test
    { Ast.(IfExp {condition; ifTrue; ifFalse}) }

or_test:
  | and_test { $1 }
  | or_test OR and_test { Ast.(BoolOp { op = Or; values = [$1; $3] }) }

and_test:
  | not_test { $1 }
  | and_test AND not_test { Ast.(BoolOp { op = And; values = [$1; $3] }) }

not_test:
  | NOT not_test { Ast.(UnaryOp { op = Ast.Not; operand = $2 }) }
  | comparison { $1 }

comparison:
  | expr { $1 }
  | left=expr ops=comp_op right=expr
    { Ast.Compare {left;ops;right} }

comp_op:
  | LT { Ast.Lt }
  | LTE { Ast.LtE }
  | GT { Ast.Gt }
  | GTE { Ast.GtE }
  | EQ { Ast.Eq }
  | NOTEQ { Ast.NotEq }

chain(P, OP):
  | P { $1 }
  | P OP chain(P,OP) { Ast.BinOp { left = $1; op = $2; right = $3} }

bitor:
  | BITOR { Ast.BitOr }
expr:
  | chain(xor_expr, bitor) { $1 }

bitxor:
  | BITXOR { Ast.BitXor }
xor_expr:
  | chain(and_expr, bitxor) { $1 }

bitand:
  | BITAND { Ast.BitAnd }
and_expr:
  | chain(shift_expr, bitand) { $1 }

shift_op:
 | LSHIFT { Ast.LShift }
 | RSHIFT { Ast.RShift }
shift_expr:
  | chain(arith_expr, shift_op) { $1 }

arith_op:
 | ADD { Ast.Add }
 | SUB { Ast.Sub }
arith_expr:
  | chain(term, arith_op) { $1 }

term_op:
  | DIV { Ast.Div }
  | MUL { Ast.Mult }
  | MOD { Ast.Mod }
term:
  | chain(factor, term_op) { $1 }

factor:
  | ADD factor { $2 }
  | SUB factor { Ast.(UnaryOp {op = Minus; operand = $2}) }
  | TILDE factor { Ast.(UnaryOp {op = Invert; operand = $2}) }
  | power { $1 }

power:
  | atom list(trailer) { List.fold_left (fun acc f -> f acc) $1 $2 }

trailer:
  | LPAREN args=separated_list(COMMA, test) RPAREN { fun func -> Ast.Call { func; args } }
  | LBRACKET test RBRACKET { fun x -> Ast.Subscript { value = x;  idx = $2 }}
  | DOT AS LT typeName=typeId GT { fun x -> Ast.CastToType { value = x; typeName } }
  | DOT IDENT { fun x -> Ast.Attribute { value = x; attr = $2 } }

atom:
  | LBRACKET RBRACKET { Ast.List [] }
  | s=STRING { Ast.Str s }
  | f=FLOAT { Ast.FloatNum (float_of_string f) }
  | i=INT { Ast.IntNum (int_of_string i) }
  /* | i=IDENT { */
  /*     match i with */
  /*     | "true"  -> Ast.Bool true */
  /*     | "false" -> Ast.Bool false */
  /*     | _       -> Ast.Name i */
  /*       } */
  | SIZEOF LT typeName=typeId GT { Ast.ByteSizeOfType{typeName} }
  | BITSIZEOF LT typeName=typeId GT { Ast.BitSizeOfType{typeName} }
  | NameOrEnumByName { $1 }
  | LBRACKET separated_nonempty_list(COMMA, test) RBRACKET { Ast.List $2 }
  | LPAREN test RPAREN { $2 }

typeId:
  | option(COLON2) separated_nonempty_list(COLON2, IDENT) option(pair(LBRACKET, LBRACKET)) {
    let absolute = Option.is_some $1 in
    Ast.({ absolute; isArray = false; names = $2 })
  }

NameOrEnumByName:
  | option(COLON2) separated_nonempty_list(COLON2, IDENT)
    {
      match $1, $2 with
      | _, [] -> assert false
      | None, [ "true" ] -> Ast.Bool true
      | None, [ "false" ] -> Ast.Bool false
      | None, [ name ] -> Ast.Name name
      | None, [ enumName; label ] ->
         Ast.(EnumByLabel { label; enumName; inType = empty_typeId })
      | prefix , path ->
         let path, enumName, label =
           match List.rev path with
           | [] | [_] | [_;_] -> assert false
           | label :: enunName :: path_rev ->
              List.rev path_rev, enunName, label
         in
         let absolute = Option.is_some prefix in
         let inType = Ast.{ absolute; isArray = false; names = path } in
         Ast.(EnumByLabel { label; enumName; inType})
    }

expression:
  | test EOF { $1 }
