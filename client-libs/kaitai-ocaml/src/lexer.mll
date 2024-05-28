{

(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2023 Marigold, <contact@marigold.dev>                       *)
(* Copyright (c) 2023 Nomadic Labs, <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

(* Original implementation at
https://github.com/kaitai-io/kaitai_struct_compiler/blob/master/shared/src/main/scala/io/kaitai/struct/exprlang/Lexical.scala *)

open Parser
let kw =
 let l =
   [ "and", AND
   ; "or", OR
   ; "not", NOT
   ; "as", AS
   ; "sizeof" , SIZEOF
   ; "bitsizeof", BITSIZEOF
   ]
 in
 let t = Hashtbl.create 10 in
 List.iter (fun (name, k) -> Hashtbl.add t name k) l;
 t

}

let octal_digit = ['0'-'7']
let decimal_literal =
  ['0'-'9'] ['0'-'9' '_']*
let hex_digit =
  ['0'-'9' 'A'-'F' 'a'-'f']
let hex_literal =
  '0' ['x' 'X'] hex_digit (hex_digit | '_')*
let oct_literal =
  '0' ['o' 'O'] octal_digit (octal_digit | '_')*
let bin_literal =
  '0' ['b' 'B'] ['0'-'1'] ['0'-'1' '_']*
let int_literal =
  decimal_literal | hex_literal | oct_literal | bin_literal
let float_literal =
  ['-' '+']?
  ['0'-'9'] ['0'-'9' '_']*
  ('.' ['0'-'9' '_']* )?
  (['e' 'E'] ['+' '-']? ['0'-'9'] ['0'-'9' '_']* )?

let lowercase  = ['a'-'z']
let uppercase  = ['A'-'Z']
let digit      = ['0'-'9']
let letter     = ( lowercase | uppercase )
let nameStart = (letter | "_" )
let namePart  = ( letter | digit | "_" )
let identifier  = nameStart namePart *

rule token = parse
  | " "  { token lexbuf }
  | "::" { COLON2 }
  | "["  { LBRACKET }
  | "]"  { RBRACKET }
  | "("  { LPAREN }
  | ")"  { RPAREN }
  | "?"  { QUESTION }
  | ":"  { COLON }
  | "<<" { LSHIFT }
  | ">>" { RSHIFT }
  | "<"  { LT }
  | ">"  { GT }
  | "==" { EQ }
  | ">=" { GTE }
  | "<=" { LTE }
  | "!=" { NOTEQ }
  | "+"  { ADD }
  | "-"  { SUB }
  | "*"  { MUL }
  | "/"  { DIV }
  | "%"  { MOD }
  | "|"  { BITOR }
  | "&"  { BITAND }
  | "^"  { BITXOR }
  | ","  { COMMA }
  | "~"  { TILDE }
  | "."  { DOT }
  | "'" [^ '\'']* "'" { STRING (Lexing.lexeme lexbuf) }
  | "\"" {
   let buf = Buffer.create 20 in
   Buffer.add_string buf (Lexing.lexeme lexbuf);
   STRING (string buf lexbuf)
  }
  | int_literal as lit { INT lit }
  | '-' decimal_literal { INT (Lexing.lexeme lexbuf) }
  | float_literal as lit { FLOAT lit }
  | identifier as id {
    match Hashtbl.find kw id with
    | exception Not_found -> IDENT id
    | kw -> kw
  }
  | eof { EOF }

and string buf = parse
  | "\""  { Buffer.add_string buf (Lexing.lexeme lexbuf); Buffer.contents buf }
  | "\\u" hex_digit hex_digit hex_digit hex_digit {
    Buffer.add_string buf (Lexing.lexeme lexbuf);
    string buf lexbuf
  }
  | "\\" octal_digit  + {
    Buffer.add_string buf (Lexing.lexeme lexbuf);
    string buf lexbuf
  }