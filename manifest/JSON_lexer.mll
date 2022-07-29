{
  open JSON_parser
}

(* NOTE: This rule does not follow the JSON spec. In particular it does
   not correctly handle backslash characters in strings. If it becomes
   necessary to support it, we could consider vendoring a json library. *)
rule token = parse
  | [' ' '\t' '\r' '\n']+ { token lexbuf }
  | '-'? ['0'-'9']+ ('.' ['0'-'9']+)? as x
      { (* TODO: this regexp probably doesn't exactly match the spec *)
        FLOAT (float_of_string x) }
  | '"' ([^'"' '\\']* as x) '"'
      { STRING x }
  | '{' { LBRACE }
  | '}' { RBRACE }
  | '[' { LBRACKET }
  | ']' { RBRACKET }
  | ',' { COMMA }
  | ':' { COLON }
  | ['a'-'z']+ as x
      { match x with
          | "true" -> BOOL true
          | "false" -> BOOL false
          | "null" -> NULL
          | _ -> failwith "parse error" }
  | eof { EOF }
