(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(*                                                                           *)
(* Permission is hereby granted, free of charge, to any person obtaining a   *)
(* copy of this software and associated documentation files (the "Software"),*)
(* to deal in the Software without restriction, including without limitation *)
(* the rights to use, copy, modify, merge, publish, distribute, sublicense,  *)
(* and/or sell copies of the Software, and to permit persons to whom the     *)
(* Software is furnished to do so, subject to the following conditions:      *)
(*                                                                           *)
(* The above copyright notice and this permission notice shall be included   *)
(* in all copies or substantial portions of the Software.                    *)
(*                                                                           *)
(* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR*)
(* IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,  *)
(* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL   *)
(* THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER*)
(* LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING   *)
(* FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER       *)
(* DEALINGS IN THE SOFTWARE.                                                 *)
(*                                                                           *)
(*****************************************************************************)

let dump_file oc file =
  let ic = open_in file in
  let buflen = 8096 in
  let buf = Bytes.create buflen in
  let rec loop () =
    let len = input ic buf 0 buflen in
    if len <> 0 then (
      Printf.fprintf
        oc
        "%s"
        (if len = buflen then Bytes.unsafe_to_string buf
        else Bytes.sub_string buf 0 len) ;
      loop ())
  in
  loop () ;
  close_in ic

(** Check that each individual file is valid OCaml and does not use
    ["external"]. *)
let check_syntax kind file =
  let ic = open_in file in
  let lexbuf = Lexing.from_channel ic in
  lexbuf.lex_start_p <- {lexbuf.lex_start_p with pos_fname = file} ;
  lexbuf.lex_curr_p <- {lexbuf.lex_curr_p with pos_fname = file} ;
  let iterator =
    (* Open recursion with records, see
       https://caml.inria.fr/pub/docs/manual-ocaml/compilerlibref/Ast_mapper.html *)
    Ast_iterator.
      {
        default_iterator with
        structure_item =
          (fun iterator struct_item ->
            match struct_item.pstr_desc with
            | Pstr_primitive _ ->
                Format.kasprintf
                  Stdlib.failwith
                  "protocol-compiler: %a: use of `external` is forbidden"
                  Location.print_loc
                  struct_item.pstr_loc
            | _should_be_fine ->
                default_iterator.structure_item iterator struct_item);
      }
  in
  (match kind with
  | `Implementation ->
      let impl = Parse.implementation lexbuf in
      iterator.structure iterator impl
  | `Interface ->
      let intf = Parse.interface lexbuf in
      iterator.signature iterator intf) ;
  close_in ic

let include_ml oc file =
  let unit =
    String.capitalize_ascii (Filename.chop_extension (Filename.basename file))
  in
  let () =
    String.iter
      (function
        | 'A' .. 'Z' | 'a' .. 'z' | '0' .. '9' | '_' -> ()
        | other ->
            Format.kasprintf
              Stdlib.failwith
              "protocol-compiler: module %S uses an illegal character: %c."
              unit
              other)
      unit
  in
  Printf.fprintf oc "open struct module %s (_ : sig end)" unit ;
  let mli = file ^ "i" in
  if Sys.file_exists mli then (
    check_syntax `Interface mli ;
    Printf.fprintf oc ": sig\n" ;
    Printf.fprintf oc "# 1 %S\n" mli ;
    dump_file oc mli ;
    Printf.fprintf oc "end ") ;
  Printf.fprintf oc "= struct\n" ;
  Printf.fprintf oc "# 1 %S\n" file ;
  check_syntax `Implementation file ;
  dump_file oc file ;
  Printf.fprintf oc "end end\n" ;
  Printf.fprintf oc "module %s = %s ()\n" unit unit

let opened_modules = ["Tezos_protocol_environment"; "Pervasives"; "Error_monad"]

let dump oc version hash files =
  Printf.fprintf
    oc
    "module Make (Tezos_protocol_environment : \
     Tezos_protocol_environment_sigs__%s.T) = struct\n"
    version ;
  Printf.fprintf oc "[@@@ocaml.warning \"-33\"]\n" ;
  List.iter (Printf.fprintf oc "open %s\n") opened_modules ;
  Printf.fprintf oc "[@@@ocaml.warning \"+33\"]\n" ;
  Printf.fprintf
    oc
    "let hash = Protocol_hash.of_b58check_exn %S;;\n"
    (Protocol_hash.to_b58check hash) ;
  for i = 0 to Array.length files - 1 do
    include_ml oc files.(i)
  done ;
  Printf.fprintf
    oc
    "  include %s\n"
    (String.capitalize_ascii
       (Filename.basename
          (Filename.chop_extension files.(Array.length files - 1)))) ;
  Printf.fprintf oc "end\n%!"
