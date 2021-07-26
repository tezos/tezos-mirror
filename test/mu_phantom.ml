(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

open Data_encoding

let desc e =
  Data_encoding.(Json.(construct Binary_schema.encoding (Binary.describe e)))

type boollist = Nil | Cons of bool * boollist

let boollist_encoding =
  let open Data_encoding in
  mu "boollist" @@ fun enc ->
  union
    [
      case
        (Tag 0)
        ~title:"Nil"
        empty
        (function Nil -> Some () | _ -> None)
        (fun () -> Nil);
      case
        (Tag 1)
        ~title:"Cons"
        (obj2 (req "head" Encoding.bool) (req "tail" enc))
        (function Cons (head, tail) -> Some (head, tail) | _ -> None)
        (fun (head, tail) -> Cons (head, tail));
    ]

type 'a canonical = Canonical of 'a

let canonical_encoding =
  let open Data_encoding in
  conv (fun (Canonical x) -> x) (fun x -> Canonical x) boollist_encoding

let described_encoding =
  Data_encoding.(def "canonicalboollistobj" canonical_encoding)

let check_refs enc () =
  let d = desc enc in
  match d with
  | `O [("toplevel", toplevel); ("fields", fields)] ->
      let tl_refs =
        match toplevel with
        | `O [("fields", `A tl_fields)] ->
            let get_ref : json -> string option = function
              | `O field_specs -> (
                  match List.assoc_opt "layout" field_specs with
                  | Some (`O layout) -> (
                      match List.assoc "kind" layout with
                      | `String "Ref" -> (
                          match List.assoc "name" layout with
                          | `String rname -> Some rname
                          | _ -> None)
                      | _ -> None)
                  | _ -> assert false)
              | _ -> assert false
            in
            List.filter_map get_ref tl_fields
        | _ -> assert false
      and fld_defs =
        match fields with
        | `A defs ->
            let get_defname : json -> string = function
              | `O def_specs -> (
                  match List.assoc "description" def_specs with
                  | `O description -> (
                      match List.assoc "title" description with
                      | `String id -> id
                      | _ -> assert false)
                  | _ -> assert false)
              | _ -> assert false
            in
            List.map get_defname defs
        | _ -> assert false
      in
      assert (List.for_all (fun r -> List.mem r fld_defs) tl_refs) ;
      ()
  | _ -> assert false

let tests =
  [
    ("base encoding", `Quick, check_refs boollist_encoding);
    ("canonical encoding", `Quick, check_refs canonical_encoding);
    ("described encoding", `Quick, check_refs described_encoding);
  ]
