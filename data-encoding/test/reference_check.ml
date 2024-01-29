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

module Helper = struct
  (** Convert an encoding to the JSON representation of its generated
      description using [describe] *)
  let desc e = Json.construct Binary_schema.encoding (Binary.describe e)

  (** Ensure that all references found in the toplevel encoding are included in
      the list of encoding definitions and have not been pruned or renamed out
      of existence *)
  let check_refs enc () =
    match desc enc with
    | `O [("toplevel", `O [("fields", `A tl_fields)]); ("fields", `A defs)] ->
        let tl_refs =
          let get_ref : json -> string option = function
            | `O field_specs -> (
                match List.assoc "layout" field_specs with
                | `O layout -> (
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
        and fld_defs =
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
        in
        assert (List.for_all (fun r -> List.mem r fld_defs) tl_refs) ;
        ()
    | _ -> assert false
end

(** Checks that a previously identified bug, in which the description of 
    an encoding defined with the {!mu} combinator contains a dangling
    reference in the toplevel encoding definition, is no longer extant.
  *)
module Mu_phantom = struct
  let boollist_encoding =
    let open Data_encoding in
    mu "boollist" @@ fun enc ->
    union
      [
        case
          (Tag 0)
          ~title:"Nil"
          empty
          (function [] -> Some () | _ -> None)
          (fun () -> []);
        case
          (Tag 1)
          ~title:"Cons"
          (obj2 (req "head" Encoding.bool) (req "tail" enc))
          (function head :: tail -> Some (head, tail) | _ -> None)
          (fun (head, tail) -> head :: tail);
      ]

  type 'a canonical = Canonical of 'a

  let canonical_encoding =
    let open Data_encoding in
    conv (fun (Canonical x) -> x) (fun x -> Canonical x) boollist_encoding

  let described_encoding =
    Data_encoding.(def "canonicalboollistobj" canonical_encoding)

  let tests =
    [
      ("base encoding", `Quick, Helper.check_refs boollist_encoding);
      ("canonical encoding", `Quick, Helper.check_refs canonical_encoding);
      ("described encoding", `Quick, Helper.check_refs described_encoding);
    ]
end

(** Checks that a previously identified bug, in which the description of 
    an encoding using {!obj} with isomorphic field encodings defined inline
    without explicitly identical names (using {!def}) would cause dangling references
    to persist in the toplevel encoding definition, is no longer extant.
  *)
module Inline_phantom = struct
  let first_encoding = def "first" unit

  let second_encoding = def "second" unit

  let ff_encoding = tup2 first_encoding first_encoding

  let fs_encoding = tup2 first_encoding second_encoding

  let sf_encoding = tup2 second_encoding first_encoding

  let ss_encoding = tup2 second_encoding second_encoding

  let adhoc_encoding =
    def "adhoc"
    @@ obj4
         (req "one" ff_encoding)
         (req "two" fs_encoding)
         (req "three" sf_encoding)
         (req "four" ss_encoding)

  let described_encoding =
    def "described"
    @@ obj4
         (req "one" (def "ff" ff_encoding))
         (req "two" (def "fs" fs_encoding))
         (req "three" (def "sf" sf_encoding))
         (req "four" (def "ss" ss_encoding))

  let tests =
    [
      ("adhoc encoding", `Quick, Helper.check_refs adhoc_encoding);
      ("described encoding", `Quick, Helper.check_refs described_encoding);
    ]
end
