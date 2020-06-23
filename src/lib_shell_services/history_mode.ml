(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2019-2021 Nomadic Labs. <contact@nomadic-labs.com>          *)
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

type additional_cycles = {offset : int}

type t = Archive | Full of additional_cycles | Rolling of additional_cycles

let default_offset = 5

let default_full = Full {offset = default_offset}

let default_rolling = Rolling {offset = default_offset}

let default = default_full

module Legacy = struct
  type t = Archive | Full | Rolling

  let encoding =
    Data_encoding.string_enum
      [("archive", Archive); ("full", Full); ("rolling", Rolling)]

  let pp ppf = function
    | Archive ->
        Format.fprintf ppf "archive"
    | Full ->
        Format.fprintf ppf "full"
    | Rolling ->
        Format.fprintf ppf "rolling"
end

let convert = function
  | Legacy.Rolling ->
      Rolling {offset = default_offset}
  | Legacy.Full ->
      default
  | Legacy.Archive ->
      Archive

let encoding =
  let open Data_encoding in
  let additional_cycles_encoding =
    obj1
      (req
         ~title:"additional cycles"
         ~description:
           (Format.sprintf
              "Number of additional cycles preserved below the savepoint. By \
               default: %d additional cycles will be stored."
              default_offset)
         "additional_cycles"
         (ranged_int 0 1_000))
  in
  def
    "history_mode"
    ~title:"history mode"
    ~description:"Storage mode for the Tezos shell."
    (union
       ~tag_size:`Uint8
       [ case
           ~title:"archive"
           ~description:
             "Archive mode retains every block and operations since the \
              genesis block including their metadata and their associated \
              contexts."
           (Tag 0)
           (constant "archive")
           (function Archive -> Some () | _ -> None)
           (fun () -> Archive);
         case
           ~title:"full"
           ~description:
             "Full mode retains every block and operations since the genesis \
              block but periodically prunes older blocks' metadata to reduce \
              the storage size."
           (Tag 1)
           (obj1 (req "full" additional_cycles_encoding))
           (function Full {offset} -> Some offset | _ -> None)
           (fun offset -> Full {offset});
         case
           ~title:"rolling"
           ~description:
             "Rolling mode only retain the most recent cycles by periodically \
              periodically discarding older blocks to reduce the storage size."
           (Tag 2)
           (obj1 (req "rolling" additional_cycles_encoding))
           (function Rolling {offset} -> Some offset | _ -> None)
           (fun offset -> Rolling {offset}) ])

let equal hm1 hm2 =
  match (hm1, hm2) with
  | (Archive, Archive) ->
      true
  | (Full {offset}, Full {offset = offset'})
  | (Rolling {offset}, Rolling {offset = offset'}) ->
      Compare.Int.(offset = offset')
  | _ ->
      false

let pp ppf = function
  | Archive ->
      Format.fprintf ppf "Archive mode"
  | Full {offset} ->
      Format.fprintf
        ppf
        "Full mode%s"
        (if offset = 0 then "" else Format.sprintf " + %d extra cycles" offset)
  | Rolling {offset} ->
      Format.fprintf
        ppf
        "Rolling mode%s"
        (if offset = 0 then "" else Format.sprintf " + %d extra cycles" offset)

let pp_short ppf = function
  | Archive ->
      Format.fprintf ppf "archive"
  | Full _ ->
      Format.fprintf ppf "full"
  | Rolling _ ->
      Format.fprintf ppf "rolling"

let tag = Tag.def "history_mode" pp
