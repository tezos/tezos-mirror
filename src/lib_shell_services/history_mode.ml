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

let pp_additional_cycles ppf {offset} =
  Format.pp_print_string
    ppf
    (if offset = 0 then "" else Format.sprintf " + %d extra cycles" offset)

(* For Full and Rolling modes, setting `additional_cycles` to `None`
   specifies that the history mode will depend on the default
   offset. Thus, if this `default_offset` value is modified at some
   point, the data will be automatically updated to meet the new
   requirements in terms of data retention. On the contrary, the usage
   of `Some N` additional cycles fixes it to `N`.*)
type t =
  | Archive
  | Full of additional_cycles option
  | Rolling of additional_cycles option

(* The default_offset value defines a window of stored cycles which is
   suitable for baking services. It currently corresponds to 6 as we
   store 1 cycle below the last preserved block level of the current
   head, which is set to [preserved_cycles] cycles in the past.
   TODO: https://gitlab.com/tezos/tezos/-/issues/1406
   As this value is potentially both network and protocol specific, it
   could be lifted as a protocol value or an hardcoded node
   configuration argument. *)
let default_offset = 1

let default_additional_cycles = {offset = default_offset}

let default_full = Full None

let default_rolling = Rolling None

let default = default_full

module Legacy = struct
  type t = Archive | Full | Rolling

  let encoding =
    Data_encoding.string_enum
      [("archive", Archive); ("full", Full); ("rolling", Rolling)]

  let pp ppf = function
    | Archive -> Format.fprintf ppf "archive"
    | Full -> Format.fprintf ppf "full"
    | Rolling -> Format.fprintf ppf "rolling"
end

let convert = function
  | Legacy.Rolling -> default_rolling
  | Legacy.Full -> default_full
  | Legacy.Archive -> Archive

let additional_cycles_encoding =
  let open Data_encoding in
  obj1
    (req
       ~title:"additional cycles"
       ~description:
         (Format.sprintf
            "Number of additional cycles preserved below the savepoint. By \
             default: %d additional cycles will be stored."
            default_additional_cycles.offset)
       "additional_cycles"
       (ranged_int 0 1_000))

let encoding =
  let open Data_encoding in
  def
    "history_mode"
    ~title:"history mode"
    ~description:"Storage mode for the Tezos shell."
    (union
       ~tag_size:`Uint8
       [
         case
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
           (function
             | Full (Some {offset}) -> Some offset
             | Full None -> None
             | _ -> None)
           (function offset -> Full (Some {offset}));
         case
           ~title:"rolling"
           ~description:
             "Rolling mode only retain the most recent cycles by periodically \
              periodically discarding older blocks to reduce the storage size."
           (Tag 2)
           (obj1 (req "rolling" additional_cycles_encoding))
           (function
             | Rolling (Some {offset}) -> Some offset
             | Rolling None -> None
             | _ -> None)
           (function offset -> Rolling (Some {offset}));
         (* Cases with default numbers of additional cycles. Using
            them in the configuration file ensures that new default
            values are used after updating instead of being pinned to
            an explicit value. *)
         case
           ~title:"full_alias"
           ~description:"Full mode with default number of additional cycles."
           (Tag 3)
           (constant "full")
           (function Full None -> Some () | _ -> None)
           (fun () -> default_full);
         case
           ~title:"rolling_alias"
           ~description:"Rolling mode with default number of additional cycles."
           (Tag 4)
           (constant "rolling")
           (function Rolling None -> Some () | _ -> None)
           (fun () -> default_rolling);
       ])

let equal hm1 hm2 =
  match (hm1, hm2) with
  | Archive, Archive | Full None, Full None | Rolling None, Rolling None -> true
  | Full (Some {offset}), Full (Some {offset = offset'})
  | Rolling (Some {offset}), Rolling (Some {offset = offset'}) ->
      Compare.Int.(offset = offset')
  | (full, Full (Some {offset}) | Full (Some {offset}), full)
    when offset = default_offset && full = default_full ->
      true
  | (rolling, Rolling (Some {offset}) | Rolling (Some {offset}), rolling)
    when offset = default_offset && rolling = default_rolling ->
      true
  | _ -> false

let mode_equality hm1 hm2 =
  match (hm1, hm2) with
  | Archive, Archive | Full _, Full _ | Rolling _, Rolling _ -> true
  | Archive, _ | Full _, _ | Rolling _, _ -> false

let pp ppf = function
  | Archive -> Format.fprintf ppf "Archive mode"
  | Full None ->
      Format.fprintf
        ppf
        "Full mode%a"
        pp_additional_cycles
        default_additional_cycles
  | Rolling None ->
      Format.fprintf
        ppf
        "Rolling mode%a"
        pp_additional_cycles
        default_additional_cycles
  | Full (Some additional_cycles) ->
      Format.fprintf ppf "Full mode%a" pp_additional_cycles additional_cycles
  | Rolling (Some additional_cycles) ->
      Format.fprintf ppf "Rolling mode%a" pp_additional_cycles additional_cycles

let pp_short ppf = function
  | Archive -> Format.fprintf ppf "archive"
  | Full _ -> Format.fprintf ppf "full"
  | Rolling _ -> Format.fprintf ppf "rolling"

let tag = Tag.def "history_mode" pp
