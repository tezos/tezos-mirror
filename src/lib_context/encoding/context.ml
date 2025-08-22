(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018-2021 Tarides <contact@tarides.com>                     *)
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

module Path = Irmin.Path.String_list
module Metadata = Irmin.Metadata.None
module Branch = Irmin.Branch.String

module Conf = struct
  let entries = 32

  let stable_hash = 256

  let contents_length_header = Some `Varint

  let inode_child_order = `Seeded_hash

  let forbid_empty_dir_persistence = true
end

module Hash : sig
  include Irmin.Hash.S

  val to_raw_string : t -> string

  val to_context_hash : t -> Context_hash.t

  val of_context_hash : Context_hash.t -> t
end = struct
  module H = Digestif.Make_BLAKE2B (struct
    let digest_size = 32
  end)

  type t = H.t

  let to_raw_string = H.to_raw_string

  let unsafe_of_raw_string s = H.of_raw_string s

  let of_context_hash s = H.of_raw_string (Context_hash.to_string s)

  let to_context_hash h = Context_hash.of_string_exn (H.to_raw_string h)

  let pp ppf t = Context_hash.pp ppf (to_context_hash t)

  let of_string x =
    match Context_hash.of_b58check x with
    | Ok x -> Ok (of_context_hash x)
    | Error err ->
        Error
          (`Msg
             (Format.asprintf
                "Failed to read b58check_encoding data: %a"
                Error_monad.pp_print_trace
                err))

  let short_hash_string = Irmin.Type.(unstage (short_hash string))

  let short_hash ?seed t = short_hash_string ?seed (H.to_raw_string t)

  let hash_size = H.digest_size

  let short_hash_substring t ~off =
    let str = Bigstringaf.substring t ~off ~len:hash_size in
    short_hash_string str

  let t : t Irmin.Type.t =
    Irmin.Type.map
      ~pp
      ~of_string
      Irmin.Type.(string_of (`Fixed H.digest_size))
      ~short_hash
      H.of_raw_string
      H.to_raw_string

  let short_hash =
    let f = short_hash_string ?seed:None in
    fun t -> f (H.to_raw_string t)

  let hash_size = H.digest_size

  let hash = H.digesti_string
end

module Info = Irmin.Info.Default

module Node
    (Contents_key : Irmin.Key.S with type hash = Hash.t)
    (Node_key : Irmin.Key.S with type hash = Hash.t) =
struct
  module M =
    Irmin.Node.Generic_key.Make (Hash) (Path) (Metadata) (Contents_key)
      (Node_key)

  (* [V1] is only used to compute preimage hashes. [assert false]
     statements should be unreachable.*)
  module V1 : sig
    val pre_hash : M.t -> (string -> unit) -> unit
  end = struct
    module Hash = Irmin.Hash.V1 (Hash)

    type entry = string * M.value

    let step_t = Irmin.Type.string

    let metadata_t =
      let some = "\255\000\000\000\000\000\000\000" in
      let none = "\000\000\000\000\000\000\000\000" in
      Irmin.Type.(map (string_of (`Fixed 8)))
        (fun _ -> assert false)
        (function Some _ -> some | None -> none)

    let metadata_of_entry (_, t) =
      match t with `Node _ -> None | `Contents (_, m) -> Some m

    let hash_of_entry (_, t) =
      match t with
      | `Node h -> Node_key.to_hash h
      | `Contents (h, _) -> Contents_key.to_hash h

    (* Irmin 1.4 uses int64 to store list lengths *)
    let entry_t : entry Irmin.Type.t =
      let open Irmin.Type in
      record "Tree.entry" (fun _ _ _ -> assert false)
      |+ field "kind" metadata_t metadata_of_entry
      |+ field "name" step_t fst
      |+ field "hash" Hash.t hash_of_entry
      |> sealr

    let entries_t : entry list Irmin.Type.t =
      Irmin.Type.(list ~len:`Int64 entry_t)

    let pre_hash_entries = Irmin.Type.(unstage (pre_hash entries_t))

    let compare_entry (x, _) (y, _) = String.compare x y

    let step_to_string = Irmin.Type.(unstage (to_bin_string Path.step_t))

    let str_key (k, v) = (step_to_string k, v)

    let pre_hash t =
      M.list t |> List.map str_key
      |> List.fast_sort compare_entry
      |> pre_hash_entries
  end

  include M

  let t = Irmin.Type.(like t ~pre_hash:V1.pre_hash)
end

module Commit
    (Node_key : Irmin.Key.S with type hash = Hash.t)
    (Commit_key : Irmin.Key.S with type hash = Hash.t) =
struct
  module M = Irmin.Commit.Generic_key.Make (Hash) (Node_key) (Commit_key)
  module V1 = Irmin.Commit.V1.Make (Hash) (M)
  include M

  let pre_hash_v1_t = Irmin.Type.(unstage (pre_hash V1.t))

  let pre_hash_v1 t = pre_hash_v1_t (V1.import t)

  let t = Irmin.Type.(like t ~pre_hash:pre_hash_v1)
end

module Contents = struct
  type t = bytes

  let ty = Irmin.Type.(pair (bytes_of `Int64) unit)

  let pre_hash_ty = Irmin.Type.(unstage (pre_hash ty))

  let pre_hash_v1 x = pre_hash_ty (x, ())

  let t = Irmin.Type.(like bytes ~pre_hash:pre_hash_v1)

  let merge = Irmin.Merge.(idempotent (Irmin.Type.option t))
end

module Schema = struct
  module Hash = Hash
  module Branch = Branch
  module Info = Info
  module Metadata = Metadata
  module Path = Path
  module Contents = Contents
  module Node = Node
  module Commit = Commit
end

module type Conf = Irmin_pack.Conf.S
