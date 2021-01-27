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

module Hash : sig
  include Irmin.Hash.S

  val to_context_hash : t -> Context_hash.t

  val of_context_hash : Context_hash.t -> t
end = struct
  module H = Digestif.Make_BLAKE2B (struct
    let digest_size = 32
  end)

  type t = H.t

  let of_context_hash s = H.of_raw_string (Context_hash.to_string s)

  let to_context_hash h = Context_hash.of_string_exn (H.to_raw_string h)

  let pp ppf t = Context_hash.pp ppf (to_context_hash t)

  let of_string x =
    match Context_hash.of_b58check x with
    | Ok x ->
        Ok (of_context_hash x)
    | Error err ->
        Error
          (`Msg
            (Format.asprintf
               "Failed to read b58check_encoding data: %a"
               Error_monad.pp_print_error
               err))

  let short_hash_string = Irmin.Type.(unstage (short_hash string))

  let short_hash_staged =
    Irmin.Type.stage
    @@ fun ?seed t -> short_hash_string ?seed (H.to_raw_string t)

  let t : t Irmin.Type.t =
    Irmin.Type.map
      ~pp
      ~of_string
      Irmin.Type.(string_of (`Fixed H.digest_size))
      ~short_hash:short_hash_staged
      H.of_raw_string
      H.to_raw_string

  let short_hash =
    let f = short_hash_string ?seed:None in
    fun t -> f (H.to_raw_string t)

  let hash_size = H.digest_size

  let hash = H.digesti_string
end

module Node = struct
  module M = Irmin.Private.Node.Make (Hash) (Path) (Metadata)

  module V1 = struct
    module Hash = Irmin.Hash.V1 (Hash)

    type kind = [`Node | `Contents of Metadata.t]

    type entry = {kind : kind; name : M.step; node : Hash.t}

    (* Irmin 1.4 uses int8 to store filename lengths.

       Irmin 2 use a variable-size encoding for strings; this is using int8
       for strings of size stricly less than 128 (e.g. 2^7) which happen to
       be the case for all filenames ever produced by Irmin 1.4. *)
    let step_t = Irmin.Type.string

    let metadata_t =
      let some = "\255\000\000\000\000\000\000\000" in
      let none = "\000\000\000\000\000\000\000\000" in
      Irmin.Type.(map (string_of (`Fixed 8)))
        (fun s ->
          match s.[0] with
          | '\255' ->
              None
          | '\000' ->
              Some ()
          | _ ->
              assert false)
        (function Some _ -> some | None -> none)

    (* Irmin 1.4 uses int64 to store list lengths *)
    let entry_t : entry Irmin.Type.t =
      let open Irmin.Type in
      record "Tree.entry" (fun kind name node ->
          let kind = match kind with None -> `Node | Some m -> `Contents m in
          {kind; name; node})
      |+ field "kind" metadata_t (function
             | {kind = `Node; _} ->
                 None
             | {kind = `Contents m; _} ->
                 Some m)
      |+ field "name" step_t (fun {name; _} -> name)
      |+ field "node" Hash.t (fun {node; _} -> node)
      |> sealr

    let entries_t : entry list Irmin.Type.t =
      Irmin.Type.(list ~len:`Int64 entry_t)

    let import_entry (s, v) =
      match v with
      | `Node h ->
          {name = s; kind = `Node; node = h}
      | `Contents (h, m) ->
          {name = s; kind = `Contents m; node = h}

    let import t = List.map import_entry (M.list t)

    let pre_hash_entries = Irmin.Type.(unstage (pre_hash entries_t))

    let compare_entry x y = String.compare x.name y.name

    let pre_hash entries =
      pre_hash_entries (List.fast_sort compare_entry entries)
  end

  include M

  let pre_hash_v1 x = V1.pre_hash (V1.import x)

  let t = Irmin.Type.(like t ~pre_hash:(stage @@ fun x -> pre_hash_v1 x))
end

module Commit = struct
  module M = Irmin.Private.Commit.Make (Hash)
  module V1 = Irmin.Private.Commit.V1 (M)
  include M

  let pre_hash_v1_t = Irmin.Type.(unstage (pre_hash V1.t))

  let pre_hash_v1 t = pre_hash_v1_t (V1.import t)

  let t = Irmin.Type.(like t ~pre_hash:(stage @@ fun x -> pre_hash_v1 x))
end

module Contents = struct
  type t = bytes

  let ty = Irmin.Type.(pair (bytes_of `Int64) unit)

  let pre_hash_ty = Irmin.Type.(unstage (pre_hash ty))

  let pre_hash_v1 x = pre_hash_ty (x, ())

  let t = Irmin.Type.(like bytes ~pre_hash:(stage @@ fun x -> pre_hash_v1 x))

  let merge = Irmin.Merge.(idempotent (Irmin.Type.option t))
end
