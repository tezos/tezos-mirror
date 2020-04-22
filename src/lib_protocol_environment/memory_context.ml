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

module M = struct
  module StringMap = Map.Make (String)

  type key = string list

  type value = Bytes.t

  type t = Dir of t StringMap.t | Key of value

  let empty = Dir StringMap.empty

  let rec raw_get m k =
    match (k, m) with
    | ([], m) ->
        Some m
    | (n :: k, Dir m) -> (
      match StringMap.find_opt n m with
      | Some res ->
          raw_get res k
      | None ->
          None )
    | (_ :: _, Key _) ->
        None

  let rec raw_set m k v =
    match (k, m, v) with
    | ([], (Key _ as m), Some v) ->
        if m = v then None else Some v
    | ([], (Dir _ as m), Some v) ->
        if m == v then None else Some v
    | ([], (Key _ | Dir _), None) ->
        Some empty
    | (n :: k, Dir m, _) -> (
      match
        raw_set (Option.value ~default:empty (StringMap.find_opt n m)) k v
      with
      | None ->
          None
      | Some rm when rm = empty ->
          Some (Dir (StringMap.remove n m))
      | Some rm ->
          Some (Dir (StringMap.add n rm m)) )
    | (_ :: _, Key _, None) ->
        None
    | (_ :: _, Key _, Some _) ->
        Stdlib.failwith "Mem_context.set"

  let mem m k =
    match raw_get m k with
    | Some (Key _) ->
        Lwt.return_true
    | Some (Dir _) | None ->
        Lwt.return_false

  let dir_mem m k =
    match raw_get m k with
    | Some (Dir _) ->
        Lwt.return_true
    | Some (Key _) | None ->
        Lwt.return_false

  let get m k =
    match raw_get m k with
    | Some (Key v) ->
        Lwt.return_some v
    | Some (Dir _) | None ->
        Lwt.return_none

  let set m k v =
    match raw_set m k (Some (Key v)) with
    | None ->
        Lwt.return m
    | Some m ->
        Lwt.return m

  let remove_rec m k =
    match raw_set m k None with None -> Lwt.return m | Some m -> Lwt.return m

  let copy m ~from ~to_ =
    match raw_get m from with
    | None ->
        Lwt.return_none
    | Some v -> (
        let pp_path =
          Format.(
            pp_print_list
              ~pp_sep:(fun ppf () -> pp_print_string ppf " / ")
              pp_print_string)
        in
        match raw_set m to_ (Some v) with
        | Some _ as v ->
            Lwt.return v
        | None ->
            Format.kasprintf
              Lwt.fail_with
              "Mem_context.copy %a %a: The value is already set."
              pp_path
              from
              pp_path
              to_
        | exception Failure s ->
            Format.kasprintf
              Lwt.fail_with
              "Mem_context.copy %a %a: Failed with %s"
              pp_path
              from
              pp_path
              to_
              s )

  type key_or_dir = [`Key of key | `Dir of key]

  let fold m k ~init ~f =
    match raw_get m k with
    | None ->
        Lwt.return init
    | Some (Key _) ->
        Lwt.return init
    | Some (Dir m) ->
        StringMap.fold
          (fun n m acc ->
            acc
            >>= fun acc ->
            match m with
            | Key _ ->
                f (`Key (k @ [n])) acc
            | Dir _ ->
                f (`Dir (k @ [n])) acc)
          m
          (Lwt.return init)

  let current_protocol_key = ["protocol"]

  let set_protocol v key =
    raw_set v current_protocol_key (Some (Key (Protocol_hash.to_bytes key)))
    |> function Some m -> Lwt.return m | None -> assert false

  let fork_test_chain c ~protocol:_ ~expiration:_ = Lwt.return c

  let encoding : t Data_encoding.t =
    let open Data_encoding in
    mu "memory_context" (fun encoding ->
        let map_encoding =
          conv
            (fun map -> List.of_seq (StringMap.to_seq map))
            (fun bindings -> StringMap.of_seq (List.to_seq bindings))
            (list (tup2 string encoding))
        in
        union
          [ case
              ~title:"directory"
              (Tag 0)
              map_encoding
              (function Dir map -> Some map | Key _ -> None)
              (fun map -> Dir map);
            case
              ~title:"value"
              (Tag 1)
              bytes
              (function Key v -> Some v | Dir _ -> None)
              (fun v -> Key v) ])
end

open Tezos_protocol_environment

type t = M.t

type _ Context.kind += Memory : t Context.kind

let ops = (module M : CONTEXT with type t = 'ctxt)

let empty =
  let ctxt = M.empty in
  Context.Context {ops; ctxt; kind = Memory}

let project : Context.t -> t =
 fun (Context.Context {ctxt; kind; _} : Context.t) ->
  match kind with Memory -> ctxt | _ -> assert false

let inject : t -> Context.t =
 fun ctxt -> Context.Context {ops; ctxt; kind = Memory}

let encoding : Context.t Data_encoding.t =
  let open Data_encoding in
  conv project inject M.encoding
