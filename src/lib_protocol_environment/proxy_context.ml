(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2020 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

module StringMap = TzString.Map

module M = struct
  type key = string list

  type value = Bytes.t

  type tree = Dir of tree StringMap.t | Key of value

  (* When the option is [None], this instance of [M] should behave
     like [Memory_context]. *)
  type t = {rpc : (key -> tree tzresult Lwt.t) option; tree : tree}

  let empty = Dir StringMap.empty

  (** Generic pretty printing functions *)
  let pp_key ppf key =
    Format.pp_print_list
      ~pp_sep:(fun ppf _ -> Format.fprintf ppf "; ")
      Format.pp_print_string
      ppf
      key

  module L = struct
    module S = Internal_event.Simple

    let section = ["proxy"; "context"]

    let proxy_context_missing =
      S.declare_1
        ~section
        ~name:"proxy_context_missing"
        ~msg:"delegating to proxy cache, because data misses for: {key}"
        ~pp1:pp_key
        ("key", Data_encoding.(Variable.list string))
  end

  (* Useful for debugging *)
  let rec _pp_tree ppf = function
    | Key _b ->
        Format.fprintf ppf "key:"
    | Dir t ->
        StringMap.iter
          (fun k t -> Format.fprintf ppf "@[%s: @[%a@]@]" k _pp_tree t)
          t

  let rec tree_size = function
    | Key _ ->
        1
    | Dir t ->
        StringMap.fold (fun _ t' i -> tree_size t' + i) t 0

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

  let raw_get m k =
    match raw_get m.tree k with
    | None -> (
        L.(S.emit proxy_context_missing) k
        >>= fun () ->
        match m.rpc with
        | None ->
            return None
        | Some rpc -> (
            rpc k
            >>= function
            | Error err ->
                failwith
                  "Proxy_context's call to RPC failed with: %a"
                  Error_monad.pp_print_error
                  err
            | Ok t ->
                return @@ Option.some t ) )
    | Some _ as v ->
        return @@ v

  let rec raw_set m k v =
    (* This function returns the update it did. This is used in the
       recursive cases to find out what to do. In case no update was
       done, this allows to maximise persistence (keeping most part
       of the existing structure).
       
       That is why, in the three base cases (the first three pipes below),
       we check whether the value being put equals the value that is there
       already (if any). If they are equal, no update needs to be done;
       and hence None is returned.
     *)
    match (k, m, v) with
    | ([], (Key _ as m), Some v) ->
        if m = v then None else Some v
    | ([], (Dir _ as m), Some v) ->
        if m == v then None else Some v
    | ([], (Key _ | Dir _), None) ->
        Some empty
    | (n :: k, Dir m, _) -> (
      (* recursive case: inspect recursive modification *)
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
        Stdlib.failwith "Proxy_context.set"

  let raw_set m k v =
    let u = raw_set m.tree k v in
    match u with None -> None | Some u -> Some {m with tree = u}

  let mem m k =
    raw_get m k
    >>= function
    | Ok (Some (Key _)) ->
        Lwt.return_true
    | Ok (Some (Dir _) | None) | Error _ ->
        Lwt.return_false

  let dir_mem m k =
    raw_get m k
    >>= function
    | Ok (Some (Dir _)) ->
        Lwt.return_true
    | Error _ | Ok (Some (Key _) | None) ->
        Lwt.return_false

  let get m k =
    raw_get m k
    >>= function
    | Error _ | Ok (Some (Dir _) | None) ->
        Lwt.return_none
    | Ok (Some (Key v)) ->
        Lwt.return_some v

  let set m k v =
    match raw_set m k (Some (Key v)) with
    | None ->
        Lwt.return m
    | Some m ->
        Lwt.return m

  let remove_rec m k =
    match raw_set m k None with None -> Lwt.return m | Some m -> Lwt.return m

  let copy m ~from ~to_ =
    raw_get m from
    >>= function
    | Error _ | Ok None ->
        Lwt.return_none
    | Ok (Some v) -> (
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
              "Proxy_context.copy %a %a: The value is already set."
              pp_path
              from
              pp_path
              to_
        | exception Failure s ->
            Format.kasprintf
              Lwt.fail_with
              "Proxy_context.copy %a %a: Failed with %s"
              pp_path
              from
              pp_path
              to_
              s )

  type key_or_dir = [`Key of key | `Dir of key]

  let fold m k ~init ~f =
    raw_get m k
    >>= function
    | Error _ | Ok None ->
        Lwt.return init
    | Ok (Some (Key _)) ->
        Lwt.return init
    | Ok (Some (Dir m)) ->
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
end

open Tezos_protocol_environment

type rpc = M.key -> M.tree tzresult Lwt.t

type _ Context.kind += Proxy : M.t Context.kind

let ops = (module M : CONTEXT with type t = 'ctxt)

let empty rpc =
  let ctxt = M.{rpc; tree = empty} in
  Context.Context {ops; ctxt; kind = Proxy}

let set_rpc : rpc -> Context.t -> Context.t =
 fun rpc (Context.Context {ops; ctxt; kind} : Context.t) ->
  match kind with
  | Proxy ->
      let ctxt' = {ctxt with rpc = Some rpc} in
      Context.Context {ops; ctxt = ctxt'; kind = Proxy}
  | _ ->
      assert false
