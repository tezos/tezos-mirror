(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs <contact@nomadic-labs.com>                *)
(* Copyright (c) 2025 Functori <contact@functori.com>                        *)
(*                                                                           *)
(*****************************************************************************)

type scope =
  | Root of {scope : Opentelemetry.Scope.t option}
  | Started of {
      parent : Opentelemetry.Span_id.t option;
      scope : Opentelemetry.Scope.t;
      name : string;
      start_time : Opentelemetry.Timestamp_ns.t;
    }

let root_scope scope = Root {scope}

let prepare_new_span = function
  | Root {scope = None} -> (Opentelemetry.Trace_id.create (), None)
  | Root {scope = Some scope} | Started {scope; _} ->
      (scope.trace_id, Some scope.span_id)

(*
  Rationale:
  The purpose of this design is to maintain a separate workflow context for each domain.
  Since callbacks can be executed from different domains, we cannot rely on shared
  global state for managing the list of scopes. Instead, we associate each domain with
  its own Domain-Local Storage (DLS) key, stored in the `domain_admin` hashtable.

  This DLS key holds a list of scopes representing the current execution context
  within that domain. When a domain is reused, its DLS is automatically reset, ensuring
  that scope data does not leak between runs. This approach guarantees thread safety
  and enables safe parallel execution, as each domain accesses and updates only its
  own scope list.
*)
let domain_admin : (Domain.id, scope list Domain.DLS.key) Stdlib.Hashtbl.t =
  Stdlib.Hashtbl.create 8

let domain_admin_lock = Mutex.create ()

let init_dls_key () =
  let domain_id = Domain.self () in
  let dls_key = Domain.DLS.new_key (fun () -> []) in
  Mutex.protect domain_admin_lock @@ fun () ->
  Stdlib.Hashtbl.replace domain_admin domain_id dls_key

let add_dls_scope scope =
  let domain_id = Domain.self () in
  let dls_key =
    Mutex.protect domain_admin_lock @@ fun () ->
    Stdlib.Hashtbl.find domain_admin domain_id
  in
  let scopes = Domain.DLS.get dls_key in
  Domain.DLS.set dls_key (scope :: scopes)

let latest_dls_scope () =
  let domain_id = Domain.self () in
  let dls_key =
    Mutex.protect domain_admin_lock @@ fun () ->
    Stdlib.Hashtbl.find domain_admin domain_id
  in
  List.hd @@ Domain.DLS.get dls_key

let pop_latest_dls_scope () =
  let domain_id = Domain.self () in
  let dls_key =
    Mutex.protect domain_admin_lock @@ fun () ->
    Stdlib.Hashtbl.find domain_admin domain_id
  in
  match Domain.DLS.get dls_key with
  | hd :: tl ->
      Domain.DLS.set dls_key tl ;
      hd
  | [] -> Root {scope = None}

let create_scope scope scope_name =
  let trace_id, parent = prepare_new_span scope in
  let span_id = Opentelemetry.Span_id.create () in
  let scope = Opentelemetry.Scope.make ~trace_id ~span_id () in
  let scope =
    Started
      {
        parent;
        scope;
        name = scope_name;
        start_time = Opentelemetry.Timestamp_ns.now_unix_ns ();
      }
  in
  add_dls_scope scope

module Key_parser = struct
  let max_key_length = 250 - String.length "/durable" - String.length "/@"

  let key_of_string_exn s =
    if String.length s > max_key_length then
      raise (Invalid_argument "key_of_string_exn") ;
    let key =
      match String.split_on_char '/' s with
      | "" :: tl -> tl (* Must start with '/' *)
      | _ -> raise (Invalid_argument "key_of_string_exn")
    in
    let assert_valid_char = function
      | '.' | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '-' | '_' -> ()
      | _ -> raise (Invalid_argument "key_of_string_exn")
    in
    let all_steps_valid =
      List.for_all (fun x ->
          x <> ""
          &&
          (String.iter assert_valid_char x ;
           true))
    in
    if all_steps_valid key then key
    else raise (Invalid_argument "key_of_string_exn")

  let of_string_opt s =
    try Some (key_of_string_exn s) with Invalid_argument _ -> None

  let with_key key (k : _ -> (_, Error_code.t) result) =
    match of_string_opt key with
    | Some key -> k ("durable" :: key)
    | None -> Error Error_code.store_invalid_key
end

module Impl = struct
  let read_durable_value tree key =
    let open Lwt_result_syntax in
    Key_parser.with_key key @@ fun key ->
    Lwt_domain.run_in_main @@ fun () ->
    let* vec = Vector.get tree key in
    let*! bytes = Vector.load_all vec in
    return (Bytes.unsafe_of_string bytes)

  let store_delete tree key is_value =
    Key_parser.with_key key @@ fun key ->
    Lwt_domain.run_in_main (fun () ->
        let open Lwt_syntax in
        let key = if is_value then key @ ["@"] else key in
        let+ tree = Irmin_context.Tree.remove tree key in
        Ok tree)

  let store_copy tree key1 key2 =
    Key_parser.with_key key1 @@ fun key1 ->
    Key_parser.with_key key2 @@ fun key2 ->
    Lwt_domain.run_in_main (fun () ->
        let open Lwt_syntax in
        let* subtree = Irmin_context.Tree.find_tree tree key1 in
        match subtree with
        | Some subtree ->
            let+ tree = Irmin_context.Tree.add_tree tree key2 subtree in
            Ok tree
        | None -> return (Error Error_code.store_not_a_node))

  let store_move tree key1 key2 =
    Key_parser.with_key key1 @@ fun key1 ->
    Key_parser.with_key key2 @@ fun key2 ->
    Lwt_domain.run_in_main (fun () ->
        let open Lwt_syntax in
        let* subtree = Irmin_context.Tree.find_tree tree key1 in
        match subtree with
        | Some subtree ->
            let* tree = Irmin_context.Tree.remove tree key1 in
            let+ tree = Irmin_context.Tree.add_tree tree key2 subtree in
            Ok tree
        | None -> return (Error Error_code.store_not_a_node))

  let mem_tree tree key =
    Key_parser.with_key key @@ fun key ->
    Lwt_domain.run_in_main (fun () ->
        let open Lwt_syntax in
        let+ exists = Irmin_context.Tree.mem_tree tree key in
        Ok exists)

  let store_has_unknown_key = 0

  let store_has_value_only = 1

  let store_has_subtrees_only = 2

  let store_has_value_and_subtrees = 3

  let store_has tree key =
    Key_parser.with_key key @@ fun key ->
    Lwt_domain.run_in_main (fun () ->
        let open Lwt_syntax in
        let* value_exists = Irmin_context.Tree.mem_tree tree (key @ ["@"]) in
        let* num_subtrees = Irmin_context.Tree.length tree key in
        match (value_exists, num_subtrees) with
        | false, 0 -> return_ok store_has_unknown_key
        | true, 1 -> return_ok store_has_value_only
        | false, _ -> return_ok store_has_subtrees_only
        | _ -> return_ok store_has_value_and_subtrees)

  let store_get_hash tree key =
    Key_parser.with_key key @@ fun key ->
    let findee =
      Lwt_domain.run_in_main (fun () -> Irmin_context.Tree.find_tree tree key)
    in
    match findee with
    | Some tree -> Ok (Irmin_context.Tree.hash tree |> Context_hash.to_bytes)
    | None -> Error Error_code.store_not_a_node

  let store_list_size tree key =
    Key_parser.with_key key @@ fun key ->
    Lwt_domain.run_in_main (fun () ->
        let open Lwt_syntax in
        let+ len = Irmin_context.Tree.length tree key in
        Ok len)

  let store_value_size tree key =
    Key_parser.with_key key @@ fun key ->
    Lwt_domain.run_in_main @@ fun () ->
    let open Lwt_result_syntax in
    let* vec = Vector.get tree key in
    Vector.length vec

  let store_read tree key offset num_bytes =
    Key_parser.with_key key @@ fun key ->
    Lwt_domain.run_in_main @@ fun () ->
    let open Lwt_result_syntax in
    let* vector = Vector.get tree key in
    Vector.load_bytes vector ~offset ~num_bytes

  let store_write (tree : Irmin_context.tree) key offset bytes =
    Key_parser.with_key key @@ fun key ->
    Lwt_domain.run_in_main @@ fun () ->
    let open Lwt_result_syntax in
    let* vector = Vector.get ~create_if_absent:true tree key in
    let* vector = Vector.write_bytes vector offset bytes in
    let*! tree = Vector.add_in_tree tree key vector in
    return (tree, Bytes.length bytes)

  let store_write_all (tree : Irmin_context.tree) key bytes =
    Key_parser.with_key key @@ fun key ->
    Lwt_domain.run_in_main @@ fun () ->
    let open Lwt_result_syntax in
    let*! vector = Vector.empty () in
    let* vector = Vector.write_bytes vector 0 bytes in
    let*! tree = Vector.add_in_tree tree key vector in
    return tree

  let reboot_flag = Key_parser.key_of_string_exn "/durable/kernel/env/reboot"

  let check_reboot_flag tree =
    Lwt_domain.run_in_main (fun () ->
        let open Lwt_syntax in
        let* exists = Irmin_context.Tree.mem_tree tree reboot_flag in
        let+ tree = Irmin_context.Tree.remove tree reboot_flag in
        (exists, tree))

  let fetch_preimage_from_remote preimages_endpoint hash_hex =
    Lwt_domain.run_in_main (fun () ->
        let open Lwt_syntax in
        let preimages_endpoint = Uri.of_string preimages_endpoint in
        let url =
          Uri.with_path
            preimages_endpoint
            String.(concat "/" [Uri.path preimages_endpoint; hash_hex])
        in
        let* resp, body = Cohttp_lwt_unix.Client.get url in
        let* body_str = Cohttp_lwt.Body.to_string body in
        match resp.status with
        | `OK -> return body_str
        | #Cohttp.Code.status_code -> raise Not_found)

  let init_spans scope scope_name =
    init_dls_key () ;
    create_scope scope scope_name

  let start_span scope_name =
    Option.iter
      (fun dls_scope -> create_scope dls_scope scope_name)
      (latest_dls_scope ())

  let span_add_attrs attrs =
    match latest_dls_scope () with
    (* No started scope to which to attach attributes *)
    | None | Some (Root _) -> ()
    | Some (Started {scope; _}) ->
        Opentelemetry.Scope.add_attrs scope @@ fun () ->
        List.map
          (fun (k, v) ->
            let (v : Opentelemetry.value) =
              match v with
              | `Bool b -> `Bool b
              | `Int i -> `Int (Int32.to_int i)
              | `Float f -> `Float f
              | `String s -> `String s
            in
            (k, v))
          attrs

  let end_span () =
    match pop_latest_dls_scope () with
    | Root _ -> (* No started scope to close *) ()
    | Started {parent; scope; name; start_time; _} ->
        let end_time = Opentelemetry.Timestamp_ns.now_unix_ns () in
        let span, _ =
          Opentelemetry.Span.create
            ~links:(Opentelemetry.Scope.links scope)
            ~attrs:(Opentelemetry.Scope.attrs scope)
            ~events:(Opentelemetry.Scope.events scope)
            ?parent
            ~id:scope.span_id
            ~trace_id:scope.trace_id
            ~start_time
            ~end_time
            name
        in
        Opentelemetry.Trace.emit ~service_name:"Wasm_runtime" [span]
end

include Impl

let register () =
  (* Preimage download helper. *)
  Callback.register "fetch_preimage_from_remote" fetch_preimage_from_remote ;

  (* General-purpose helpers *)
  Callback.register "layer2_store__read_durable_value" read_durable_value ;
  Callback.register "layer2_store__mem_tree" mem_tree ;
  Callback.register "layer2_store__check_reboot_flag" check_reboot_flag ;

  (* Host functions helpers *)
  Callback.register "layer2_store__store_delete" store_delete ;
  Callback.register "layer2_store__store_copy" store_copy ;
  Callback.register "layer2_store__store_move" store_move ;
  Callback.register "layer2_store__store_has" store_has ;
  Callback.register "layer2_store__store_get_hash" store_get_hash ;
  Callback.register "layer2_store__store_list_size" store_list_size ;
  Callback.register "layer2_store__store_value_size" store_value_size ;
  Callback.register "layer2_store__store_read" store_read ;
  Callback.register "layer2_store__store_write" store_write ;
  Callback.register "layer2_store__store_write_all" store_write_all ;

  (* Opentelemetry *)
  Callback.register "init_spans" init_spans ;
  Callback.register "start_span" start_span ;
  Callback.register "span_add_attrs" span_add_attrs ;
  Callback.register "end_span" end_span

module Internal_for_tests = struct
  include Impl
  module Vector = Vector
  module Error_code = Error_code
end
