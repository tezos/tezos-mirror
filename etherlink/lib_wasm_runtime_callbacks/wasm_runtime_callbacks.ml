(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

type scope =
  | Root of {scope : Opentelemetry.Scope.t option; trace_host_funs : bool}
  | Started of {
      parent : Opentelemetry.Span_id.t option;
      scope : Opentelemetry.Scope.t;
      name : string;
      start_time : Opentelemetry.Timestamp_ns.t;
      trace_host_funs : bool;
    }

let root_scope ?(trace_host_funs = false) scope = Root {scope; trace_host_funs}

let prepare_new_span = function
  | Root {scope = None; trace_host_funs} ->
      (Opentelemetry.Trace_id.create (), None, trace_host_funs)
  | Root {scope = Some scope; trace_host_funs}
  | Started {scope; trace_host_funs; _} ->
      (scope.trace_id, Some scope.span_id, trace_host_funs)

let prepare_span_for_host_function = function
  | Root {scope = None; _} -> None
  | Root {trace_host_funs = false; _} | Started {trace_host_funs = false; _} ->
      None
  | Root {scope = Some scope; _} | Started {scope; _} ->
      Some (scope.trace_id, scope.span_id)

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
  let otel_trace name scope key f =
    match prepare_span_for_host_function scope with
    | None -> f None
    | Some (trace_id, parent) ->
        Opentelemetry_lwt.Trace.with_
          ~trace_id
          ~parent
          ~service_name:"Host_funs"
          name
        @@ fun scope ->
        Opentelemetry.Scope.add_attrs scope (fun () ->
            [("durable_storage.key", `String (String.concat "/" ("" :: key)))]) ;
        f (Some scope)

  let read_durable_value tree key =
    let open Lwt_result_syntax in
    Key_parser.with_key key @@ fun key ->
    Lwt_domain.run_in_main @@ fun () ->
    let* vec = Vector.get tree key in
    let*! bytes = Vector.load_all vec in
    return (Bytes.unsafe_of_string bytes)

  let store_delete scope tree key is_value =
    Key_parser.with_key key @@ fun key ->
    Lwt_domain.run_in_main (fun () ->
        otel_trace "store_delete" scope key @@ fun _ ->
        let open Lwt_syntax in
        let key = if is_value then key @ ["@"] else key in
        let+ tree = Irmin_context.Tree.remove tree key in
        Ok tree)

  let store_copy scope tree key1 key2 =
    Key_parser.with_key key1 @@ fun key1 ->
    Key_parser.with_key key2 @@ fun key2 ->
    Lwt_domain.run_in_main (fun () ->
        otel_trace "store_copy" scope key1 @@ fun _ ->
        let open Lwt_syntax in
        let* subtree = Irmin_context.Tree.find_tree tree key1 in
        match subtree with
        | Some subtree ->
            let+ tree = Irmin_context.Tree.add_tree tree key2 subtree in
            Ok tree
        | None -> return (Error Error_code.store_not_a_node))

  let store_move scope tree key1 key2 =
    Key_parser.with_key key1 @@ fun key1 ->
    Key_parser.with_key key2 @@ fun key2 ->
    Lwt_domain.run_in_main (fun () ->
        otel_trace "store_move" scope key1 @@ fun _ ->
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

  let store_has scope tree key =
    Key_parser.with_key key @@ fun key ->
    Lwt_domain.run_in_main (fun () ->
        otel_trace "store_has" scope key @@ fun _ ->
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

  let store_list_size scope tree key =
    Key_parser.with_key key @@ fun key ->
    Lwt_domain.run_in_main (fun () ->
        otel_trace "store_list_size" scope key @@ fun _ ->
        let open Lwt_syntax in
        let+ len = Irmin_context.Tree.length tree key in
        Ok len)

  let store_value_size scope tree key =
    Key_parser.with_key key @@ fun key ->
    Lwt_domain.run_in_main @@ fun () ->
    otel_trace "store_value_size" scope key @@ fun _ ->
    let open Lwt_result_syntax in
    let* vec = Vector.get tree key in
    Vector.length vec

  let store_read scope tree key offset num_bytes =
    Key_parser.with_key key @@ fun key ->
    Lwt_domain.run_in_main @@ fun () ->
    otel_trace "store_read" scope key @@ fun _ ->
    let open Lwt_result_syntax in
    let* vector = Vector.get tree key in
    Vector.load_bytes vector ~offset ~num_bytes

  let store_write scope (tree : Irmin_context.tree) key offset bytes =
    Key_parser.with_key key @@ fun key ->
    Lwt_domain.run_in_main @@ fun () ->
    otel_trace "store_write" scope key @@ fun _ ->
    let open Lwt_result_syntax in
    let* vector = Vector.get ~create_if_absent:true tree key in
    let* vector = Vector.write_bytes vector offset bytes in
    let*! tree = Vector.add_in_tree tree key vector in
    return (tree, Bytes.length bytes)

  let store_write_all scope (tree : Irmin_context.tree) key bytes =
    Key_parser.with_key key @@ fun key ->
    Lwt_domain.run_in_main @@ fun () ->
    otel_trace "store_write_all" scope key @@ fun _ ->
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

  let open_span parent span_name =
    let trace_id, parent, trace_host_funs = prepare_new_span parent in
    let span_id = Opentelemetry.Span_id.create () in
    let scope = Opentelemetry.Scope.make ~trace_id ~span_id () in
    Started
      {
        parent;
        scope;
        name = span_name;
        start_time = Opentelemetry.Timestamp_ns.now_unix_ns ();
        trace_host_funs;
      }

  let close_span scope =
    match scope with
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
  Callback.register "open_span" open_span ;
  Callback.register "close_span" close_span

module Internal_for_tests = struct
  include Impl
  module Vector = Vector
  module Error_code = Error_code
end
