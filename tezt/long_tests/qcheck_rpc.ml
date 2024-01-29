(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Nomadic Labs <contact@nomadic-labs.com>                *)
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

(* Testing
   -------
   Component: Node
   Invocation: dune exec tezt/long_tests/main.exe -- --file qcheck_rpc.ml
   Subject: Property testing the RPC server
*)

(*
   {0 Description}

   This test tries to see if it can crash the node by calling RPCs with random
   inputs. The setup is as follows. A fresh node and client are started and the
   node is brought to protocol alpha. A single RPC is tested on that node by
   calls from the client with random input that obeys the schema of that RPC.
   This is repeated for each RPC with a fresh node and client. The list of RPCs
   is generated from starting a node and getting JSON descriptions from the
   following URLs:
     - ["$rpc_host:$rpc_port/describe/?recurse=yes"]
     - ["$rpc_host:$rpc_port/describe/chains/main/blocks/head?recurse=yes"]
     - ["$rpc_host:$rpc_port/describe/chains/main/mempool?recurse=yes"]

   To see a log of all RPC calls, pass [--log-file] when running.
 *)

(* Test configuration *)
(* ------------------------------------------------------------------------- *)

(* The number of random inputs we generate per RPC *)
let num_rand_inputs = 30

(* We currently are only able to test protocol alpha *)
let protocol = Protocol.Alpha

(* The central types *)
(* ------------------------------------------------------------------------- *)

(* A parameter to a URL of an RPC

   Note: this is not exhaustive; it includes the inputs
   that are easy to generate (e.g., excluding a ["sapling_state_id"])
*)
type path_input =
  | Chain_ID
  | Block_hash
  | List_offset
  | Op_offset
  | Block_level
  | Protocol_hash
  | Rand_string

(* A URL given to an RPC *)
type path = (string, path_input) Either.t list

(* A type representation for the JSON input to an RPC. *)
type rpc_input =
  | Any : rpc_input
  | Boolean : rpc_input
  | Integer : {min : int option; max : int option} -> rpc_input
  | Z : rpc_input (* ["bignum"] *)
  | Int_enum : int list -> rpc_input
  | Float : {min : float option; max : float option} -> rpc_input
  | Even_alphanum : rpc_input (* Even number of alpha numeric [char]s. *)
  | String_enum : string list -> rpc_input
  | Rand_string : rpc_input
  | Array : rpc_input -> rpc_input
  | Object : property list -> rpc_input
  | One_of : rpc_input list -> rpc_input
  | Mich_exp : rpc_input
  | Tree_encoding : rpc_input
  | Inode_tree : rpc_input

(* A JSON object field *)
and property = {name : string; required : bool; payload : rpc_input}

(* This is a description/schema for an RPC *)
type rpc_description = {
  description : string;
  meth : Client.meth;
  path : path;
  data : rpc_input option;
}

(* This is a particular RPC call *)
type rpc_instance = {
  description : string;
  meth : Client.meth;
  full_path : string list;
  input : Ezjsonm.value option;
}

let show_rpc_instance {meth; full_path; input; _} : string =
  let inp_str =
    match input with
    | None -> "none"
    | Some value -> Ezjsonm.value_to_string ~minify:false value
  in
  let meth_str = Client.string_of_meth meth in
  let path_str = String.concat "/" full_path in
  sf "\n\nMeth: %s\nPath:%s\nInput:\n%s\n" meth_str path_str inp_str

(* Dynamically get the list of all RPCs *)
(* ------------------------------------------------------------------------- *)
module RPC_Index = struct
  open Tezos_openapi
  open Tezos_openapi.Openapi
  module String_map = Map.Make (String)

  (* Parsing into [rpc_description]s *)
  (* ------------------------------- *)

  let path_param_form_re = Re.Str.(regexp "^{.+}$")

  (* We only parse fields we can easily generate inputs for.
     E.g., we do not parse ["{sapling_state_id}"].
  *)
  let parse_path_input s : path_input =
    match s with
    | "{chain_id}" -> Chain_ID
    | "{block_hash}" -> Block_hash
    | "{list_offset}" -> List_offset
    | "{operation_offset}" -> Op_offset
    | "{block_level}" -> Block_level
    | "{Protocol_hash}" -> Protocol_hash
    | _ -> Rand_string

  let parse_path_input str : (string, path_input) Either.t =
    let is_path_param = Re.Str.(string_match path_param_form_re str 0) in

    match is_path_param with
    | false -> Either.left str
    | true -> Either.right @@ parse_path_input str

  let parse_path path_string : path =
    (* Note: We use [List.tl] since [split_on_char '/' "/a/b"]
       is [[""; "a"; "b"]].
    *)
    path_string |> String.split_on_char '/' |> List.tl
    |> List.map parse_path_input

  (** [map_e f l] applies the [f] function (that can return [Ok _] or [Error _])
      to all elements of [l]. If [f] succeeded on all elements, [Ok _] of
      all transformed elements is returned, otherwise the first error is returned. *)
  let rec map_e f = function
    | [] -> Ok []
    | x :: xs -> (
        match f x with
        | Ok y -> ( match map_e f xs with Ok ys -> Ok (y :: ys) | err -> err)
        | Error err ->
            (* No need to continue, in particular, no need to recurse *)
            Error err)

  let rec parse_input env schema : (rpc_input, string) result =
    let open Schema in
    match schema with
    (* These first four references are circular. *)
    | Ref "bignum" -> Ok Z
    | Ref "micheline.alpha.michelson_v1.expression" -> Ok Mich_exp
    | Ref "tree_encoding" -> Ok Tree_encoding
    | Ref "inode_tree" -> Ok Inode_tree
    | Ref str -> parse_input env @@ String_map.find str env
    | Other {kind = Boolean; _} -> Ok Boolean
    | Other {kind = Integer {minimum; maximum; enum = None}; _} ->
        Ok (Integer {min = minimum; max = maximum})
    | Other {kind = Integer {enum = Some enum; _}; _} -> Ok (Int_enum enum)
    | Other {kind = Number {minimum; maximum}; _} ->
        Ok (Float {min = minimum; max = maximum})
    | Other {kind = String {pattern = Some s; _}; _} -> (
        (* Only one regexp is used currently *)
        match s with
        | "^([a-zA-Z0-9][a-zA-Z0-9])*$" -> Ok Even_alphanum
        | _ -> Error ("Unexpected regexp: " ^ s))
    | Other {kind = String {enum = None; _}; _} -> Ok Rand_string
    | Other {kind = String {enum = Some enum; _}; _} -> Ok (String_enum enum)
    | Other {kind = Array schema'; _} -> (
        match parse_input env schema' with Ok x -> Ok (Array x) | err -> err)
    | Other {kind = Object {additional_properties = Some _; _}; _} ->
        (* Note: Additional properties never occur in input. *)
        Error "Additional properties are unsupported"
    | Other {kind = Object {properties; _}; _} -> (
        let conv_prop Schema.{name; required; schema} =
          match parse_input env schema with
          | Ok payload -> Ok {name; required; payload}
          | Error err -> Error err
        in
        match map_e conv_prop properties with
        | Ok conv_properties -> Ok (Object conv_properties)
        | Error err -> Error err)
    | Other {kind = One_of schemas; _} -> (
        match map_e (parse_input env) schemas with
        | Ok sub -> Ok (One_of sub)
        | Error err -> Error err)
    | Other {kind = Any; _} -> Ok Any

  let parse_service path env meth service : rpc_description =
    let open Service in
    let data =
      match service.request_body with
      | Some schema -> (
          match parse_input env schema with
          | Ok rpc_input -> Some rpc_input
          | Error err ->
              Test.fail
                "Error when parsing service \"%s\": %s. Can function \
                 parse_input above be generalized?"
                service.description
                err)
      | None -> None
    in
    let description = service.description in
    {description; meth; path; data}

  (* A single endpoint can have multiple RPCs. An endpoint is basically
     just a URL, which can have a different RPC per method.
  *)
  let parse_endpoint env endpoint : rpc_description list =
    let open Endpoint in
    let path_str = Path.to_string endpoint.path in
    let path = parse_path path_str in
    let services =
      Client.
        [
          (GET, Openapi.(Endpoint.get_method endpoint GET));
          (POST, Openapi.(Endpoint.get_method endpoint POST));
          (PUT, Openapi.(Endpoint.get_method endpoint PUT));
          (DELETE, Openapi.(Endpoint.get_method endpoint DELETE));
          (PATCH, Openapi.(Endpoint.get_method endpoint PATCH));
        ]
    in
    let opt_convert (meth, opt_service) =
      opt_service |> Option.map @@ parse_service path env meth
    in
    List.filter_map opt_convert services

  (* Parse [rpc_description]s from the given [env] and [Endpoint.t list]. *)
  let parse_endpoints env endpoints : rpc_description list =
    List.concat_map (parse_endpoint env) endpoints

  (* Dynamically querying all RPCs *)
  (* ----------------------------- *)

  (* Given a list of urls that spit out JSON descriptions of RPCs,
     use the technique from [Rpc_openapi.Openapi] to query a node to get those
     JSON descriptions and parse them.
  *)
  let get_endpoints rpc_port urls : (Convert.env * Endpoint.t) list Lwt.t =
    let get_url_endpoints url =
      let open Lwt_process in
      let* curl_result = pread ~stderr:`Dev_null ("curl", [|"curl"; url|]) in
      curl_result |> JSON.parse ~origin:url |> Api.parse_tree
      |> Api.parse_services |> Api.flatten
      |> List.map Convert.convert_endpoint
      |> return
    in
    let args = Node.[Network "sandbox"; Expected_pow 0; No_bootstrap_peers] in
    let* node = Node.init ~rpc_port args in
    let* client = Client.init ~endpoint:(Node node) () in
    let* () = Client.activate_protocol ~protocol client in
    Lwt_list.map_s get_url_endpoints urls |> Lwt.map List.concat

  (* For the given protocol, get [rpc_description]s for the rpc server. *)
  let get_index () : rpc_description list Lwt.t =
    (* Define urls to query JSON descriptions from *)
    let port = 8732 in
    let url_prefix = sf "http://%s:%d/describe/" Constant.default_host port in
    let shell_url = url_prefix ^ "?recurse=yes" in
    let proto_url = url_prefix ^ "chains/main/blocks/head?recurse=yes" in
    let mempool_url = url_prefix ^ "chains/main/mempool?recurse=yes" in
    let urls = [shell_url; proto_url; mempool_url] in
    let* envs, endpts = Lwt.(get_endpoints port urls >|= List.split) in
    let env = Convert.merge_env_list envs in
    return @@ parse_endpoints env endpts
end

(* Generate random data for RPC calls *)
(* ------------------------------------------------------------------------- *)
module Gen = struct
  type 'a t = 'a QCheck2.Gen.t

  (* Prerequisites of [path_gen] *)
  (* ---------------------------- *)

  let chain_id_gen : string t =
    let open QCheck2.Gen in
    let open Tezos_crypto.Hashed in
    let non_alias =
      list string >|= Chain_id.hash_string >|= Chain_id.to_string
    in
    frequency [(4, pure "main"); (2, pure "test"); (1, non_alias)]

  let block_hash_gen : string t =
    let open QCheck2.Gen in
    let open Tezos_crypto.Hashed in
    let non_alias =
      list string >|= Block_hash.hash_string >|= Block_hash.to_string
    in
    frequency [(4, pure "head"); (2, pure "genesis"); (1, non_alias)]

  let protocol_hash_gen : string t =
    let open QCheck2.Gen in
    let open Tezos_crypto.Hashed in
    list string >|= Protocol_hash.hash_string >|= Protocol_hash.to_string

  let path_int_gen : string t = QCheck2.Gen.(map Int.to_string small_nat)

  (* Prerequisites of [known_input_gen] *)
  (* ----------------------------------- *)

  let nums = "0123456789"

  let alpha = "abcdefghijklmnopqrstuvwxyz"

  let alpha_num_alphabet : char list =
    nums ^ alpha ^ String.capitalize_ascii alpha |> String.to_seq |> List.of_seq

  let even_alpha_num_gen : string t =
    let alpha_num_gen = QCheck2.Gen.oneofl alpha_num_alphabet in
    let even_gen = QCheck2.Gen.map (( * ) 2) QCheck2.Gen.(0 -- 100) in
    QCheck2.Gen.string_size ~gen:alpha_num_gen even_gen

  let rec take n xs : 'a list =
    match (n, xs) with
    | 0, _ -> []
    | _, [] -> []
    | n, y :: ys -> y :: take (n - 1) ys

  let pick_some_elems xs : 'a list t =
    let open QCheck2.Gen in
    let shuffle_gen = pair (shuffle_l xs) (0 -- List.length xs) in
    map (fun (ys, n) -> take n ys) shuffle_gen

  let micheline_exp_gen =
    let open Tezos_alpha_test_helpers.Test_global_constants in
    let open Tezos_protocol_alpha.Protocol.Michelson_v1_primitives in
    let open Tezos_micheline in
    let open Micheline_encoding in
    let open QCheck2.Gen in
    let l_gen = return (-1) in
    let annot_gen = return [] in
    let micheline_node_gen =
      Generators.micheline_node_gen l_gen Generators.prim_gen annot_gen
    in
    let node_encoding =
      table_encoding ~variant:"name" Data_encoding.int31 prim_encoding
    in
    micheline_node_gen >|= Data_encoding.Json.construct node_encoding

  (* TODO: Those trivial implementations are here to prevent the parsing
     of "tree_encoding" and "inode_tree" references from generating stack
     overflows (these references are circular) and crashing tests.
     We ought to make proper implementations if we want the tests
     to be significant. *)
  let tree_encoding_gen = QCheck2.Gen.return `Null

  let inode_tree_gen = QCheck2.Gen.return `Null

  (* Random path generation *)
  (* ---------------------- *)

  let path_gen path : string list t =
    let open QCheck2.Gen in
    let path_str_gen = string_size (1 -- 100) in
    let elem_to_gen : (string, path_input) Either.t -> string t = function
      | Either.Left s -> pure s
      | Either.Right input -> (
          match input with
          | Chain_ID -> chain_id_gen
          | Block_hash -> block_hash_gen
          | List_offset -> path_int_gen
          | Op_offset -> path_int_gen
          | Block_level -> path_int_gen
          | Rand_string -> path_str_gen
          | Protocol_hash -> protocol_hash_gen)
    in
    path |> List.map elem_to_gen |> flatten_l

  (* Random input json generation *)
  (* ---------------------------- *)

  let rec known_input_gen : rpc_input -> Ezjsonm.value t =
    let open QCheck2.Gen in
    function
    | Boolean -> map Ezjsonm.bool bool
    | Integer {min; max} ->
        int_range
          (Option.value min ~default:Int.min_int)
          (Option.value max ~default:Int.max_int)
        >|= Ezjsonm.int
    | Z -> map Ezjsonm.int64 ui64
    | Int_enum enum -> oneofl enum |> map Ezjsonm.int
    | Float {min; max} ->
        float_range
          (Option.value min ~default:Float.min_float)
          (Option.value max ~default:Float.max_float)
        >|= Ezjsonm.float
    | Even_alphanum -> map Ezjsonm.string even_alpha_num_gen
    | String_enum enum -> oneofl enum |> map Ezjsonm.string
    | Any | Rand_string -> map Ezjsonm.string string
    | Array rpc_input ->
        list_size (0 -- 10) (known_input_gen rpc_input) |> map (fun l -> `A l)
    | One_of rpc_inputs -> oneof @@ List.map known_input_gen rpc_inputs
    | Object properties ->
        let open QCheck2.Gen in
        List.partition (fun x -> x.required) properties |> fun (req, not_req) ->
        pick_some_elems not_req >>= fun some_elems ->
        req @ some_elems
        |> List.map (fun {name; payload; _} -> (name, payload))
        |> List.split
        |> fun (names, payloads) ->
        List.map known_input_gen payloads
        |> QCheck2.Gen.flatten_l
        |> QCheck2.Gen.map (fun inputs -> `O (List.combine names inputs))
    | Mich_exp -> micheline_exp_gen
    | Tree_encoding -> tree_encoding_gen
    | Inode_tree -> inode_tree_gen

  let input_gen opt_rpc_input : Ezjsonm.value option t =
    Option.map known_input_gen opt_rpc_input |> QCheck2.Gen.flatten_opt

  (* Random RPC instance generation *)
  (* ------------------------------ *)

  let instance_gen {description; meth; path; data} : rpc_instance t =
    let open QCheck2.Gen in
    pair (input_gen data) (path_gen path)
    |> map (fun (input, full_path) -> {description; meth; full_path; input})
end

(* Test each RPC for whether it crashes the node *)
(* ------------------------------------------------------------------------- *)
module Test = struct
  (* Call an rpc, allowing for failure without calling [Tezt.Test.fail].
     We need this because we are testing the resilience of the node; the
     test fails iff the node given random-input RPC calls (which will almost
     always fail) crashes.
  *)
  let call_rpc client {meth; full_path; input; _} :
      (JSON.t, Unix.process_status) result Lwt.t =
    let proc =
      (* As this test performs many RPC calls, we don't want to log,
         to avoid consuming a lot of storage space on the CI.
         Only checking if the node stays alive matters.

         In case this test needs to be debugged, set all three [log_*] flags
         to [true], to get feedback on the faulty RPC. *)
      Client.spawn_rpc
        ~log_command:false
        ~log_status_on_exit:false
        ~log_output:false
        ?data:(Option.map (fun x : RPC_core.data -> Data x) input)
        meth
        full_path
        client
    in
    let* exit_code = Process.wait proc in
    match exit_code with
    | WEXITED 0 ->
        let* output = Lwt_io.read @@ Process.stdout proc in
        let origin = Client.string_of_path full_path ^ " response" in
        return @@ Ok (JSON.parse ~origin output)
    | _ -> return @@ Error exit_code

  (* Check if the node attached to a client is still alive.

     We assume that if the vanilla node (not proxy) is still
     alive it will return something for "rpc get /version". *)
  let check_node_alive client : bool Lwt.t =
    Lwt.map Result.is_ok
    @@ call_rpc
         client
         {
           description = "";
           meth = Client.GET;
           full_path = ["version"];
           input = None;
         }

  let test_instance client rpc_instance : unit Lwt.t =
    (* To see the RPC being called: *)
    (* let () = Log.info "%s" (show_rpc_instance rpc_instance) in *)
    let* _ = call_rpc client rpc_instance in
    let* node_alive = check_node_alive client in
    match node_alive with
    | true -> unit
    | false -> Tezt.Test.fail "Node has crashed! See log file."

  let test_rpc (rpc_description : rpc_description) : unit Lwt.t =
    (* Log description of RPC *)
    let () = Log.info "%s\n\n" rpc_description.description in
    (* Start node and client *)
    let* node, client = Client.init_with_protocol `Client ~protocol () in
    (* Generate and test instances *)
    let* () =
      rpc_description |> Gen.instance_gen
      |> QCheck2.Gen.generate ~n:num_rand_inputs
      |> Lwt_list.iter_s (test_instance client)
    in
    Node.terminate node

  (* The presence of any of these terms means the RPC is a streaming RPC
     that does not return. We omit these from the scope of this test.

     - /monitor requires to sync with other nodes to function properly
     - /log requires some activity, e.g., pushing operations in parallel
  *)
  let streaming_url = ["monitor"; "log"]

  let filter_description {path; data; _} =
    let not_streaming =
      List.for_all (fun x -> not @@ List.mem (Either.Left x) path) streaming_url
    in
    (* We don't want to fuzz RPCs that have no input: *)
    let not_no_input = Option.is_some data in
    not_streaming && not_no_input

  let test_rpc_server () : unit Lwt.t =
    let* rpc_descriptions = RPC_Index.get_index () in
    let filtered = List.filter filter_description rpc_descriptions in
    Lwt_list.iter_s test_rpc filtered
end

let property_test_rpc_server ~executors =
  Long_test.register
    ~__FILE__
    ~title:"property_test_rpc_server"
    ~tags:["node"; "pbt"; "fuzz"; "rpc"]
    ~executors
    ~timeout:(Hours 1)
    Test.test_rpc_server

let register_for_alpha () = property_test_rpc_server
