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

open Protocol
open Protocol_client_context

let protocol =
  Protocol_hash.of_b58check_exn
    "Ps9mPmXaRzmzk35gbAYNCAw6UXdE2qoABTHbN2oEEc1qM7CwT9P"

let bake cctxt ?timestamp block command sk =
  let timestamp =
    match timestamp with
    | Some t -> t
    | None -> Time.System.(to_protocol (Tezos_base.Time.System.now ()))
  in
  let protocol_data = {command; signature = Tezos_crypto.Signature.V0.zero} in
  Genesis_block_services.Helpers.Preapply.block
    cctxt
    ~block
    ~timestamp
    ~protocol_data
    []
  >>=? fun (shell_header, _) ->
  let blk = Data.Command.forge shell_header command in
  Shell_services.Chain.chain_id cctxt ~chain:`Main () >>=? fun chain_id ->
  Client_keys_v0.append cctxt sk ~watermark:(Block_header chain_id) blk
  >>=? fun signed_blk -> Shell_services.Injection.block cctxt signed_blk []

let int32_parameter =
  Tezos_clic.parameter (fun _ p ->
      match Int32.of_string p with
      | i32 ->
          if Compare.Int32.(i32 < 0l) then
            failwith "Cannot provide a negative int32"
          else return i32
      | exception _ -> failwith "Cannot read int32")

let file_parameter =
  Tezos_clic.parameter (fun _ p ->
      if not (Sys.file_exists p) then failwith "File doesn't exist: '%s'" p
      else return p)

let fitness_from_uint32 fitness =
  (* definition taken from src/proto_alpha/lib_protocol/src/constants_repr.ml *)
  let version_number = "\002" in
  let int32_to_bytes i =
    let b = Bytes.create 4 in
    TzEndian.set_int32 b 0 i ;
    b
  in
  (* Tenderbake-compatible fitness format.

     We encode the fitness using tenderbake's fitness format.
     In order to do that, we need to select a tenderbake's fitness
     field that won't break tenderbake invariants. When a tenderbake
     protocol parses the fitness: [level], [locked_round], [round] and
     [predecessor_round]. [level] cannot be used as tenderbake
     protocols might not be able to produce higher fitness; if
     present, [locked_round] must be lower or equal than [round],
     [predecessor_round] starts at 0xffffffff which cannot be simply
     incremented; [round] thus may be used but will impact block
     delays. *)
  (* We use (fitness - 1) as the most common way to call this command
     is by providing a fitness of 1 and we don't want block
     delays. Therefore, we want an argument of 1 to result in a round
     0. *)
  let fitness_to_round = Int32.(max 0l (pred fitness)) in
  [
    Bytes.of_string version_number (* version *);
    int32_to_bytes 0l (* level *);
    Bytes.empty (* locked-round *);
    int32_to_bytes (-1l) (* predecessor round *);
    int32_to_bytes fitness_to_round (* round *);
  ]

let timestamp_arg =
  Tezos_clic.arg
    ~long:"timestamp"
    ~placeholder:"date"
    ~doc:"Set the timestamp of the block (and initial time of the chain)"
    (Tezos_clic.parameter (fun _ t ->
         match Time.Protocol.of_notation t with
         | None ->
             failwith "Could not parse value provided to -timestamp option"
         | Some t -> return t))

let test_delay_arg =
  Tezos_clic.default_arg
    ~long:"delay"
    ~placeholder:"time"
    ~doc:"Set the life span of the test chain (in seconds)"
    ~default:(Int64.to_string (Int64.mul 24L 3600L))
    (Tezos_clic.parameter (fun _ t ->
         match Int64.of_string_opt t with
         | None -> failwith "Could not parse value provided to -delay option"
         | Some t -> return t))

let proto_param ~name ~desc t =
  Tezos_clic.param
    ~name
    ~desc
    (Tezos_clic.parameter (fun _ str ->
         Lwt.return (Protocol_hash.of_b58check str)))
    t

let commands () =
  let open Tezos_clic in
  let args = args1 timestamp_arg in
  [
    command
      ~desc:"Activate a protocol"
      args
      (prefixes ["activate"; "protocol"]
      @@ proto_param ~name:"version" ~desc:"Protocol version (b58check)"
      @@ prefixes ["with"; "fitness"]
      @@ param
           ~name:"fitness"
           ~desc:"Hardcoded fitness of the first block (integer)"
           int32_parameter
      @@ prefixes ["and"; "key"]
      @@ Client_keys_v0.Secret_key.source_param
           ~name:"password"
           ~desc:"Activator's key"
      @@ prefixes ["and"; "parameters"]
      @@ param
           ~name:"parameters"
           ~desc:"Protocol parameters (as JSON file)"
           file_parameter
      @@ stop)
      (fun timestamp
           hash
           fitness
           sk
           param_json_file
           (cctxt : Client_context.full)
         ->
        let fitness = fitness_from_uint32 fitness in
        Tezos_stdlib_unix.Lwt_utils_unix.Json.read_file param_json_file
        >>=? fun json ->
        let protocol_parameters =
          Data_encoding.Binary.to_bytes_exn Data_encoding.json json
        in
        bake
          cctxt
          ?timestamp
          cctxt#block
          (Activate {protocol = hash; fitness; protocol_parameters})
          sk
        >>=? fun hash ->
        cctxt#answer "Injected %a" Block_hash.pp_short hash >>= fun () ->
        return_unit);
    command
      ~desc:"Fork a test protocol"
      (args2 timestamp_arg test_delay_arg)
      (prefixes ["fork"; "test"; "protocol"]
      @@ proto_param ~name:"version" ~desc:"Protocol version (b58check)"
      @@ prefixes ["with"; "key"]
      @@ Client_keys_v0.Secret_key.source_param
           ~name:"password"
           ~desc:"Activator's key"
      @@ stop)
      (fun (timestamp, delay) hash sk cctxt ->
        bake
          cctxt
          ?timestamp
          cctxt#block
          (Activate_testchain {protocol = hash; delay})
          sk
        >>=? fun hash ->
        cctxt#answer "Injected %a" Block_hash.pp_short hash >>= fun () ->
        return_unit);
  ]

let () = Client_commands.register protocol @@ fun _network -> commands ()
