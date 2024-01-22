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
   Component: big_maps RPC
   Invocation: dune exec tezt/tests/main.exe -- big_map_all
   Subject: Check that RPC [/chain/<chain_id>/blocks/<block_id>/context/big_maps]
            added in protocol G behaves correctly with and without pagination.
*)

let init ~protocol =
  let* node = Node.init [Synchronisation_threshold 0; Connections 0] in
  let* client = Client.init ~endpoint:(Node node) () in
  (* This configuration override is necessary to create "big" big maps
     (hundreds of entries) *)
  let* parameter_file =
    Protocol.write_parameter_file
      ~base:(Either.right (protocol, None))
      [(["hard_storage_limit_per_operation"], `String_of_int 99999999)]
  in
  let* () = Client.activate_protocol ~protocol ~parameter_file client in
  Log.info "Activated protocol." ;
  return (node, client)

let values_to_string l =
  l |> List.map string_of_int |> fun l -> "[" ^ String.concat "; " l ^ "]"

let opt_to_string = Option.fold ~none:"none" ~some:string_of_int

let big_map_size = 10

let all_values = List.init big_map_size Fun.id

(** [any_in_range_inclusive min max] returns a random int [n] such
    that [min <= n <= max].
*)
let any_in_range_inclusive min max = Random.int (max + 1 - min) + min

let rpc_big_map_get_all ?offset ?length client =
  let* json =
    Client.RPC.call client
    @@ RPC.get_chain_block_context_big_maps ~id:"4" ?offset ?length ()
  in
  Lwt.return
    (json |> JSON.as_list
    |> List.map (fun elem ->
           elem |> JSON.get "int" |> JSON.as_string |> int_of_string))

let check error_message ~actual ~expected =
  if not (actual = expected) then
    Test.fail
      "big_map_get_all: %s: actual %s is different from expected %s"
      error_message
      (values_to_string actual)
      (values_to_string expected)
  else ()

let test_all_values actual_all_values =
  check
    "(no offset, no length) should return all values (order being ignored)"
    ~actual:(List.sort Int.compare actual_all_values)
    ~expected:all_values

let test_all_values_explicit_params actual_all_values client =
  let* actual_all_values_explicit =
    rpc_big_map_get_all ~offset:0 ~length:big_map_size client
  in
  Lwt.return
  @@ check
       "(offset = 0, length = big_map_size) should be the same as not passing \
        offset and length"
       ~actual:actual_all_values_explicit
       ~expected:actual_all_values

let test_no_value client =
  let* actual_all_values_explicit =
    rpc_big_map_get_all ?offset:None ~length:0 client
  in
  Lwt.return
  @@ check
       "(offset = none, length = 0) should return an empty list"
       ~actual:actual_all_values_explicit
       ~expected:[]

let test_bigger_length_is_ok actual_all_values client =
  let* actual_big_length =
    rpc_big_map_get_all ?offset:None ~length:(big_map_size + 10) client
  in
  Lwt.return
  @@ check
       "(no offset, length > big_map_size) should be the same as not passing \
        offset and length"
       ~actual:actual_big_length
       ~expected:actual_all_values

let test_bigger_offset_is_empty client =
  let* actual_big_offset =
    rpc_big_map_get_all ~offset:big_map_size ?length:None client
  in
  Lwt.return
  @@ check
       "(offset >= big_map_size, length = none) should be an empty result"
       ~actual:actual_big_offset
       ~expected:[]

let test_1_pivot actual_all_values client =
  (* Any pivot in [1; big_map_size] interval. Length of 0 is forbidden by the RPC,
     and we pass the pivot to [~length] (see below). *)
  let pivot = any_in_range_inclusive 1 big_map_size in
  let* actual_before_pivot =
    rpc_big_map_get_all ?offset:None ~length:pivot client
  in
  let* actual_after_pivot =
    rpc_big_map_get_all ~offset:pivot ?length:None client
  in
  Lwt.return
  @@ check
       "(pivot on offset/length) 2 requests with 1 pivot should be equal to \
        all values"
       ~actual:(actual_before_pivot @ actual_after_pivot)
       ~expected:actual_all_values

let test_2_pivots actual_all_values client =
  (* Any pivot in [1; big_map_size - 2] interval *)
  let pivot1 = any_in_range_inclusive 1 (big_map_size - 2) in
  (* Any pivot in [pivot1 + 1; big_map_size] interval *)
  let pivot2 = any_in_range_inclusive (pivot1 + 1) big_map_size in
  let* actual_before_pivot1 =
    rpc_big_map_get_all ?offset:None ~length:pivot1 client
  in
  let* actual_between_pivots =
    rpc_big_map_get_all ~offset:pivot1 ~length:(pivot2 - pivot1) client
  in
  let* actual_after_pivot2 =
    rpc_big_map_get_all ~offset:pivot2 ?length:None client
  in
  Lwt.return
  @@ check
       "(pivot on offset/length) 3 requests with 2 pivots should be equal to \
        all values"
       ~actual:
         (actual_before_pivot1 @ actual_between_pivots @ actual_after_pivot2)
       ~expected:actual_all_values

let test_invalid_input_fail client =
  let must_fail ?offset ?length () =
    let*? process =
      Client.RPC.spawn client
      @@ RPC.get_chain_block_context_big_maps ~id:"0" ?offset ?length ()
    in
    Process.check ~expect_failure:true process
  in
  let* _ = must_fail ?offset:None ~length:(-1) () in
  let* _ = must_fail ~offset:(-1) ?length:None () in
  let* _ = must_fail ~offset:(-1) ~length:(-1) () in
  Lwt.return_unit

let test_wrapper =
  Protocol.register_test
    ~__FILE__
    ~title:
      "RPC \
       /chain/<chain_id>/blocks/<block_id>/context/big_maps/<big_map_id>?offset=[int]&length=[int]"
    ~tags:["big_map_all"; "rpc"]
  @@ fun protocol ->
  let* _, client = init ~protocol in
  let entries : (string * int) list =
    List.map (fun i -> (Format.sprintf "\"%04i\"" i, i)) all_values
  in
  let entries_s =
    List.map (fun (k, v) -> sf "Elt %s %s " k @@ Int.to_string v) entries
  in
  let init = "{" ^ String.concat ";" entries_s ^ "}" in
  let* _ =
    Client.originate_contract_at
      ~amount:Tez.zero
      ~src:"bootstrap1"
      ~init
      ~burn_cap:Tez.(of_int 9999999)
      client
      ["mini_scenarios"; "big_map_all"]
      protocol
  in
  let* () = Client.bake_for_and_wait client in
  let* actual_all_values =
    rpc_big_map_get_all ?offset:None ?length:None client
  in
  test_all_values actual_all_values ;
  let* () = test_all_values_explicit_params actual_all_values client in
  let* () = test_bigger_length_is_ok actual_all_values client in
  let* () = test_bigger_offset_is_empty client in
  let* () = test_1_pivot actual_all_values client in
  let* () = test_2_pivots actual_all_values client in
  let* () = test_no_value client in
  let* () = test_invalid_input_fail client in
  Lwt.return_unit

let register ~protocols = test_wrapper protocols
