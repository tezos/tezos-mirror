(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Functori <contact@functori.com>                        *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

(** Testing
    -------
    Component:    Bin_evm_node
    Invocation:   dune exec etherlink/bin_node/test/test_bitset_nonce.exe
    Subject:      Tests for Nonce_bitset
*)
open Evm_node_lib_dev

open Tx_queue.Internal_for_tests

let comparable_bitset =
  Check.(
    convert
      (fun ({next_nonce; bitset} : Nonce_bitset.t) ->
        (Z.to_int next_nonce, Tezos_base.Bitset.to_list bitset))
      (tuple2 int (list int)))

let test_register =
  Test.register
    ~uses_node:false
    ~uses_client:false
    ~uses_admin_client:false
    ~__FILE__

let check_nonce ~__LOC__ ~found ~expected =
  let open Check in
  (Z.to_int found = Z.to_int expected)
    int
    ~__LOC__
    ~error_msg:"invalid nonce detected, found %L, expected %R"

let make_bitset_nonce ~__LOC__ (next_nonce, bitset) =
  Nonce_bitset.
    {
      next_nonce = Z.of_int next_nonce;
      bitset =
        WithExceptions.Result.get_ok ~loc:__LOC__
        @@ Tezos_base.Bitset.from_list bitset;
    }

let check_bitset ~__LOC__ bitset_nonce ~expected =
  let open Check in
  let expected_bitset = make_bitset_nonce ~__LOC__ expected in
  (bitset_nonce = expected_bitset)
    comparable_bitset
    ~__LOC__
    ~error_msg:"invalid bitset_nonce detected, found %L, expected %R"

let check_bitset_opt ~__LOC__ bitset ~expected =
  let open Check in
  let expected_bitset = Option.map (make_bitset_nonce ~__LOC__) expected in
  (bitset = expected_bitset)
    (option comparable_bitset)
    ~__LOC__
    ~error_msg:"invalid bitset detected, found %L, expected %R"

let create ~__LOC__ ~next_nonce =
  let bitset = Nonce_bitset.create ~next_nonce:(Z.of_int next_nonce) in
  check_bitset ~__LOC__ bitset ~expected:(next_nonce, []) ;
  bitset

let add ~__LOC__ bitset ~nonce ~expected =
  let bitset =
    WithExceptions.Result.get_ok ~loc:__LOC__
    @@ Nonce_bitset.add bitset ~nonce:Z.(of_int nonce)
  in
  check_bitset ~__LOC__ bitset ~expected ;
  bitset

let add_many ~__LOC__ bitset ~nonce ~expected ~length =
  let bitset =
    WithExceptions.Result.get_ok ~loc:__LOC__
    @@ Nonce_bitset.add_many bitset ~nonce:Z.(of_int nonce) ~length
  in
  check_bitset ~__LOC__ bitset ~expected ;
  bitset

let shift ~__LOC__ bitset ~nonce ~expected =
  let bitset =
    WithExceptions.Result.get_ok ~loc:__LOC__
    @@ Nonce_bitset.shift bitset ~nonce:Z.(of_int nonce)
  in
  check_bitset ~__LOC__ bitset ~expected ;
  bitset

let remove ~__LOC__ bitset ~nonce ~expected =
  let bitset =
    WithExceptions.Result.get_ok ~loc:__LOC__
    @@ Nonce_bitset.remove bitset ~nonce:Z.(of_int nonce)
  in
  check_bitset ~__LOC__ bitset ~expected ;
  bitset

let remove_many ~__LOC__ bitset ~nonce ~expected ~length =
  let bitset =
    WithExceptions.Result.get_ok ~loc:__LOC__
    @@ Nonce_bitset.remove_many bitset ~nonce:Z.(of_int nonce) ~length
  in
  check_bitset ~__LOC__ bitset ~expected ;
  bitset

let next_gap_nonce ~__LOC__ bitset ~expected =
  let found = Nonce_bitset.next_gap bitset in
  check_nonce ~__LOC__ ~found ~expected:(Z.of_int expected)

let shift_then_next_gap_nonce ~__LOC__ ~shift_nonce bitset ~expected =
  let found =
    WithExceptions.Result.get_ok ~loc:__LOC__
    @@ Nonce_bitset.shift_then_next_gap
         ~shift_nonce:(Z.of_int shift_nonce)
         bitset
  in
  check_nonce ~__LOC__ ~found ~expected:(Z.of_int expected)

let add_shift_then_remove =
  test_register ~title:"Add, shift and remove" ~tags:["bitset_nonce"]
  @@ fun () ->
  let bitset = create ~__LOC__ ~next_nonce:0 in
  let bitset = add ~__LOC__ bitset ~nonce:0 ~expected:(0, [0]) in
  let bitset = add ~__LOC__ bitset ~nonce:1 ~expected:(0, [0; 1]) in
  let bitset = add ~__LOC__ bitset ~nonce:2 ~expected:(0, [0; 1; 2]) in
  let bitset = add ~__LOC__ bitset ~nonce:3 ~expected:(0, [0; 1; 2; 3]) in

  let bitset = shift ~__LOC__ bitset ~nonce:1 ~expected:(1, [0; 1; 2]) in
  let bitset = shift ~__LOC__ bitset ~nonce:2 ~expected:(2, [0; 1]) in
  let bitset = shift ~__LOC__ bitset ~nonce:3 ~expected:(3, [0]) in
  let bitset = shift ~__LOC__ bitset ~nonce:4 ~expected:(4, []) in

  let bitset = add ~__LOC__ bitset ~nonce:4 ~expected:(4, [0]) in
  let bitset = add ~__LOC__ bitset ~nonce:5 ~expected:(4, [0; 1]) in
  let bitset = add ~__LOC__ bitset ~nonce:6 ~expected:(4, [0; 1; 2]) in
  let bitset = add ~__LOC__ bitset ~nonce:7 ~expected:(4, [0; 1; 2; 3]) in

  let bitset = remove ~__LOC__ bitset ~nonce:4 ~expected:(4, [1; 2; 3]) in
  let bitset = remove ~__LOC__ bitset ~nonce:5 ~expected:(4, [2; 3]) in
  let bitset = remove ~__LOC__ bitset ~nonce:6 ~expected:(4, [3]) in
  let bitset = remove ~__LOC__ bitset ~nonce:7 ~expected:(4, []) in

  let* () =
    Log.info "cardinal %d" (Tezos_base.Bitset.cardinal bitset.bitset) ;
    if not (Nonce_bitset.is_empty bitset) then Test.fail "bitset is not empty"
    else unit
  in
  unit

let add_shift_then_remove_many =
  test_register ~title:"Add_many, shift and remove_many" ~tags:["bitset_nonce"]
  @@ fun () ->
  let bitset = create ~__LOC__ ~next_nonce:0 in
  let bitset = add_many ~__LOC__ bitset ~nonce:0 ~length:1 ~expected:(0, [0]) in
  let bitset =
    add_many ~__LOC__ bitset ~nonce:1 ~length:2 ~expected:(0, [0; 1; 2])
  in
  let bitset =
    add_many ~__LOC__ bitset ~nonce:3 ~length:3 ~expected:(0, [0; 1; 2; 3; 4; 5])
  in

  let bitset = shift ~__LOC__ bitset ~nonce:1 ~expected:(1, [0; 1; 2; 3; 4]) in
  let bitset = shift ~__LOC__ bitset ~nonce:2 ~expected:(2, [0; 1; 2; 3]) in
  let bitset = shift ~__LOC__ bitset ~nonce:3 ~expected:(3, [0; 1; 2]) in
  let bitset = shift ~__LOC__ bitset ~nonce:4 ~expected:(4, [0; 1]) in
  let bitset = shift ~__LOC__ bitset ~nonce:5 ~expected:(5, [0]) in
  let bitset = shift ~__LOC__ bitset ~nonce:6 ~expected:(6, []) in

  let bitset =
    add_many ~__LOC__ bitset ~nonce:6 ~length:4 ~expected:(6, [0; 1; 2; 3])
  in

  let bitset =
    remove_many ~__LOC__ bitset ~nonce:6 ~length:2 ~expected:(6, [2; 3])
  in
  let bitset =
    remove_many ~__LOC__ bitset ~nonce:8 ~length:2 ~expected:(6, [])
  in

  let* () =
    Log.info "cardinal %d" (Tezos_base.Bitset.cardinal bitset.bitset) ;
    if not (Nonce_bitset.is_empty bitset) then Test.fail "bitset is not empty"
    else unit
  in
  unit

let test_next_gap_nonce =
  test_register
    ~title:"next gap nonce is correctly computed"
    ~tags:["bitset_nonce"; "next_gap"]
  @@ fun () ->
  let nonce = 0 in
  let bitset = create ~__LOC__ ~next_nonce:nonce in
  next_gap_nonce ~__LOC__ bitset ~expected:0 ;
  shift_then_next_gap_nonce ~__LOC__ bitset ~shift_nonce:nonce ~expected:0 ;
  shift_then_next_gap_nonce ~__LOC__ bitset ~shift_nonce:(nonce + 1) ~expected:1 ;

  let bitset = add ~__LOC__ bitset ~nonce:0 ~expected:(nonce, [0]) in
  next_gap_nonce ~__LOC__ bitset ~expected:1 ;
  shift_then_next_gap_nonce ~__LOC__ bitset ~shift_nonce:nonce ~expected:1 ;
  shift_then_next_gap_nonce ~__LOC__ bitset ~shift_nonce:(nonce + 1) ~expected:1 ;
  shift_then_next_gap_nonce ~__LOC__ bitset ~shift_nonce:(nonce + 2) ~expected:2 ;

  let bitset = add ~__LOC__ bitset ~nonce:1 ~expected:(nonce, [0; 1]) in
  next_gap_nonce ~__LOC__ bitset ~expected:2 ;
  shift_then_next_gap_nonce ~__LOC__ bitset ~shift_nonce:nonce ~expected:2 ;
  shift_then_next_gap_nonce ~__LOC__ bitset ~shift_nonce:(nonce + 3) ~expected:3 ;

  let bitset = add ~__LOC__ bitset ~nonce:3 ~expected:(nonce, [0; 1; 3]) in
  next_gap_nonce ~__LOC__ bitset ~expected:2 ;
  shift_then_next_gap_nonce ~__LOC__ bitset ~shift_nonce:(nonce + 3) ~expected:4 ;

  let bitset = add ~__LOC__ bitset ~nonce:2 ~expected:(nonce, [0; 1; 2; 3]) in
  next_gap_nonce ~__LOC__ bitset ~expected:4 ;
  shift_then_next_gap_nonce ~__LOC__ bitset ~shift_nonce:nonce ~expected:4 ;

  let bitset = remove ~__LOC__ bitset ~nonce:1 ~expected:(nonce, [0; 2; 3]) in
  next_gap_nonce ~__LOC__ bitset ~expected:1 ;
  shift_then_next_gap_nonce ~__LOC__ bitset ~shift_nonce:nonce ~expected:1 ;
  shift_then_next_gap_nonce ~__LOC__ bitset ~shift_nonce:(nonce + 1) ~expected:1 ;
  shift_then_next_gap_nonce ~__LOC__ bitset ~shift_nonce:(nonce + 2) ~expected:4 ;

  let bitset = remove ~__LOC__ bitset ~nonce:0 ~expected:(nonce, [2; 3]) in
  next_gap_nonce ~__LOC__ bitset ~expected:0 ;
  shift_then_next_gap_nonce ~__LOC__ bitset ~shift_nonce:nonce ~expected:0 ;
  shift_then_next_gap_nonce ~__LOC__ bitset ~shift_nonce:(nonce + 1) ~expected:1 ;
  shift_then_next_gap_nonce ~__LOC__ bitset ~shift_nonce:(nonce + 2) ~expected:4 ;
  unit

module Address_nonce_helpers = struct
  let find_bitset_nonce nonces ~addr = Address_nonce.find nonces ~addr

  let get_bitset_nonce nonces ~addr ~__LOC__ =
    WithExceptions.Option.get ~loc:__LOC__ @@ find_bitset_nonce nonces ~addr

  let add_nonce nonces ~addr ~__LOC__ ~next_nonce ~nonce =
    WithExceptions.Result.get_ok ~loc:__LOC__
    @@ Address_nonce.add
         nonces
         ~addr
         ~next_nonce:(Z.of_int next_nonce)
         ~nonce:(Z.of_int nonce)
         ~add:(fun bitset nonce -> Nonce_bitset.add bitset ~nonce)

  let next_gap_nonce nonces ~addr ~__LOC__ ~next_nonce =
    WithExceptions.Result.get_ok ~loc:__LOC__
    @@ Address_nonce.next_gap nonces ~addr ~next_nonce:(Z.of_int next_nonce)

  let confirm_nonce nonces ~addr ~__LOC__ ~nonce =
    WithExceptions.Result.get_ok ~loc:__LOC__
    @@ Address_nonce.confirm_nonce
         nonces
         ~addr
         ~nonce:(Z.of_int nonce)
         ~next:Z.succ

  let remove_nonce nonces ~addr ~__LOC__ ~nonce =
    WithExceptions.Result.get_ok ~loc:__LOC__
    @@ Address_nonce.remove
         nonces
         ~addr
         ~nonce:(Z.of_int nonce)
         ~rm:(fun bitset nonce -> Nonce_bitset.remove bitset ~nonce)

  let add nonces ~addr ~__LOC__ ~next_nonce ~nonce ~expected =
    add_nonce nonces ~addr ~__LOC__ ~next_nonce ~nonce ;
    let bitset = get_bitset_nonce nonces ~addr ~__LOC__ in
    check_bitset ~__LOC__ bitset ~expected

  let confirm nonces ~addr ~__LOC__ ~nonce ~expected =
    confirm_nonce nonces ~addr ~__LOC__ ~nonce ;
    let bitset = find_bitset_nonce nonces ~addr in
    check_bitset_opt ~__LOC__ bitset ~expected

  let remove nonces ~addr ~__LOC__ ~nonce ~expected =
    remove_nonce nonces ~addr ~__LOC__ ~nonce ;
    let bitset = find_bitset_nonce nonces ~addr in
    check_bitset_opt ~__LOC__ bitset ~expected

  let next_gap nonces ~addr ~__LOC__ ~next_nonce ~expected =
    let found = next_gap_nonce nonces ~addr ~__LOC__ ~next_nonce in
    check_nonce ~__LOC__ ~found ~expected:(Z.of_int expected)
end

let test_address_nonces =
  test_register
    ~title:
      "Add multiple nonces to an address in the Address_nonces hashtbl then \
       confirm or delete them"
    ~tags:["bitset_nonce"; "address_nonce"]
  @@ fun () ->
  let nonces = Address_nonce.empty ~start_size:20 in
  let addr = "bootstrap" in
  let next_gap = Address_nonce_helpers.next_gap nonces ~addr in
  let add = Address_nonce_helpers.add nonces ~addr in
  let confirm = Address_nonce_helpers.confirm nonces ~addr in
  let remove = Address_nonce_helpers.remove nonces ~addr in

  let next_nonce = 0 in
  next_gap ~__LOC__ ~next_nonce ~expected:0 ;
  add ~__LOC__ ~next_nonce ~nonce:0 ~expected:(next_nonce, [0]) ;

  next_gap ~__LOC__ ~next_nonce:10 ~expected:10 ;
  add ~__LOC__ ~next_nonce ~nonce:1 ~expected:(next_nonce, [0; 1]) ;
  add ~__LOC__ ~next_nonce ~nonce:2 ~expected:(next_nonce, [0; 1; 2]) ;
  add ~__LOC__ ~next_nonce ~nonce:3 ~expected:(next_nonce, [0; 1; 2; 3]) ;
  confirm
    ~__LOC__
    ~nonce:next_nonce
    ~expected:(Some (next_nonce + 1, [0; 1; 2])) ;

  let next_nonce = 1 in
  confirm ~__LOC__ ~nonce:next_nonce ~expected:(Some (next_nonce + 1, [0; 1])) ;

  let next_nonce = 2 in
  confirm ~__LOC__ ~nonce:next_nonce ~expected:(Some (next_nonce + 1, [0])) ;

  let next_nonce = 3 in
  confirm ~__LOC__ ~nonce:next_nonce ~expected:None ;
  add ~__LOC__ ~next_nonce ~nonce:4 ~expected:(next_nonce, [1]) ;
  add ~__LOC__ ~next_nonce ~nonce:next_nonce ~expected:(next_nonce, [0; 1]) ;
  remove ~__LOC__ ~nonce:next_nonce ~expected:(Some (next_nonce, [1])) ;
  remove ~__LOC__ ~nonce:4 ~expected:None ;
  unit

let () =
  add_shift_then_remove ;
  add_shift_then_remove_many ;
  test_next_gap_nonce ;
  test_address_nonces

let () = Test.run ()
