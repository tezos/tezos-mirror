(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Nomadic Labs <contact@nomadic-labs.com>                *)
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
   Component:    Baker / liquidity baking
   Invocation:   dune exec tezt/tests/main.exe -- --file liquidity_baking_per_block_votes.ml
   Subject:      Run the baker with various arguments for the [--liquidity-baking-toggle-vote]
                 and [--votefile] options.
*)

let default_votefile = "per_block_votes.json"

let ensure_removal path p =
  Lwt.finalize p @@ fun () ->
  if Sys.file_exists path then Sys.remove path ;
  unit

let baker_wait_for_vote baker =
  Baker.wait_for baker "vote_for_liquidity_baking_toggle.v0" @@ fun json ->
  JSON.as_string json |> Baker.liquidity_baking_vote_of_string_opt

let baker_wait_for_per_block_vote_file_error ?expected_id ?expected_file_path
    baker =
  Baker.wait_for baker "per_block_vote_file_error.v0" @@ fun json ->
  let json = JSON.(json |=> 0) in
  let id = JSON.(json |-> "id" |> as_string) in
  let file_path = JSON.(json |-> "file_path" |> as_string) in
  if
    Option.fold ~none:true ~some:(String.equal id) expected_id
    && Option.fold ~none:true ~some:(String.equal file_path) expected_file_path
  then Some (id, file_path)
  else None

let vote_typ = Check.(convert Baker.liquidity_baking_vote_to_string string)

let check_vote ?__LOC__ expected_vote obtained_vote =
  Check.(obtained_vote = expected_vote)
    vote_typ
    ?__LOC__
    ~error_msg:"expected baker to vote %R, but it voted %L"

let check_vote_success ?__LOC__ expected_vote (baker, vote_test_result) =
  match vote_test_result with
  | Ok obtained_vote ->
      check_vote ?__LOC__ expected_vote obtained_vote ;
      baker
  | Error (error, votefile') ->
      Test.fail
        ?__LOC__
        "Did not expect baker to fail reading the vote file '%s', got error: %s"
        votefile'
        error

let check_vote_error ?__LOC__ expected_error expected_file_path
    (baker, vote_test_result) =
  match vote_test_result with
  | Ok _ ->
      Test.fail
        ?__LOC__
        "Did not expect baker to succeed, expected error '%s' with file_path \
         '%s'"
        expected_error
        expected_file_path
  | Error (obtained_error, obtained_file_path) ->
      Check.(
        (obtained_error = expected_error)
          string
          ?__LOC__
          ~error_msg:"Expected to fail with error %R, got %L") ;
      Check.(
        (obtained_file_path = obtained_file_path)
          string
          ?__LOC__
          ~error_msg:"Expected to fail with file path %R, got %L") ;
      baker

let test_all_per_block_votes =
  (* This test actually supports protocols >= 012. But unfortunately, because
     it creates, uses and then deletes a file at a fixed location ([default_votefile]),
     it cannot be run in parallel with itself for multiple protocols.
     So we only run it on Alpha. *)
  Protocol.register_test
    ~__FILE__
    ~title:"liquidity baking with per-block votes"
    ~tags:["liquidity"; "baking"; "votes"]
    ~supports:
      (Protocol.Between_protocols (Protocol.number Alpha, Protocol.number Alpha))
    ~uses:(fun protocol -> [Protocol.baker protocol])
  @@ fun protocol ->
  let ( >|= ) = Lwt.( >|= ) in
  let error_prefix = "client." ^ Protocol.encoding_prefix protocol ^ "." in

  if Sys.file_exists default_votefile then
    Test.fail
      ~__LOC__
      "this test will recreate and delete the file %s, which is already \
       present on your system. This may be the result of a partial run of this \
       test, in which case the file can safely be removed."
      default_votefile ;

  let parameters =
    [
      (["minimal_block_delay"], `String_of_int 2);
      (["delay_increment_per_round"], `String_of_int 1);
    ]
  in
  let* parameter_file =
    Protocol.write_parameter_file ~base:(Right (protocol, None)) parameters
  in
  let* node, client =
    Client.init_with_protocol
      `Client
      ~protocol
      ~parameter_file
      ~timestamp:Now
      ()
  in

  (* Note that per default, a baker launched with [run_vote_file] will
     not set [--liquidity-baking-toggle-vote pass] as the normal baker
     does. *)
  let run_vote_file ?votefile ?liquidity_baking_toggle_vote baker =
    let* () = Baker.terminate baker in
    let delegates =
      Array.map (fun Account.{alias; _} -> alias) Account.Bootstrap.keys
      |> Array.to_list
    in

    let baker =
      Baker.create
        ~liquidity_baking_toggle_vote
        ?votefile
        ~protocol
        ~delegates
        node
        client
    in
    let p_vote =
      let* vote = baker_wait_for_vote baker in
      return (baker, Ok vote)
    in
    let p_error =
      let* error = baker_wait_for_per_block_vote_file_error baker in
      return (baker, Error error)
    in
    let* () = Baker.run baker in
    let* () = Baker.wait_for_ready baker in
    Lwt.pick [p_vote; p_error]
  in

  let* baker = Baker.init ~protocol node client in

  Log.info "Test [off] vote file" ;
  let* baker =
    let votefile = Baker.liquidity_baking_votefile Off in
    run_vote_file ~votefile ~liquidity_baking_toggle_vote:On baker
    >|= check_vote_success ~__LOC__ Off
  in

  Log.info "Test [on] vote file" ;
  let* baker =
    let votefile = Baker.liquidity_baking_votefile On in
    run_vote_file ~votefile ~liquidity_baking_toggle_vote:Off baker
    >|= check_vote_success ~__LOC__ On
  in

  Log.info "Test [pass] vote file" ;
  let* baker =
    let votefile = Baker.liquidity_baking_votefile Pass in
    run_vote_file ~votefile ~liquidity_baking_toggle_vote:On baker
    >|= check_vote_success ~__LOC__ Pass
  in

  Log.info "Test non-existant vote file" ;
  let* baker =
    let votefile = "nonexistant.json" in
    if Sys.file_exists votefile then
      Test.fail ~__LOC__ "Did not expect the file %s to exist" votefile ;
    Lwt.catch
      (fun () ->
        let* _ =
          run_vote_file baker ~votefile ~liquidity_baking_toggle_vote:Pass
        in
        Test.fail ~__LOC__ "Baker should not have started")
      (fun _exn ->
        Log.info "As expected, baker did not start" ;
        return baker)
  in

  Log.info "Test invalid json in vote file" ;
  let* baker =
    let votefile = Temp.file "invalid-vote-file.json" in
    Base.write_file votefile ~contents:{|{"liquidity_baking_toggle_vote": true|} ;
    Lwt.catch
      (fun () ->
        let* _ =
          run_vote_file baker ~votefile ~liquidity_baking_toggle_vote:Pass
        in
        Test.fail ~__LOC__ "Baker should not have started")
      (fun _exn ->
        Log.info "As expected, baker did not start" ;
        return baker)
  in

  Log.info "Test vote file at default file location " ;
  let* baker =
    ensure_removal default_votefile @@ fun () ->
    let _ = Baker.liquidity_baking_votefile ~path:default_votefile On in
    run_vote_file baker >|= check_vote_success ~__LOC__ On
  in

  Log.info "Test caching of the votefile setting" ;
  let* _baker =
    ensure_removal default_votefile @@ fun () ->
    let _ = Baker.liquidity_baking_votefile ~path:default_votefile On in
    let* baker = run_vote_file baker >|= check_vote_success ~__LOC__ On in
    (* Explicitly remove the votefile to check that the baker has retained its value *)
    let p_error =
      baker_wait_for_per_block_vote_file_error
        ~expected_id:
          (error_prefix ^ "per_block_vote_file.block_vote_file_not_found")
        ~expected_file_path:default_votefile
        baker
    in
    Sys.remove default_votefile ;
    (* Wait to ensure the removal was detected by the baker *)
    let* _ = p_error in
    (* Now wait for the next vote event *)
    let* vote = baker_wait_for_vote baker in
    check_vote ~__LOC__ vote On ;
    return baker
  in

  Log.info "Test [--votefile] overrides default file location " ;
  let* baker =
    ensure_removal default_votefile @@ fun () ->
    let _ = Baker.liquidity_baking_votefile ~path:default_votefile On in
    let votefile = Baker.liquidity_baking_votefile Off in
    run_vote_file ~votefile baker >|= check_vote_success ~__LOC__ Off
  in

  (* Tests that using booleans (which was possible until Ithaca) is
     not possible anymore, and other invalid contents. *)
  let* _ =
    Lwt_list.fold_left_s
      (fun baker contents ->
        Log.info "Test invalid vote file contents: %s" contents ;
        let votefile = Temp.file "invalid-vote-file.json" in
        Base.write_file votefile ~contents ;
        Lwt.catch
          (fun () ->
            let* _ =
              run_vote_file baker ~votefile ~liquidity_baking_toggle_vote:Pass
            in
            Test.fail ~__LOC__ "Baker should not have started")
          (fun _exn ->
            Log.info "As expected, baker did not start" ;
            return baker))
      baker
      [
        {|{"liquidity_baking_toggle_vote": true}|};
        {|{"liquidity_baking_toggle_vote": false}|};
        {|{"liquidity_baking_toggle": true}|};
      ]
  in

  unit

let register ~protocols = test_all_per_block_votes protocols
