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
   Component:    Client commands
   Invocation:   dune exec tezt/tests/main.exe -- --file timelock.ml
   Subject:      Tests checking coin toss timelock game contract
*)

(* --- Format/IO helper functions --- *)
let bytes_to_string b = "0x" ^ Hex.(of_bytes b |> show)

let head_hex = Bytes.of_string "head" |> bytes_to_string

let tail_hex = Bytes.of_string "tail" |> bytes_to_string

let chest_to_string chest =
  Data_encoding.Binary.to_bytes_exn Tezos_crypto.Timelock.chest_encoding chest
  |> bytes_to_string

let chest_key_to_string chest_key =
  Data_encoding.Binary.to_bytes_exn
    Tezos_crypto.Timelock.chest_key_encoding
    chest_key
  |> bytes_to_string

let read_encoding path enc =
  let ic = open_in path in
  let size = In_channel.length ic |> Int64.to_int in
  let buffer = Bytes.create size in
  let _ = In_channel.really_input ic buffer 0 size in
  let () = close_in ic in
  Data_encoding.Binary.of_bytes_exn enc buffer |> return

(* --- Hardcoded values --- *)

let path = "/tmp"

let time = "1024"

let dummy_chest =
  let rng_state = Random.get_state () in
  let chest, _ck =
    Tezos_crypto.Timelock.chest_sampler
      ~rng_state
      ~plaintext_size:100
      ~time:(int_of_string time)
  in
  chest_to_string chest

(* --- Contract helper functions --- *)
let originate_contract ?(mockup = true) protocol contract =
  let* client =
    if mockup then Client.init_mockup ~protocol ()
    else
      let* _, c = Client.init_with_protocol `Client ~protocol () in
      return c
  in
  Log.info "Activated protocol." ;
  let* contract_address =
    let prg =
      sf
        "./src/%s/lib_protocol/contracts/%s"
        (Protocol.directory protocol)
        contract
    in
    let init =
      "(Pair 0 (Pair " ^ dummy_chest ^ " (Pair 0xdeadbeef 0xdeadbeef)))"
    in
    Client.originate_contract
      ~wait:"none"
      ~init
      ~alias:contract
      ~amount:Tez.zero
      ~burn_cap:(Tez.of_int 2)
      ~src:Constant.bootstrap1.alias
      ~prg
      client
  in
  let* () =
    if mockup then Lwt.return_unit else Client.bake_for_and_wait client
  in
  Log.info "  - Originated %s." contract ;
  return (client, contract_address)

let get_contract_storage client address =
  let* str = Client.contract_storage address client in
  let data =
    List.map String.trim (String.split_on_char '\n' str) |> Array.of_list
  in
  let level =
    String.sub data.(0) 5 (String.length data.(0) - 5) |> int_of_string
  in
  let chest = data.(1) in
  let guess = data.(2) in
  let msg = data.(3) in
  return (level, chest, guess, msg)

(* --- Client helper functions --- *)
let gen_precompute client time filepath =
  let* _client_output =
    Client.spawn_command
      client
      ["timelock"; "precompute"; "for"; time; "in"; filepath]
    |> Process.check_and_read_stdout
  in
  return None

let gen_chest client time payload filepath =
  let* _client_output =
    Client.spawn_command
      client
      ["timelock"; "create"; "for"; time; "with"; payload; "in"; filepath]
    |> Process.check_and_read_stdout
  in
  return None

let open_chest client time chestpath path =
  let* _client_output =
    Client.spawn_command
      client
      ["timelock"; "open"; "for"; time; "chest"; chestpath; "in"; path]
    |> Process.check_and_read_stdout
  in
  return None

let verify_chest client time chestpath chestkeypath =
  let* _client_output =
    Client.spawn_command
      client
      [
        "timelock";
        "verify";
        "for";
        time;
        "chest";
        chestpath;
        "chest_key";
        chestkeypath;
      ]
    |> Process.check_and_read_stdout
  in
  return None

(* Test helpers *)
let chest_prefix = "time_chest_"

let get_chest_suffix chest_file =
  let l1 = String.length chest_prefix in
  let l2 = String.length chest_file in
  String.sub chest_file l1 (l2 - l1)

let creator_key_prefix = "time_key_create_"

let opener_key_prefix = "time_key_open_"

let create_timelock client path time payload =
  let add_path r n = r ^ "/" ^ n in
  let* _ = gen_precompute client time path in
  let* _ = gen_chest client time payload path in
  let chest_files_before =
    Sys.readdir path |> Array.to_list
    |> List.filter (fun x -> String.starts_with ~prefix:chest_prefix x)
  in
  let* _ = gen_precompute client time path in
  let* _ = gen_chest client time payload path in
  let chest_files_after =
    Sys.readdir path |> Array.to_list
    |> List.filter (fun x -> String.starts_with ~prefix:chest_prefix x)
  in
  let chest_file =
    List.fold_left
      (fun acc file -> if List.mem file chest_files_before then acc else file)
      ""
      chest_files_after
  in
  let creator_key_file = creator_key_prefix ^ get_chest_suffix chest_file in
  let* chest =
    read_encoding
      (add_path path chest_file)
      Tezos_crypto.Timelock.chest_encoding
  in
  let* creator_key =
    read_encoding
      (add_path path creator_key_file)
      Tezos_crypto.Timelock.chest_key_encoding
  in
  let* _ =
    verify_chest
      client
      time
      (add_path path chest_file)
      (add_path path creator_key_file)
  in
  return (chest_file, chest, creator_key_file, creator_key)

let open_timelock ?(verify = true) client path chest_file time =
  let add_path r n = r ^ "/" ^ n in
  let* _ = open_chest client time (add_path path chest_file) path in
  let opener_key_file = opener_key_prefix ^ get_chest_suffix chest_file in
  let* _ =
    if verify then
      verify_chest
        client
        time
        (add_path path chest_file)
        (add_path path opener_key_file)
    else Lwt.return_none
  in
  let* opener_key =
    read_encoding
      (add_path path opener_key_file)
      Tezos_crypto.Timelock.chest_key_encoding
  in
  return (opener_key_file, opener_key)

let assert_storage ?(lvl = 1) ?(chest = dummy_chest) ?(guess = "0xa0")
    ?(msg = "0xa0") client address =
  let* l, c, g, m = get_contract_storage client address in
  if
    lvl = l
    && String.(equal c chest)
    && String.(equal g guess)
    && String.(equal m msg)
  then Lwt.return_true
  else Lwt.return_false

let burn_cap = Tez.of_int 2

let amount = Tez.zero

let test_contract_correct_guess ~protocol () =
  let* client, receiver = originate_contract protocol "timelock_flip.tz" in
  (* bootstrap2 starts a coin toss game by submitting a chest *)
  let str = head_hex in
  let* chest_file, chest, _, _ = create_timelock client path time str in
  let chest = chest_to_string chest in
  let giver = Constant.bootstrap2.alias in
  let arg = "Left " ^ chest in
  let* () = Client.transfer ~burn_cap ~amount ~giver ~receiver ~arg client in
  let* b_init = assert_storage ~chest client receiver in
  (* bootstrap3 submits their guess *)
  let giver = Constant.bootstrap3.alias in
  let guess = head_hex in
  let arg = "Right (Left " ^ guess ^ ")" in
  let* () = Client.transfer ~burn_cap ~amount ~giver ~receiver ~arg client in
  let* b_guess = assert_storage ~chest ~guess ~msg:"0xb0" client receiver in
  (* bootstrap3 opens the chest to finish the game *)
  let* _chest_key_file, chest_key = open_timelock client path chest_file time in
  let giver = Constant.bootstrap3.alias in
  let arg = "Right (Right " ^ chest_key_to_string chest_key ^ ")" in
  let* () = Client.transfer ~burn_cap ~amount ~giver ~receiver ~arg client in
  let* b_result = assert_storage ~chest ~guess ~msg:"0x00" client receiver in
  assert (b_init && b_guess && b_result) ;
  unit

let test_contract_incorrect_guess ~protocol () =
  let* client, receiver = originate_contract protocol "timelock_flip.tz" in
  (* bootstrap2 starts a coin toss game by submitting a chest *)
  let str = head_hex in
  let* chest_file, chest, _, _ = create_timelock client path time str in
  let chest = chest_to_string chest in
  let giver = Constant.bootstrap2.alias in
  let arg = "Left " ^ chest in
  let* () = Client.transfer ~burn_cap ~amount ~giver ~receiver ~arg client in
  let* b_init = assert_storage ~chest client receiver in
  (* bootstrap3 submits their guess *)
  let giver = Constant.bootstrap3.alias in
  let guess = tail_hex in
  let arg = "Right (Left " ^ guess ^ ")" in
  let* () = Client.transfer ~burn_cap ~amount ~giver ~receiver ~arg client in
  let* b_guess = assert_storage ~chest ~guess ~msg:"0xb0" client receiver in
  (* bootstrap3 opens the chest to finish the game *)
  let* _chest_key_file, chest_key = open_timelock client path chest_file time in
  let giver = Constant.bootstrap3.alias in
  let arg = "Right (Right " ^ chest_key_to_string chest_key ^ ")" in
  let* () = Client.transfer ~burn_cap ~amount ~giver ~receiver ~arg client in
  let* b_result = assert_storage ~chest ~guess ~msg:"0x01" client receiver in
  assert (b_init && b_guess && b_result) ;
  unit

let test_contract_guess_too_late ~protocol () =
  let open Tezos_crypto.Timelock in
  let* client, receiver =
    originate_contract ~mockup:false protocol "timelock_flip.tz"
  in
  (* bootstrap2 starts a coin toss game by submitting a chest *)
  let str = head_hex in
  let* _chest_file, chest, _, _ = create_timelock client path time str in
  let chest_str = chest_to_string chest in
  let giver = Constant.bootstrap2.alias in
  let arg = "Left " ^ chest_str in
  let* () = Client.transfer ~burn_cap ~amount ~giver ~receiver ~arg client in
  let* () = Client.bake_for_and_wait client in
  let* b_init = assert_storage ~lvl:3 ~chest:chest_str client receiver in
  (* bootstrap3 submits their guess too late as they cheat, trying to open the timelock quickly enough*)
  let giver = Constant.bootstrap3.alias in
  let key = create_chest_key chest ~time:(int_of_string time) in
  let plaintext =
    match open_chest chest key ~time:(int_of_string time) with
    | Correct plain -> plain
    | _ -> assert false
  in
  let* () =
    Lwt_list.fold_left_s
      (fun _ _ -> Client.bake_for_and_wait client)
      ()
      (List.init 9 Fun.const)
  in
  let guess = bytes_to_string plaintext in
  let arg = "Right (Left " ^ guess ^ ")" in
  let* () = Client.transfer ~burn_cap ~amount ~giver ~receiver ~arg client in
  let* b_guess = assert_storage ~lvl:3 ~chest:chest_str client receiver in
  assert (b_init && b_guess) ;
  unit

let test_contract_error_opening ~protocol () =
  let* client, receiver = originate_contract protocol "timelock_flip.tz" in
  (* bootstrap2 starts a coin toss game by submitting a chest *)
  let str = head_hex in
  let* chest_file, chest, _, _ = create_timelock client path time str in
  let chest = chest_to_string chest in
  let giver = Constant.bootstrap2.alias in
  let arg = "Left " ^ chest in
  let* () = Client.transfer ~burn_cap ~amount ~giver ~receiver ~arg client in
  let* b_init = assert_storage ~chest client receiver in
  (* bootstrap3 submits their guess *)
  let giver = Constant.bootstrap3.alias in
  let guess = Bytes.of_string "tail" |> bytes_to_string in
  let arg = "Right (Left " ^ guess ^ ")" in
  let* () = Client.transfer ~burn_cap ~amount ~giver ~receiver ~arg client in
  let* b_guess = assert_storage ~chest ~guess ~msg:"0xb0" client receiver in
  (* bootstrap3 opens the chest wrongly to finish the game *)
  let* _chest_key_file, chest_key =
    open_timelock ~verify:false client path chest_file "10"
  in
  let giver = Constant.bootstrap3.alias in
  let arg = "Right (Right " ^ chest_key_to_string chest_key ^ ")" in
  let* () = Client.transfer ~burn_cap ~amount ~giver ~receiver ~arg client in
  let* b_result = assert_storage ~chest ~guess ~msg:"0x10" client receiver in
  assert (b_init && b_guess && b_result) ;
  unit

let register ~protocols =
  List.iter
    (fun (title, test_function, uses_node) ->
      Protocol.register_test
        ~supports:Protocol.(From_protocol (number Nairobi + 1))
        ~__FILE__
        ~title
        ~tags:["client"; "michelson"; "timelock"]
        ~uses_node
        (fun protocol -> test_function ~protocol ())
        protocols)
    [
      ("Correct guess test on timelock", test_contract_correct_guess, false);
      ("Incorrect guess test on timelock", test_contract_incorrect_guess, false);
      ("Guess too late test on timelock", test_contract_guess_too_late, true);
      ("Error opening test on timelock", test_contract_error_opening, false);
    ]
