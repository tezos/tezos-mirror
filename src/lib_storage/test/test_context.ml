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

open Context

let ( >>= ) = Lwt.bind

(* Same as [>>=], but handle errors using [Assert.fail_msg]. *)
let ( >>=! ) x f =
  x
  >>= fun result ->
  match result with
  | Error trace ->
      let message = Format.asprintf "%a" Error_monad.pp_print_error trace in
      Assert.fail_msg "%s" message
  | Ok x ->
      f x

let ( >|= ) = Lwt.( >|= )

let ( // ) = Filename.concat

(** Basic blocks *)

let genesis_block =
  Block_hash.of_b58check_exn
    "BLockGenesisGenesisGenesisGenesisGenesisGeneskvg68z"

let genesis_protocol =
  Protocol_hash.of_b58check_exn
    "ProtoDemoNoopsDemoNoopsDemoNoopsDemoNoopsDemo6XBoYp"

let genesis_time = Time.Protocol.of_seconds 0L

let chain_id = Chain_id.of_block_hash genesis_block

(** Context creation *)

let commit = commit ~time:Time.Protocol.epoch ~message:""

let block2 =
  Block_hash.of_hex_exn
    (`Hex "2222222222222222222222222222222222222222222222222222222222222222")

let create_block2 idx genesis_commit =
  checkout idx genesis_commit
  >>= function
  | None ->
      Assert.fail_msg "checkout genesis_block"
  | Some ctxt ->
      set ctxt ["a"; "b"] (Bytes.of_string "Novembre")
      >>= fun ctxt ->
      set ctxt ["a"; "c"] (Bytes.of_string "Juin")
      >>= fun ctxt ->
      set ctxt ["version"] (Bytes.of_string "0.0") >>= fun ctxt -> commit ctxt

let block3a =
  Block_hash.of_hex_exn
    (`Hex "3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a")

let create_block3a idx block2_commit =
  checkout idx block2_commit
  >>= function
  | None ->
      Assert.fail_msg "checkout block2"
  | Some ctxt ->
      del ctxt ["a"; "b"]
      >>= fun ctxt ->
      set ctxt ["a"; "d"] (Bytes.of_string "Mars") >>= fun ctxt -> commit ctxt

let block3b =
  Block_hash.of_hex_exn
    (`Hex "3b3b3b3b3b3b3b3b3b3b3b3b3b3b3b3b3b3b3b3b3b3b3b3b3b3b3b3b3b3b3b3b")

let block3c =
  Block_hash.of_hex_exn
    (`Hex "3c3c3c3c3c3c3c3c3c3c3c3c3c3c3c3c3c3c3c3c3c3c3c3c3c3c3c3c3c3c3c3c")

let create_block3b idx block2_commit =
  checkout idx block2_commit
  >>= function
  | None ->
      Assert.fail_msg "checkout block3b"
  | Some ctxt ->
      del ctxt ["a"; "c"]
      >>= fun ctxt ->
      set ctxt ["a"; "d"] (Bytes.of_string "Février")
      >>= fun ctxt -> commit ctxt

type t = {
  idx : Context.index;
  genesis : Context_hash.t;
  block2 : Context_hash.t;
  block3a : Context_hash.t;
  block3b : Context_hash.t;
}

let wrap_context_init f _ () =
  Lwt_utils_unix.with_tempdir "tezos_test_" (fun base_dir ->
      let root = base_dir // "context" in
      Context.init ~mapsize:4_096_000L root
      >>= fun idx ->
      Context.commit_genesis
        idx
        ~chain_id
        ~time:genesis_time
        ~protocol:genesis_protocol
      >>=! fun genesis ->
      create_block2 idx genesis
      >>= fun block2 ->
      create_block3a idx block2
      >>= fun block3a ->
      create_block3b idx block2
      >>= fun block3b -> f {idx; genesis; block2; block3a; block3b})

(** Simple test *)

let c = function None -> None | Some s -> Some (Bytes.to_string s)

let test_simple {idx; block2; _} =
  checkout idx block2
  >>= function
  | None ->
      Assert.fail_msg "checkout block2"
  | Some ctxt ->
      get ctxt ["version"]
      >>= fun version ->
      Assert.equal_string_option ~msg:__LOC__ (c version) (Some "0.0") ;
      get ctxt ["a"; "b"]
      >>= fun novembre ->
      Assert.equal_string_option (Some "Novembre") (c novembre) ;
      get ctxt ["a"; "c"]
      >>= fun juin ->
      Assert.equal_string_option ~msg:__LOC__ (Some "Juin") (c juin) ;
      Lwt.return_unit

let test_continuation {idx; block3a; _} =
  checkout idx block3a
  >>= function
  | None ->
      Assert.fail_msg "checkout block3a"
  | Some ctxt ->
      get ctxt ["version"]
      >>= fun version ->
      Assert.equal_string_option ~msg:__LOC__ (Some "0.0") (c version) ;
      get ctxt ["a"; "b"]
      >>= fun novembre ->
      Assert.is_none ~msg:__LOC__ (c novembre) ;
      get ctxt ["a"; "c"]
      >>= fun juin ->
      Assert.equal_string_option ~msg:__LOC__ (Some "Juin") (c juin) ;
      get ctxt ["a"; "d"]
      >>= fun mars ->
      Assert.equal_string_option ~msg:__LOC__ (Some "Mars") (c mars) ;
      Lwt.return_unit

let test_fork {idx; block3b; _} =
  checkout idx block3b
  >>= function
  | None ->
      Assert.fail_msg "checkout block3b"
  | Some ctxt ->
      get ctxt ["version"]
      >>= fun version ->
      Assert.equal_string_option ~msg:__LOC__ (Some "0.0") (c version) ;
      get ctxt ["a"; "b"]
      >>= fun novembre ->
      Assert.equal_string_option ~msg:__LOC__ (Some "Novembre") (c novembre) ;
      get ctxt ["a"; "c"]
      >>= fun juin ->
      Assert.is_none ~msg:__LOC__ (c juin) ;
      get ctxt ["a"; "d"]
      >>= fun mars ->
      Assert.equal_string_option ~msg:__LOC__ (Some "Février") (c mars) ;
      Lwt.return_unit

let test_replay {idx; genesis; _} =
  checkout idx genesis
  >>= function
  | None ->
      Assert.fail_msg "checkout genesis_block"
  | Some ctxt0 ->
      set ctxt0 ["version"] (Bytes.of_string "0.0")
      >>= fun ctxt1 ->
      set ctxt1 ["a"; "b"] (Bytes.of_string "Novembre")
      >>= fun ctxt2 ->
      set ctxt2 ["a"; "c"] (Bytes.of_string "Juin")
      >>= fun ctxt3 ->
      set ctxt3 ["a"; "d"] (Bytes.of_string "July")
      >>= fun ctxt4a ->
      set ctxt3 ["a"; "d"] (Bytes.of_string "Juillet")
      >>= fun ctxt4b ->
      set ctxt4a ["a"; "b"] (Bytes.of_string "November")
      >>= fun ctxt5a ->
      get ctxt4a ["a"; "b"]
      >>= fun novembre ->
      Assert.equal_string_option ~msg:__LOC__ (Some "Novembre") (c novembre) ;
      get ctxt5a ["a"; "b"]
      >>= fun november ->
      Assert.equal_string_option ~msg:__LOC__ (Some "November") (c november) ;
      get ctxt5a ["a"; "d"]
      >>= fun july ->
      Assert.equal_string_option ~msg:__LOC__ (Some "July") (c july) ;
      get ctxt4b ["a"; "b"]
      >>= fun novembre ->
      Assert.equal_string_option ~msg:__LOC__ (Some "Novembre") (c novembre) ;
      get ctxt4b ["a"; "d"]
      >>= fun juillet ->
      Assert.equal_string_option ~msg:__LOC__ (Some "Juillet") (c juillet) ;
      Lwt.return_unit

let fold_keys s k ~init ~f =
  let rec loop k acc =
    fold s k ~init:acc ~f:(fun file acc ->
        match file with `Key k -> f k acc | `Dir k -> loop k acc)
  in
  loop k init

let keys t = fold_keys t ~init:[] ~f:(fun k acc -> Lwt.return (k :: acc))

let test_fold {idx; genesis; _} =
  checkout idx genesis
  >>= function
  | None ->
      Assert.fail_msg "checkout genesis_block"
  | Some ctxt ->
      set ctxt ["a"; "b"] (Bytes.of_string "Novembre")
      >>= fun ctxt ->
      set ctxt ["a"; "c"] (Bytes.of_string "Juin")
      >>= fun ctxt ->
      set ctxt ["a"; "d"; "e"] (Bytes.of_string "Septembre")
      >>= fun ctxt ->
      set ctxt ["f"] (Bytes.of_string "Avril")
      >>= fun ctxt ->
      set ctxt ["g"; "h"] (Bytes.of_string "Avril")
      >>= fun ctxt ->
      keys ctxt []
      >>= fun l ->
      Assert.equal_string_list_list
        ~msg:__LOC__
        [["a"; "b"]; ["a"; "c"]; ["a"; "d"; "e"]; ["f"]; ["g"; "h"]]
        (List.sort compare l) ;
      keys ctxt ["a"]
      >>= fun l ->
      Assert.equal_string_list_list
        ~msg:__LOC__
        [["a"; "b"]; ["a"; "c"]; ["a"; "d"; "e"]]
        (List.sort compare l) ;
      keys ctxt ["f"]
      >>= fun l ->
      Assert.equal_string_list_list ~msg:__LOC__ [] l ;
      keys ctxt ["g"]
      >>= fun l ->
      Assert.equal_string_list_list ~msg:__LOC__ [["g"; "h"]] l ;
      keys ctxt ["i"]
      >>= fun l ->
      Assert.equal_string_list_list ~msg:__LOC__ [] l ;
      Lwt.return_unit

let test_dump {idx; block3b; _} =
  Lwt_utils_unix.with_tempdir "tezos_test_" (fun base_dir2 ->
      let dumpfile = base_dir2 // "dump" in
      let ctxt_hash = block3b in
      let history_mode = Tezos_shell_services.History_mode.Full in
      let empty_block_header context =
        Block_header.
          {
            protocol_data = Bytes.empty;
            shell =
              {
                level = 0l;
                proto_level = 0;
                predecessor = Block_hash.zero;
                timestamp = Time.Protocol.epoch;
                validation_passes = 0;
                operations_hash = Operation_list_list_hash.zero;
                fitness = [];
                context;
              };
          }
      in
      let _empty_pruned_block =
        ( {
            block_header = empty_block_header Context_hash.zero;
            operations = [];
            operation_hashes = [];
          }
          : Context.Pruned_block.t )
      in
      let empty =
        {
          Context.Block_data.block_header = empty_block_header Context_hash.zero;
          operations = [[]];
        }
      in
      let bhs =
        (fun context ->
          ( empty_block_header context,
            empty,
            history_mode,
            fun _ -> return (None, None) ))
          ctxt_hash
      in
      Context.dump_contexts idx bhs ~filename:dumpfile
      >>=? fun () ->
      let root = base_dir2 // "context" in
      Context.init ?patch_context:None root
      >>= fun idx2 ->
      Context.restore_contexts
        idx2
        ~filename:dumpfile
        (fun _ -> return_unit)
        (fun _ _ _ -> return_unit)
      >>=? fun imported ->
      let (bh, _, _, _, _, _) = imported in
      let expected_ctxt_hash = bh.Block_header.shell.context in
      assert (Context_hash.equal ctxt_hash expected_ctxt_hash) ;
      return ())
  >>=! Lwt.return

(******************************************************************************)

let tests : (string * (t -> unit Lwt.t)) list =
  [ ("simple", test_simple);
    ("continuation", test_continuation);
    ("fork", test_fork);
    ("replay", test_replay);
    ("fold", test_fold);
    ("dump", test_dump) ]

let tests =
  List.map
    (fun (s, f) -> Alcotest_lwt.test_case s `Quick (wrap_context_init f))
    tests
