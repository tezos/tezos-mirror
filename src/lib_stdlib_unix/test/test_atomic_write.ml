(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2026 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

(* Testing
   -------
   Component:    Lwt_utils_unix atomic writes
   Invocation:   dune exec src/lib_stdlib_unix/test/main.exe -- \
                   --file test_atomic_write.ml
   Subject:      [with_atomic_open_out] leaves nothing behind, and
                 [with_open_file] closes its descriptor, when the task it runs
                 fails or is cancelled.
*)

open Error_monad

let leftovers dir =
  Sys.readdir dir |> Array.to_list
  |> List.filter (fun name -> Filename.check_suffix name ".tmp")

let check_no_leftover dir =
  Check.(
    (leftovers dir = [])
      (list string)
      ~error_msg:"temporary files left behind: %L"
      ~__LOC__)

(* The descriptors of this process, when the system exposes them. Returns
   [None] elsewhere, where the leak cannot be observed this way. *)
let open_descriptors () =
  try Some (Array.length (Sys.readdir "/proc/self/fd"))
  with Sys_error _ -> None

let check_no_leaked_descriptor ~before =
  match (before, open_descriptors ()) with
  | Some before, Some after ->
      Check.(
        (after <= before)
          int
          ~error_msg:"descriptors went from %R to %L, one was leaked"
          ~__LOC__)
  | _, _ -> Log.warn "/proc/self/fd is not readable: descriptor check skipped"

let write_string content fd = Lwt_utils_unix.write_string fd content

(* [with_atomic_open_out] reports its own failures as [Error _] and lets the
   exceptions of the task through, so both shapes are accepted here: the point
   of these tests is what is left on the disk afterwards. *)
let expect_no_success name promise =
  let open Lwt_syntax in
  (* The verdict is reached outside the [Lwt.catch]: [Test.fail] raises, so
     failing from inside it would be caught by its own handler and the test
     would pass no matter what happened. *)
  let* outcome =
    Lwt.catch
      (fun () ->
        let* result = promise () in
        Lwt.return_some result)
      (fun _exn -> Lwt.return_none)
  in
  match outcome with
  | None | Some (Error _) -> return_unit
  | Some (Ok ()) ->
      Test.fail "%s: expected a failure, got a successful write" name

let () =
  Tezt_core.Test.register
    ~__FILE__
    ~title:"with_atomic_open_out writes the file it is given"
    ~tags:["lwt_utils_unix"; "atomic"]
  @@ fun () ->
  let open Lwt_syntax in
  Lwt_utils_unix.with_tempdir "tezos_atomic_write" @@ fun dir ->
  let path = Filename.concat dir "written" in
  let* result =
    Lwt_utils_unix.with_atomic_open_out path (write_string "contents")
  in
  (match result with Ok () -> () | Error _ -> Test.fail "the write failed") ;
  let* written = Lwt_utils_unix.read_file path in
  Check.((written = "contents") string ~error_msg:"read %L, wrote %R" ~__LOC__) ;
  check_no_leftover dir ;
  unit

let () =
  Tezt_core.Test.register
    ~__FILE__
    ~title:"with_atomic_open_out removes its temporary file when the task fails"
    ~tags:["lwt_utils_unix"; "atomic"]
  @@ fun () ->
  let open Lwt_syntax in
  Lwt_utils_unix.with_tempdir "tezos_atomic_write" @@ fun dir ->
  let path = Filename.concat dir "never_written" in
  let before = open_descriptors () in
  let* () =
    expect_no_success "failing task" @@ fun () ->
    Lwt_utils_unix.with_atomic_open_out path (fun _fd ->
        Lwt.fail (Failure "the task failed"))
  in
  check_no_leftover dir ;
  Check.(
    (Sys.file_exists path = false)
      bool
      ~error_msg:"the target was created despite the failure"
      ~__LOC__) ;
  check_no_leaked_descriptor ~before ;
  unit

let () =
  Tezt_core.Test.register
    ~__FILE__
    ~title:
      "with_atomic_open_out removes its temporary file when the task is \
       cancelled"
    ~tags:["lwt_utils_unix"; "atomic"]
  @@ fun () ->
  let open Lwt_syntax in
  Lwt_utils_unix.with_tempdir "tezos_atomic_write" @@ fun dir ->
  let path = Filename.concat dir "never_written" in
  let before = open_descriptors () in
  (* The task reports that it has started, so that the cancellation below is
     delivered while the descriptor is open rather than while the file is still
     being opened. [wakeup_later] and the pause matter: waking the caller with
     [Lwt.wakeup] runs its continuation immediately, which would cancel the
     promise from inside the task itself, before there is anything to
     cancel. *)
  let started, task_has_started = Lwt.wait () in
  let promise =
    Lwt_utils_unix.with_atomic_open_out path (fun _fd ->
        Lwt.wakeup_later task_has_started () ;
        fst (Lwt.task ()))
  in
  let* () = started in
  let* () = Lwt.pause () in
  Lwt.cancel promise ;
  let* () = expect_no_success "cancelled task" (fun () -> promise) in
  check_no_leftover dir ;
  Check.(
    (Sys.file_exists path = false)
      bool
      ~error_msg:"the target was created despite the cancellation"
      ~__LOC__) ;
  check_no_leaked_descriptor ~before ;
  unit

let () =
  Tezt_core.Test.register
    ~__FILE__
    ~title:"with_atomic_open_out does not leak a descriptor per failure"
    ~tags:["lwt_utils_unix"; "atomic"]
  @@ fun () ->
  let open Lwt_syntax in
  Lwt_utils_unix.with_tempdir "tezos_atomic_write" @@ fun dir ->
  let path = Filename.concat dir "never_written" in
  (* One write first, so that whatever the runtime opens on its own the first
     time round is not counted as a leak. *)
  let* () =
    expect_no_success "failing task" @@ fun () ->
    Lwt_utils_unix.with_atomic_open_out path (fun _fd -> Lwt.fail Not_found)
  in
  let before = open_descriptors () in
  let* () =
    List.iter_s
      (fun _ ->
        expect_no_success "failing task" @@ fun () ->
        Lwt_utils_unix.with_atomic_open_out path (fun _fd -> Lwt.fail Not_found))
      (Stdlib.List.init 50 Fun.id)
  in
  check_no_leaked_descriptor ~before ;
  check_no_leftover dir ;
  unit

let () =
  Tezt_core.Test.register
    ~__FILE__
    ~title:
      "with_atomic_open_out removes its temporary file when the rename fails"
    ~tags:["lwt_utils_unix"; "atomic"]
  @@ fun () ->
  let open Lwt_syntax in
  Lwt_utils_unix.with_tempdir "tezos_atomic_write" @@ fun dir ->
  (* Renaming a file onto a directory fails, which is the one way to reach the
     [`Rename] branch without breaking the filesystem underneath the test. *)
  let path = Filename.concat dir "a_directory" in
  Unix.mkdir path 0o755 ;
  let before = open_descriptors () in
  let* () =
    expect_no_success "rename onto a directory" @@ fun () ->
    Lwt_utils_unix.with_atomic_open_out path (write_string "contents")
  in
  check_no_leftover dir ;
  check_no_leaked_descriptor ~before ;
  unit
