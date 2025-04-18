(** These tests for issue #1658, which follows PR #1655 which introduces a store
    version bump from V1 to V2 when a writer instance adds a V2-only pack entry
    (including nodes and commits, but not contents). *)

open! Import
open Common

(** {2 Preamble} *)

let src = Logs.Src.create "tests.version_bump" ~doc:"Test pack version bump"

module Log = (val Logs.src_log src : Logs.LOG)

(** Set up modules to allow access to "version_1" store *)
module Private = struct
  (* The behaviour under test is independent of which [Conf] we pick: *)
  module Conf = Brassaia_tezos.Conf

  (* Note: the existing stores use a different hash from [Brassaia_tezos.Schema]
     (SHA1 rather than BLAKE2b) *)
  module Schema = Common.Schema

  (* from test_existing_stores.ml; the V2 is because
     test_existing_stores defines two different configs *)
  module V2_maker = Brassaia_pack_unix.Maker (Conf)
  module V2 = V2_maker.Make (Schema)

  (* the following modules are necessary to expose the File_manager.*)
  module Index = Brassaia_pack_unix.Index.Make (Schema.Hash)
  module Io = Brassaia_pack_unix.Io.Unix
  module Io_errors = Brassaia_pack_unix.Io_errors
  module File_manager = Brassaia_pack_unix.File_manager.Make (Index)
end

module Util = struct
  (** Following are generic utils *)

  let exec_cmd = Common.exec_cmd

  let ( / ) = Filename.concat

  let tmp_dir () = Filename.temp_file "test_pack_version_bump_" ""

  (** Copy src to dst; dst is assumed to not exist *)
  let copy_dir src dst =
    assert (not (Sys.file_exists dst)) ;
    (* don't check if it is empty; perhaps we should *)
    Filename.quote_command "cp" ["-R"; src; dst] |> fun cmd ->
    exec_cmd cmd |> function
    | Ok () -> ()
    | Error n ->
        Fmt.failwith
          "Failed to set up test env; command `%s' exited with non-zero code %d\n"
          cmd
          n

  (** Identify the root directory, by comparing st_dev,st_ino *)
  let is_root =
    let open Unix in
    let root_stat = stat "/" in
    fun s ->
      let stat = Unix.stat s in
      (stat.st_dev, stat.st_ino) = (root_stat.st_dev, root_stat.st_ino)

  (** Starting from ".", try to find a parent directory that has a given
      property. *)
  let find_parent_matching test =
    let rec go path =
      match test path with
      | true -> Ok path
      | false -> (
          match is_root path with
          | true -> Error ()
          | false -> go (path / Filename.parent_dir_name))
    in
    go Filename.current_dir_name

  (** More specific utils from here *)

  let v1_store_archive_dir =
    "brassaia-eio" / "test" / "brassaia-pack" / "data" / "version_1"

  (** Find the project root, that contains the v1_store_archive_dir *)
  let project_root () =
    find_parent_matching (fun d -> Sys.file_exists (d / v1_store_archive_dir))
    |> function
    | Ok s -> s
    | Error () ->
        Fmt.failwith
          "Couldn't find project root containing path to %s, after examining \
           current directory %s and ancestors"
          v1_store_archive_dir
          (Sys.getcwd ())

  module Unix_ = Brassaia_pack_unix.Io.Unix

  (** Get the version of the underlying file; file is assumed to exist; file is
      assumed to be an Brassaia_pack.IO.Unix file *)
  let io_get_version ~root : [`V1 | `V2 | `V3 | `V4 | `V5] =
    File_manager.version ~root |> Io_errors.raise_if_error

  let alco_check_version ~pos:_ ~expected ~actual =
    Alcotest.check_repr Brassaia_pack.Version.t "" expected actual
end

open Util

(** This sets up infrastructure to open the existing "version_1" store *)
module With_existing_store () = struct
  let tmp_dir = tmp_dir ()

  let () = [%log.info "Using temporary directory %s" tmp_dir]

  (* Make a copy of the v1_store_archive_dir in tmp_dir *)
  let () =
    rm_dir tmp_dir ;
    copy_dir (project_root () / v1_store_archive_dir) tmp_dir ;
    ()

  (* [S] is the functionality we use from Private, together with an
     appropriate config *)
  module S = Private.V2

  (* Code copied and modified from test_existing_stores.ml; this is
     the config for index and pack *)
  let config ~readonly : Brassaia.config =
    Brassaia_pack.config ~readonly ~index_log_size:1000 ~fresh:false tmp_dir
end

(** {2 The tests} *)

(** Cannot open a V1 store in RO mode. *)
let test_RO_no_migration () : unit =
  [%log.info "Executing test_RO_no_migration"] ;
  let open With_existing_store () in
  assert (io_get_version ~root:tmp_dir = `V1) ;

  let () =
    Alcotest.check_raises
      "open V1 store in RO"
      (Brassaia_pack_unix.Errors.Pack_error `Migration_needed)
      (fun () ->
        let repo = S.Repo.init (config ~readonly:true) in
        S.Repo.close repo)
  in
  (* maybe the version bump is only visible after, check again *)
  alco_check_version
    ~pos:__POS__
    ~expected:`V1
    ~actual:(io_get_version ~root:tmp_dir)

(** Open a V1 store RW mode. Even if no writes, the store migrates to V3. *)
let test_open_RW () =
  [%log.info "Executing test_open_RW"] ;
  let open With_existing_store () in
  assert (io_get_version ~root:tmp_dir = `V1) ;
  let repo = S.Repo.init (config ~readonly:false) in
  let () = S.Repo.close repo in
  alco_check_version
    ~pos:__POS__
    ~expected:`V3
    ~actual:(io_get_version ~root:tmp_dir)

let tests =
  let f g () = g () in
  Alcotest.
    [
      test_case_eio "test_RO_no_migration" `Quick (f test_RO_no_migration);
      test_case_eio "test_open_RW" `Quick (f test_open_RW);
    ]
