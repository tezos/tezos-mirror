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

let make_prefix_dir () = Temp.dir "michelson_self_test_scripts"

let script0 name = sf "%s.tz" name

let script1 name start = sf "%s_%03d.tz" name (Protocol.number start)

let script2 name start end_ =
  sf "%s_%03d_%03d.tz" name (Protocol.number start) (Protocol.number end_)

let with_previous_protocol p f =
  match Protocol.previous_protocol p with
  | None ->
      Log.warn
        "@[Test@ is@ desactivated@ because@ there@ is@ no@ declared@ \
         predecessor@ for@ %s.@]"
        (Protocol.name p) ;
      unit
  | Some p' -> f p'

let require_previous_protocol p =
  match Protocol.previous_protocol p with
  | Some p' -> p'
  | None ->
      Test.fail
        "Could not find predecessor of %s protocol necessary for test"
        (Protocol.name p)

let touch_p =
  let mkdir_p dir = Process.spawn "mkdir" ["-p"; dir] |> Process.check in
  fun path ->
    let* () = mkdir_p (Filename.dirname path) in
    Process.spawn "touch" [path] |> Process.check

let check_find_all ~prefix ?(filter = Fun.id) ~protocol ~expected ~descr
    ~__LOC__ () =
  let actual =
    Michelson_script.find_all ~prefix protocol
    |> filter
    |> List.map (Michelson_script.path ~no_prefix:true)
  in
  Log.info "%s: %s" descr (String.concat "," actual) ;
  Check.(actual = expected)
    Check.(list string)
    ~error_msg:"Expected to find %R, got %L"
    ~__LOC__

let check_find ~prefix ~protocol ~name ~expected ~__LOC__ =
  let actual =
    Michelson_script.find ~prefix name protocol
    |> Michelson_script.path ~no_prefix:true
  in
  Check.(actual = expected)
    Check.string
    ~error_msg:"Expected to find %R, got %L"
    ~__LOC__

let protocol_number_string protocol = Protocol.number protocol |> sf "%03d"

let blacklist dirs scripts =
  scripts
  |> List.filter @@ fun (script : Michelson_script.t) ->
     not
     @@ List.exists (fun dir -> List.equal String.equal dir script.dirname) dirs

let whitelist dirs scripts =
  scripts
  |> List.filter @@ fun (script : Michelson_script.t) ->
     List.exists (fun dir -> List.equal String.equal dir script.dirname) dirs

let test_all () =
  Test.register
    ~__FILE__
    ~title:"michelson_scripts: all"
    ~tags:["michelson_scripts"; "all"]
  @@ fun () ->
  let previous_protocol = require_previous_protocol Alpha in
  let prefix = make_prefix_dir () in
  let a = script0 "a" in
  let a_alpha = script1 "a" Alpha in
  let a_prev = script1 "a" previous_protocol in
  let c = script0 "c" in
  let c_alpha = script1 "c" Alpha in
  let c_prev = script1 "c" previous_protocol in
  let f = script0 "f" in
  let f_alpha = script1 "f" Alpha in
  let k = script0 "k" in
  let* () = touch_p (prefix // a) in
  let* () = touch_p (prefix // a_prev) in
  let* () = touch_p (prefix // a_alpha) in
  let* () = touch_p (prefix // "b" // c) in
  let* () = touch_p (prefix // "b" // c_prev) in
  let* () = touch_p (prefix // "b" // c_alpha) in
  let* () = touch_p (prefix // "d" // k) in
  let* () = touch_p (prefix // "d" // "e" // f) in
  let* () = touch_p (prefix // "d" // "e" // f_alpha) in
  let () =
    (* get all of the scripts in the foo directory for previous_protocol *)
    let expected = [a_prev; "b" // c_prev; "d" // "e" // f; "d" // k] in
    check_find_all
      ~prefix
      ~protocol:previous_protocol
      ~descr:"All previous protocol (no filter)"
      ~expected
      ~__LOC__
      () ;
    check_find_all
      ~prefix
      ~filter:(blacklist [])
      ~protocol:previous_protocol
      ~descr:"All previous protocol (blacklist [])"
      ~expected
      ~__LOC__
      ()
  in
  check_find_all
    ~prefix
    ~filter:(blacklist [[]])
    ~protocol:previous_protocol
    ~descr:"All previous protocol (blacklist [[]])"
    ~expected:["b" // c_prev; "d" // "e" // f; "d" // k]
    ~__LOC__
    () ;
  check_find_all
    ~prefix
    ~filter:(whitelist [[]; ["d"; "e"]])
    ~protocol:previous_protocol
    ~descr:"All previous protocol (whitelist [[], [d, e]])"
    ~expected:[a_prev; "d" // "e" // f]
    ~__LOC__
    () ;
  check_find_all
    ~prefix
    ~filter:(whitelist [[]])
    ~protocol:previous_protocol
    ~descr:"All previous protocol (whitelist [[]])"
    ~expected:[a_prev]
    ~__LOC__
    () ;
  check_find_all
    ~prefix
    ~filter:(fun ts ->
      ts
      |> List.filter (fun (t : Michelson_script.t) ->
             match t.dirname with "d" :: _ -> true | _ -> false))
    ~protocol:previous_protocol
    ~descr:"All previous protocol (custom filter: dirname starts with d)"
    ~expected:["d" // "e" // f; "d" // k]
    ~__LOC__
    () ;
  check_find_all
    ~prefix
    ~filter:(fun ts ->
      ts
      |> List.filter (fun (t : Michelson_script.t) ->
             match t.dirname with "d" :: _ -> true | _ -> false))
    ~protocol:Alpha
    ~descr:"All alpha protocol (custom filter: dirname starts with d)"
    ~expected:["d" // "e" // f_alpha; "d" // k]
    ~__LOC__
    () ;
  check_find_all
    ~prefix
    ~filter:(whitelist [["d"; "e"]])
    ~protocol:previous_protocol
    ~descr:"All previous protocol (whitelist [d, e])"
    ~expected:["d" // "e" // f]
    ~__LOC__
    () ;
  unit

let test_find () =
  Test.register
    ~__FILE__
    ~title:"michelson_scripts: find"
    ~tags:["michelson_scripts"; "find"]
  @@ fun () ->
  let previous_protocol = require_previous_protocol Alpha in
  let prefix = make_prefix_dir () in
  let bar = script0 "bar" in
  let bar_alpha = script1 "bar" Alpha in
  let* () = touch_p (prefix // "foo" // bar) in
  let* () = touch_p (prefix // "foo" // bar_alpha) in
  let name = ["foo"; "bar"] in
  check_find
    ~prefix
    ~name
    ~protocol:previous_protocol
    ~expected:("foo" // bar)
    ~__LOC__ ;
  check_find
    ~prefix
    ~name
    ~protocol:Alpha
    ~expected:("foo" // bar_alpha)
    ~__LOC__ ;
  check_find_all
    ~prefix
    ~protocol:Alpha
    ~descr:"All"
    ~expected:["foo" // bar_alpha]
    ~__LOC__
    () ;
  check_find_all
    ~prefix
    ~filter:(whitelist [["foo"]])
    ~protocol:Alpha
    ~descr:"All bar"
    ~expected:["foo" // bar_alpha]
    ~__LOC__
    () ;
  unit

let test_version_range1 () =
  Test.register
    ~__FILE__
    ~title:"michelson_scripts: version_range1"
    ~tags:["michelson_scripts"; "version_range"]
  @@ fun () ->
  let prefix = make_prefix_dir () in
  let prev_proto = require_previous_protocol Alpha in
  with_previous_protocol prev_proto @@ fun prev_prev_proto ->
  (* Assuming:
     alpha ~ 016
     prev_proto ~ 015
     prev_prev_proto ~ 014

     Create the following scripts:
     - foo/bar.tz
     - foo/bar_014_015.tz
     - foo/bar_014_016.tz

     And test that [find_all prev_proto] returns an error.
  *)
  let* () = touch_p (prefix // "foo" // script0 "bar") in
  let* () =
    touch_p (prefix // "foo" // script2 "bar" prev_prev_proto prev_proto)
  in
  let* () = touch_p (prefix // "foo" // script2 "bar" prev_prev_proto Alpha) in
  let () =
    match Michelson_script.find_all_res ~prefix prev_proto with
    | Error (__LOC__, msg) ->
        Log.info "Expected Error, got Error:" ;
        Log.info "%s: %s" __LOC__ msg
    | Ok ts ->
        Test.fail
          ~__LOC__
          "Expected Error, got Ok:\n%s"
          (ts
          |> List.map (fun t ->
                 sf " - %s" (Michelson_script.path ~no_prefix:true t))
          |> String.concat "\n")
  in
  unit

let test_version_range2 () =
  Test.register
    ~__FILE__
    ~title:"michelson_scripts: version_range2"
    ~tags:["michelson_scripts"; "version_range"]
  @@ fun () ->
  let prefix = make_prefix_dir () in
  let prev_proto = require_previous_protocol Alpha in
  with_previous_protocol prev_proto @@ fun prev_prev_proto ->
  (* Assuming:
     Alpha ~ 016
     prev_proto ~ 015
     prev_prev_proto ~ 014

     Create the following scripts:
     - foo/bar.tz
     - foo/bar_014_016.tz
     - foo/bar_015.tz

     And test that [find_all prev_proto] returns an error.
  *)
  let* () = touch_p (prefix // "foo" // script0 "bar") in
  let* () = touch_p (prefix // "foo" // script2 "bar" prev_prev_proto Alpha) in
  let* () = touch_p (prefix // "foo" // script1 "bar" prev_proto) in
  let () =
    match Michelson_script.find_all_res ~prefix prev_proto with
    | Error (__LOC__, msg) ->
        Log.info "Expected Error, got Error:" ;
        Log.info "%s: %s" __LOC__ msg
    | Ok ts ->
        Test.fail
          ~__LOC__
          "Expected Error, got Ok:\n%s"
          (ts
          |> List.map (fun t ->
                 sf " - %s" (Michelson_script.path ~no_prefix:true t))
          |> String.concat "\n")
  in
  unit

let test_version_range3 () =
  Test.register
    ~__FILE__
    ~title:"michelson_scripts: version_range3"
    ~tags:["michelson_scripts"; "version_range"]
  @@ fun () ->
  let prefix = make_prefix_dir () in
  let prev_proto = require_previous_protocol Alpha in
  with_previous_protocol prev_proto @@ fun prev_prev_proto ->
  (* Assuming:
     Alpha ~ 016
     prev_proto ~ 015
     prev_prev_proto ~ 014

     Create the following scripts:
     - foo/bar.tz
     - foo/bar_014_016.tz

     And test that [find_all prev_proto] returns an ok.
  *)
  let out_of_range = "foo" // script0 "bar" in
  let in_range = "foo" // script2 "bar" prev_prev_proto Alpha in
  let* () = touch_p (prefix // out_of_range) in
  let* () = touch_p (prefix // in_range) in
  let () =
    match Michelson_script.find_all_res ~prefix prev_proto with
    | Error (__LOC__, msg) ->
        Test.fail ~__LOC__ "Expected Ok, got Error:\n%s" msg
    | Ok ts ->
        let paths = ts |> List.map (Michelson_script.path ~no_prefix:true) in
        Check.( = )
          paths
          ["foo" // script2 "bar" prev_prev_proto Alpha]
          Check.(list string)
          ~__LOC__
          ~error_msg:"Got %R, expected %L"
  in

  unit

let test_version_range4 () =
  Test.register
    ~__FILE__
    ~title:"michelson_scripts: version_range4"
    ~tags:["michelson_scripts"; "version_range"]
  @@ fun () ->
  let prefix = make_prefix_dir () in
  let prev_proto = require_previous_protocol Alpha in
  with_previous_protocol prev_proto @@ fun prev_prev_proto ->
  (* Assuming:
     Alpha ~ 016
     prev_proto ~ 015
     prev_prev_proto ~ 014

     Create the following scripts:
     - foo/bar_014_015.tz
     - foo/bar_015_016.tz

     And test that [find_all prev_proto] returns an error.
  *)
  let* () =
    touch_p (prefix // "foo" // script2 "bar" prev_prev_proto prev_proto)
  in
  let* () = touch_p (prefix // "foo" // script2 "bar" prev_proto Alpha) in
  let () =
    match Michelson_script.find_all_res ~prefix prev_proto with
    | Error (__LOC__, msg) ->
        Log.info "Expected Error, got Error:" ;
        Log.info "%s: %s" __LOC__ msg
    | Ok ts ->
        Test.fail
          ~__LOC__
          "Expected Error, got Ok:\n%s"
          (ts
          |> List.map (fun t ->
                 sf " - %s" (Michelson_script.path ~no_prefix:true t))
          |> String.concat "\n")
  in
  unit

let test_version_range5 () =
  Test.register
    ~__FILE__
    ~title:"michelson_scripts: version_range5"
    ~tags:["michelson_scripts"; "version_range"]
  @@ fun () ->
  let prefix = make_prefix_dir () in
  let prev_proto = require_previous_protocol Alpha in
  (* Assuming:
     Alpha ~ 016
     prev_proto ~ 015
     prev_prev_proto ~ 014

     Create the following scripts:
     - foo/bar.tz
     - foo/bar_015_015.tz

     And test that [find_all prev_proto] returns an error.
  *)
  let* () = touch_p (prefix // "foo" // script0 "bar") in
  let* () = touch_p (prefix // "foo" // script2 "bar" prev_proto prev_proto) in
  let () =
    match Michelson_script.find_all_res ~prefix prev_proto with
    | Error (__LOC__, msg) ->
        Test.fail ~__LOC__ "Expected Ok, got Error:\n%s" msg
    | Ok ts ->
        Log.info
          "Expected Ok, got Ok:\n%s"
          (ts
          |> List.map (fun t ->
                 sf " - %s" (Michelson_script.path ~no_prefix:true t))
          |> String.concat "\n")
  in
  unit

let test_version_range6 () =
  Test.register
    ~__FILE__
    ~title:"michelson_scripts: version_range6"
    ~tags:["michelson_scripts"; "version_range"]
  @@ fun () ->
  let prefix = make_prefix_dir () in
  let prev_proto = require_previous_protocol Alpha in
  with_previous_protocol prev_proto @@ fun prev_prev_proto ->
  (* Assuming:
     Alpha ~ 016
     prev_proto ~ 015
     prev_prev_proto ~ 014

     Create the following scripts:
     - foo/bar_014_014.tz
     - foo/bar_016.tz

     And test that [find_all prev_proto] returns an error.
  *)
  let* () =
    touch_p (prefix // "foo" // script2 "bar" prev_prev_proto prev_prev_proto)
  in
  let* () = touch_p (prefix // "foo" // script1 "bar" Alpha) in
  let () =
    match Michelson_script.find_all_res ~prefix prev_proto with
    | Error (__LOC__, msg) ->
        Test.fail ~__LOC__ "Expected Ok, got Error:\n%s" msg
    | Ok ts ->
        let paths = ts |> List.map (Michelson_script.path ~no_prefix:true) in
        Check.( = )
          paths
          []
          Check.(list string)
          ~__LOC__
          ~error_msg:"Expected %R, got %L"
  in
  unit

let test_version_range7 () =
  Test.register
    ~__FILE__
    ~title:"michelson_scripts: version_range7"
    ~tags:["michelson_scripts"; "version_range"]
  @@ fun () ->
  let prefix = make_prefix_dir () in
  let prev_proto = require_previous_protocol Alpha in
  with_previous_protocol prev_proto @@ fun prev_prev_proto ->
  (* Assuming:
     Alpha ~ 016
     prev_proto ~ 015
     prev_prev_proto ~ 014

     Create the following scripts:
     - foo/bar_014_014.tz
     - foo/bar.tz

     And test that [find_all prev_proto] returns an error.
  *)
  let* () =
    touch_p (prefix // "foo" // script2 "bar" prev_prev_proto prev_prev_proto)
  in
  let* () = touch_p (prefix // "foo" // script1 "bar" Alpha) in
  let () =
    match Michelson_script.find_all_res ~prefix prev_proto with
    | Error (__LOC__, msg) ->
        Test.fail ~__LOC__ "Expected Ok, got Error:\n%s" msg
    | Ok ts ->
        let paths = ts |> List.map (Michelson_script.path ~no_prefix:true) in
        Check.( = )
          paths
          []
          Check.(list string)
          ~__LOC__
          ~error_msg:"Expected []"
  in
  unit

let () =
  test_all () ;
  test_find () ;
  test_version_range1 () ;
  test_version_range2 () ;
  test_version_range3 () ;
  test_version_range4 () ;
  test_version_range5 () ;
  test_version_range6 () ;
  test_version_range7 ()
