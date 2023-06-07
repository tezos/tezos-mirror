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

(** Testing
    -------
    Component:  Protocol (baking)
    Invocation: dune exec src/proto_016_PtMumbai/lib_protocol/test/unit/main.exe \
                  -- --file test_level_module.ml
    Subject:    some functions in the Level module
*)

open Protocol

let test_create_cycle_eras () =
  let empty_cycle_eras =
    Level_repr.create_cycle_eras [] |> Environment.wrap_tzresult
  in
  Assert.proto_error_with_info
    ~loc:__LOC__
    empty_cycle_eras
    "Invalid cycle eras"
  >>=? fun () ->
  let increasing_first_levels =
    [
      Level_repr.
        {
          first_level = Raw_level_repr.of_int32_exn 1l;
          first_cycle = Cycle_repr.succ Cycle_repr.root;
          blocks_per_cycle = 8l;
          blocks_per_commitment = 2l;
        };
      {
        first_level = Raw_level_repr.of_int32_exn 9l;
        first_cycle = Cycle_repr.root;
        blocks_per_cycle = 8l;
        blocks_per_commitment = 2l;
      };
    ]
    |> Level_repr.create_cycle_eras |> Environment.wrap_tzresult
  in
  Assert.proto_error_with_info
    ~loc:__LOC__
    increasing_first_levels
    "Invalid cycle eras"
  >>=? fun () ->
  let increasing_first_cycles =
    [
      Level_repr.
        {
          first_level = Raw_level_repr.of_int32_exn 9l;
          first_cycle = Cycle_repr.root;
          blocks_per_cycle = 8l;
          blocks_per_commitment = 2l;
        };
      {
        first_level = Raw_level_repr.of_int32_exn 1l;
        first_cycle = Cycle_repr.succ Cycle_repr.root;
        blocks_per_cycle = 8l;
        blocks_per_commitment = 2l;
      };
    ]
    |> Level_repr.create_cycle_eras |> Environment.wrap_tzresult
  in
  Assert.proto_error_with_info
    ~loc:__LOC__
    increasing_first_cycles
    "Invalid cycle eras"

let test_case_1 =
  ( [
      Level_repr.
        {
          first_level = Raw_level_repr.of_int32_exn 1l;
          first_cycle = Cycle_repr.root;
          blocks_per_cycle = 8l;
          blocks_per_commitment = 2l;
        };
    ],
    [
      (1, (1, 0, 0, 0, false));
      (2, (2, 1, 0, 1, true));
      (3, (3, 2, 0, 2, false));
      (8, (8, 7, 0, 7, true));
      (9, (9, 8, 1, 0, false));
      (16, (16, 15, 1, 7, true));
      (17, (17, 16, 2, 0, false));
      (64, (64, 63, 7, 7, true));
      (65, (65, 64, 8, 0, false));
    ] )

let test_case_2 =
  ( List.rev
      [
        Level_repr.
          {
            first_level = Raw_level_repr.of_int32_exn 1l;
            first_cycle = Cycle_repr.root;
            blocks_per_cycle = 8l;
            blocks_per_commitment = 2l;
          };
        {
          first_level = Raw_level_repr.of_int32_exn 17l;
          first_cycle = Cycle_repr.of_int32_exn 2l;
          blocks_per_cycle = 16l;
          blocks_per_commitment = 4l;
        };
      ],
    [
      (1, (1, 0, 0, 0, false));
      (2, (2, 1, 0, 1, true));
      (3, (3, 2, 0, 2, false));
      (8, (8, 7, 0, 7, true));
      (9, (9, 8, 1, 0, false));
      (16, (16, 15, 1, 7, true));
      (17, (17, 16, 2, 0, false));
      (32, (32, 31, 2, 15, true));
      (33, (33, 32, 3, 0, false));
      (64, (64, 63, 4, 15, true));
      (65, (65, 64, 5, 0, false));
    ] )

let test_case_3 =
  ( List.rev
      [
        Level_repr.
          {
            first_level = Raw_level_repr.of_int32_exn 1l;
            first_cycle = Cycle_repr.root;
            blocks_per_cycle = 8l;
            blocks_per_commitment = 2l;
          };
        {
          first_level = Raw_level_repr.of_int32_exn 17l;
          first_cycle = Cycle_repr.of_int32_exn 2l;
          blocks_per_cycle = 16l;
          blocks_per_commitment = 4l;
        };
        {
          first_level = Raw_level_repr.of_int32_exn 49l;
          first_cycle = Cycle_repr.of_int32_exn 4l;
          blocks_per_cycle = 6l;
          blocks_per_commitment = 3l;
        };
      ],
    [
      (1, (1, 0, 0, 0, false));
      (2, (2, 1, 0, 1, true));
      (3, (3, 2, 0, 2, false));
      (8, (8, 7, 0, 7, true));
      (9, (9, 8, 1, 0, false));
      (16, (16, 15, 1, 7, true));
      (17, (17, 16, 2, 0, false));
      (32, (32, 31, 2, 15, true));
      (33, (33, 32, 3, 0, false));
      (48, (48, 47, 3, 15, true));
      (49, (49, 48, 4, 0, false));
      (64, (64, 63, 6, 3, false));
      (65, (65, 64, 6, 4, false));
      (66, (66, 65, 6, 5, true));
      (67, (67, 66, 7, 0, false));
    ] )

let test_level_from_raw () =
  List.iter_es
    (fun (cycle_eras, test_cases) ->
      List.iter_es
        (fun ( input_level,
               ( level,
                 level_position,
                 cycle,
                 cycle_position,
                 expected_commitment ) ) ->
          let raw_level =
            Raw_level_repr.of_int32_exn (Int32.of_int input_level)
          in
          Level_repr.create_cycle_eras cycle_eras |> Environment.wrap_tzresult
          >>?= fun cycle_eras ->
          let level_from_raw =
            Protocol.Level_repr.level_from_raw ~cycle_eras raw_level
          in
          Assert.equal_int
            ~loc:__LOC__
            (Int32.to_int (Raw_level_repr.to_int32 level_from_raw.level))
            level
          >>=? fun () ->
          Assert.equal_int
            ~loc:__LOC__
            (Int32.to_int level_from_raw.level_position)
            level_position
          >>=? fun () ->
          Assert.equal_int
            ~loc:__LOC__
            (Int32.to_int (Cycle_repr.to_int32 level_from_raw.cycle))
            cycle
          >>=? fun () ->
          Assert.equal_int
            ~loc:__LOC__
            (Int32.to_int level_from_raw.cycle_position)
            cycle_position
          >>=? fun () ->
          Assert.equal_bool
            ~loc:__LOC__
            level_from_raw.expected_commitment
            expected_commitment
          >>=? fun () ->
          let offset =
            Int32.neg (Int32.add Int32.one (Int32.of_int input_level))
          in
          let res =
            Level_repr.level_from_raw_with_offset ~cycle_eras ~offset raw_level
          in
          Assert.proto_error
            ~loc:__LOC__
            (Environment.wrap_tzresult res)
            (fun err ->
              let error_info =
                Error_monad.find_info_of_error (Environment.wrap_tzerror err)
              in
              error_info.title = "Negative sum of level and offset"))
        test_cases)
    [test_case_1; test_case_2; test_case_3]

let test_first_level_in_cycle () =
  let cycle_eras = fst test_case_3 in
  let test_cases =
    (* cycle, level *)
    [
      (0l, 1);
      (1l, 9);
      (2l, 17);
      (3l, 33);
      (4l, 49);
      (5l, 55);
      (6l, 61);
      (7l, 67);
    ]
  in
  let f (input_cycle, level) =
    Level_repr.create_cycle_eras cycle_eras |> Environment.wrap_tzresult
    >>?= fun cycle_eras ->
    let input_cycle = Cycle_repr.of_int32_exn input_cycle in
    let level_res =
      Level_repr.first_level_in_cycle_from_eras ~cycle_eras input_cycle
    in
    Assert.equal_int
      ~loc:__LOC__
      (Int32.to_int (Raw_level_repr.to_int32 level_res.level))
      level
  in
  List.iter_es f test_cases

let tests =
  [
    Tztest.tztest "create_cycle_eras" `Quick test_create_cycle_eras;
    Tztest.tztest "level_from_raw" `Quick test_level_from_raw;
    Tztest.tztest "first_level_in_cycle" `Quick test_first_level_in_cycle;
  ]

let () =
  Alcotest_lwt.run ~__FILE__ Protocol.name [("level module", tests)]
  |> Lwt_main.run
