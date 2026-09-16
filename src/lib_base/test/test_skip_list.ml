(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Nomadic Labs, <contact@nomadic-labs.com>               *)
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
    Component:  Base
    Invocation: dune exec src/lib_base/test/main.exe \
                  -- --file test_skip_list.ml
    Subject:    Test skip list implementation
*)

open TzPervasives

exception Skip_list_test_error of string

module TestNat (Parameters : sig
  val basis : int
end) =
struct
  open Parameters
  include Skip_list.Make (Parameters)

  (* This represents cells of skip lists. *)
  type t = {size : int; cells : (int * (int, int) cell) list}

  let deref ?(lowest_known_cell = 0) list i =
    if i < lowest_known_cell then None
    else List.assoc ~equal:Int.equal i list.cells

  (* Must be an even number. See {!val:succ}. *)
  let initial_value = 10

  (* Since the list was initialised once/computed once, we can get
     back its content from its index directly. *)
  let content_from_index ~default list i =
    match deref list i with None -> default | Some x -> content x

  let show_cell cell =
    Printf.sprintf
      "{ content = %d; back_pointers = %s }"
      (content cell)
      (back_pointers cell |> List.map string_of_int |> String.concat " ")

  let show_cells cells =
    String.concat
      "; "
      (List.map
         (fun (i, cell) -> Printf.sprintf "%d:%s" i (show_cell cell))
         cells)

  let show_list list =
    Printf.sprintf
      "basis: %d, size: %d, cells = %s"
      basis
      list.size
      (show_cells list.cells)

  let show_path path = String.concat " " (List.map string_of_int path)

  let head list =
    match List.hd list.cells with
    | None -> Test.fail "head function called on an empty list"
    | Some h -> h

  let zero = {size = 1; cells = [(0, genesis initial_value)]}

  let succ list =
    let prev_cell_ptr, prev_cell = head list in
    (* Content of cells are only even numbers so that searching odd numbers will always fail. *)
    let cell =
      next ~prev_cell ~prev_cell_ptr ((2 * list.size) + initial_value)
    in
    {size = list.size + 1; cells = (list.size, cell) :: list.cells}

  let back_path list start stop =
    back_path ~deref:(deref list) ~cell_ptr:start ~target_index:(Z.of_int stop)

  let find list start stop =
    find ~deref:(deref list) ~cell_ptr:start ~target_index:(Z.of_int stop)

  let search ?lowest_known_cell list start target_content =
    search
      ~deref:(deref ?lowest_known_cell list)
      ~compare:(fun x -> Int.(compare x target_content))
      ~cell:start

  let valid_back_path list start stop path =
    valid_back_path
      ~equal_ptr:( = )
      ~deref:(deref list)
      ~cell_ptr:start
      ~target_ptr:stop
      path

  (* This produces a skip lists whose content are even numbers from
     {!val:initial_value} and increase 2 by 2. *)
  let rec nlist basis n = if n = 0 then zero else succ (nlist basis (n - 1))

  (* This produces a skip lists whose content is just the index of the cell. *)
  let rec nlist' basis n =
    if n = 0 then {size = 1; cells = [(0, genesis 0)]}
    else
      let list = nlist' basis (n - 1) in
      let prev_cell_ptr, prev_cell = head list in
      let cell = next ~prev_cell ~prev_cell_ptr list.size in
      {size = list.size + 1; cells = (list.size, cell) :: list.cells}

  let check_find i j =
    let l = nlist basis i in
    let () =
      match find l i j with
      | None -> Test.fail ~__LOC__ "There must be a cell (%d)" i
      | Some cell ->
          let index = Z.to_int (index cell) in
          if index <> j then
            Test.fail
              ~__LOC__
              "Found cell is not the correct one (found %d, expected %d)"
              index
              j
    in
    let path =
      match back_path l i j with
      | None -> Test.fail ~__LOC__ "There must be path from %d to %d" i j
      | Some path -> path
    in
    let () =
      match List.(hd (rev path)) with
      | None ->
          Test.fail ~__LOC__ " There can't be an empty path from %d to %d" i j
      | Some stop_cell ->
          if j <> stop_cell then
            Test.fail
              ~__LOC__
              "Found cell is not equal to stop cell of back path (%d to %d)"
              i
              j
    in
    ()

  let check_invalid_find i =
    let l = nlist basis i in
    let check_nothing_found i j =
      match find l i j with
      | None -> ()
      | Some _v ->
          Test.fail ~__LOC__ "There should be no value found at %d from %d" i j
    in
    check_nothing_found i (-1) ;
    let rec aux j =
      if i > j then (
        check_nothing_found j i ;
        aux (j + 1))
    in
    aux 0

  let check_path i j back_path_fn =
    let l = nlist basis i in
    let path = back_path_fn l i j in
    match path with
    | None -> Test.fail ~__LOC__ "There must be path from %d to %d" i j
    | Some path ->
        let len = List.length path in
        let log_basis x =
          int_of_float @@ ceil (log (float_of_int x) /. log (float_of_int basis))
        in
        let log_ij = log_basis (i - j + 1) in
        let expected = max 1 (log_ij * basis) in
        if len > expected then
          Test.fail
            ~__LOC__
            "The proof is too long! Expected = %d < len = %d [basis = %d, i = \
             %d, log = %d, j = %d]\n"
            expected
            len
            basis
            i
            log_ij
            j ;
        if not (valid_back_path l i j path) then
          Test.fail
            ~__LOC__
            "The path %s does not connect %d to %d (or is invalid/non-minimal)"
            (show_path path)
            i
            j

  let check_invalid_paths i =
    let l = nlist basis i in
    let rec aux j =
      if i <= j then ()
      else
        let () =
          match back_path l j i with
          | None -> ()
          | Some _path ->
              Test.fail
                ~__LOC__
                "There should be no path connecting %d to %d"
                j
                i
        in
        aux (j + 1)
    in
    aux 0

  let check_lower_path history rev_path target =
    match rev_path with
    | [] ->
        (* checked before. *)
        assert false
    | [cell_x] ->
        if
          (* If there is a single element, we check the content of the
             cell is smaller than the target. *)
          Compare.Int.(content cell_x >= target)
        then Test.fail ~__LOC__ "Invalid path: %d" target
    | rev_path -> (
        (* The path is returned from the start cell to the target. The
           invariant we want to check is in the opposite direction. *)
        match rev_path with
        | cell_x :: cell_z :: _ -> (
            let i = Z.to_int (index cell_x) in
            let next_index = i + 1 in
            match
              List.nth history.cells (List.length history.cells - next_index - 1)
            with
            | None -> assert false
            | Some (_y, cell_y) ->
                if
                  Compare.Int.(
                    content cell_x < target
                    && target < content cell_y
                    && content cell_y <= content cell_z)
                then ()
                else Test.fail ~__LOC__ "Invariant for 'Lower' is broken")
        | _ -> assert false)

  let check_invalid_search_paths i =
    let l = nlist basis i in
    let rec aux j =
      if i <= j then ()
      else
        (* An odd number to make the search fails. *)
        let shift_size = 5 in
        (* delta is chosen so that j + delta is not in the list and
           can be below the smallest element and greater than the
           largest element. *)
        let delta =
          if List.length l.cells mod 2 = 0 then -shift_size else shift_size
        in
        let t = content_from_index ~default:(-1) l j + delta in
        (* By construction, deref never fails since j <= List.length list. *)
        match deref l i with
        | None -> assert false
        | Some start_content ->
            (* For each case below, we check whether the last cell
               returned is valid with respect to the current path. Two
               cases are not possible. *)
            (match search l start_content t with
            | {last_cell = No_exact_or_lower_ptr; rev_path} -> (
                (* In that case, we check the path returned by search
                   is above the target. *)
                match rev_path with
                | [] -> Test.fail ~__LOC__ "unexpected empty path"
                | head :: _ ->
                    if Compare.Int.(content head > t) then ()
                    else
                      Test.fail
                        ~__LOC__
                        "Invariant for 'No_exact_or_lower_ptr' broken")
            | {last_cell = Nearest _; rev_path} ->
                (* In that case, we check the property of being a lower path. *)
                check_lower_path l rev_path t
            | {last_cell = Deref_returned_none; _} ->
                (* deref should always work *)
                assert false
            | {last_cell = Found _; _} ->
                (* Because we search for a cell that which is not in
                   the list, if the cell was found, we fail. *)
                Test.fail
                  ~__LOC__
                  "There should be no search path connecting %d to a node with \
                   content %d"
                  i
                  t) ;
            aux (j + 1)
    in
    aux 0

  let pp_search_result fmt =
    pp_search_result
      ~pp_cell:(fun fmt cell -> Format.fprintf fmt "%s" (show_cell cell))
      fmt
end

let () =
  Qcheck_tezt.register ~__FILE__ ~tags:Tezos_test_helpers.Tag.[layer1; base]
  @@
  let generator =
    QCheck2.Gen.(
      let* basis = frequency [(5, pure 4); (1, 2 -- 73)] in
      let* i = 0 -- 100 in
      let* j = 0 -- i in
      return (basis, i, j))
  in
  QCheck2.Test.make
    ~name:"Skip list: produce paths with `back_path` and check"
    ~count:10
    generator
  @@ fun (basis, i, j) ->
  let module M = TestNat (struct
    let basis = basis
  end) in
  let back_path list start stop = M.back_path list start stop in
  M.check_path i j back_path ;
  true

let () =
  Qcheck_tezt.register ~__FILE__ ~tags:Tezos_test_helpers.Tag.[layer1; base]
  @@
  let generator =
    QCheck2.Gen.(
      let* basis = frequency [(5, pure 4); (1, 2 -- 73)] in
      let* i = 0 -- 100 in
      let* j = 0 -- i in
      return (basis, i, j))
  in
  QCheck2.Test.make
    ~name:"Skip list: find cell with `find` and `check`"
    ~count:10
    generator
  @@ fun (basis, i, j) ->
  let module M = TestNat (struct
    let basis = basis
  end) in
  M.check_find i j ;
  true

let () =
  Qcheck_tezt.register ~__FILE__ ~tags:Tezos_test_helpers.Tag.[layer1; base]
  @@
  let generator =
    QCheck2.Gen.(
      let* basis = frequency [(5, pure 4); (1, 2 -- 73)] in
      let* i = 0 -- 100 in
      return (basis, i))
  in
  QCheck2.Test.make
    ~name:"Skip list: `find` won't produce invalid value"
    ~count:10
    generator
  @@ fun (basis, i) ->
  let module M = TestNat (struct
    let basis = basis
  end) in
  M.check_invalid_find i ;
  true

let () =
  Qcheck_tezt.register ~__FILE__ ~tags:Tezos_test_helpers.Tag.[layer1; base]
  @@
  let generator =
    QCheck2.Gen.(
      let* basis = frequency [(5, pure 4); (1, 2 -- 73)] in
      let* i = 0 -- 100 in
      return (basis, i))
  in
  QCheck2.Test.make
    ~name:"Skip list: `back_path` won't produce invalid paths"
    ~count:10
    generator
  @@ fun (basis, i) ->
  let module M = TestNat (struct
    let basis = basis
  end) in
  M.check_invalid_paths i ;
  true

let () =
  Test.register
    ~__FILE__
    ~title:"Skip list: check if the back_path is minimal"
    ~tags:Tezos_test_helpers.Tag.[layer1; base]
  @@ fun () ->
  let basis = 4 in
  let module M = TestNat (struct
    let basis = basis
  end) in
  let l = M.nlist basis 20 in
  let check_minimal_path = function
    | None, _ -> Test.fail ~__LOC__ "empty path"
    | Some path, expected_path ->
        if path = expected_path then ()
        else
          Test.fail
            ~__LOC__
            "non-minimal path:[%s] != expected_path:[%s]"
            (M.show_path path)
            (M.show_path expected_path)
  in
  let cases =
    [
      (6, 1, [6; 3; 2; 1]);
      (6, 3, [6; 3]);
      (10, 3, [10; 7; 3]);
      (10, 5, [10; 7; 6; 5]);
      (10, 7, [10; 7]);
      (10, 9, [10; 9]);
    ]
  in
  List.iter
    check_minimal_path
    (List.map
       (fun (start, target, expected_path) ->
         (M.back_path l start target, expected_path))
       cases) ;
  unit

let () =
  Qcheck_tezt.register ~__FILE__ ~tags:Tezos_test_helpers.Tag.[layer1; base]
  @@
  let generator =
    QCheck2.Gen.(
      let* basis = frequency [(5, pure 4); (1, 2 -- 73)] in
      let* i = 0 -- 100 in
      let* j = 0 -- i in
      return (basis, i, j))
  in
  QCheck2.Test.make
    ~name:"Skip list: produce paths with `search` and check"
    ~count:10
    generator
  @@ fun (basis, i, j) ->
  let module M = TestNat (struct
    let basis = basis
  end) in
  M.check_path i j (fun l i j ->
      let target = M.content_from_index ~default:(-1) l j in
      let start =
        match M.deref l i with None -> assert false | Some start -> start
      in
      match M.search l start target with
      | {last_cell = Found _; rev_path} ->
          List.rev_map
            (fun cell ->
              let x = M.content cell in
              (x - 10) / 2)
            rev_path
          |> Option.some
      | _result -> None) ;
  true

(* This test ensures that if [j] is part of the skip list, then we can
   produce a path from [i] to [j] without knowing any cell strictly
   below [j]. *)
let () =
  Qcheck_tezt.register ~__FILE__ ~tags:Tezos_test_helpers.Tag.[layer1; base]
  @@
  let generator =
    QCheck2.Gen.(
      let* basis = frequency [(5, pure 4); (1, 2 -- 73)] in
      let* i = pure 100 in
      let* j = 0 -- i in
      return (basis, i, j))
  in
  QCheck2.Test.make
    ~name:"Skip list: produce a path when deref is partial"
    ~count:100
    generator
  @@ fun (basis, i, j) ->
  let module M = TestNat (struct
    let basis = basis
  end) in
  (* The list is equivalent to [0;1;2;3;...;i]. *)
  let l = M.nlist' basis i in
  (match M.deref l i with
  | None -> Test.fail "Unable to deref initial cell"
  | Some start -> (
      match M.search ~lowest_known_cell:j l start j with
      | M.{last_cell = Nearest _; rev_path = _} ->
          (* This is not supposed to happen since [j] is part of the Skip list. *)
          Test.fail "Skip_list.search returned 'Nearest'"
      | M.{last_cell = Found _; rev_path = _} ->
          (* We do not check the validity of the path here and rely on
             other tests for that. *)
          ()
      | M.{last_cell = No_exact_or_lower_ptr; rev_path = _} ->
          (* This is not supposed to happen since [j] is part of the Skip list. *)
          Test.fail "Skip_list.search returned 'No_exact_or_lower_ptr'"
      | M.{last_cell = Deref_returned_none; rev_path = _} ->
          (* We want to ensure that the [Skip_list.search] function
             succeeds when [deref] is used on older cells than the
             target one when it exists. Hence, this error should not
             happen. *)
          Test.fail "Skip_list.search returned 'Deref_returned_none'")) ;
  true

let () =
  Qcheck_tezt.register ~__FILE__ ~tags:Tezos_test_helpers.Tag.[layer1; base]
  @@
  let generator =
    QCheck2.Gen.(
      let* basis = frequency [(5, pure 4); (1, 2 -- 73)] in
      let* i = 0 -- 10 in
      return (basis, i))
  in
  QCheck2.Test.make
    ~name:"Skip list: `search` won't produce invalid paths"
    ~count:10
    generator
  @@ fun (basis, i) ->
  let module M = TestNat (struct
    let basis = basis
  end) in
  M.check_invalid_search_paths i ;
  true

let () =
  Qcheck_tezt.register ~__FILE__ ~tags:Tezos_test_helpers.Tag.[layer1; base]
  @@ QCheck2.Test.make
       ~name:"Skip list: genesis has valid indices"
       ~count:10_000
       QCheck2.Gen.(pair (2 -- 73) int)
  @@ fun (basis, content) ->
  let module M = Skip_list.Make (struct
    let basis = basis
  end) in
  M.valid_indices
    ~deref:(fun _ -> Test.fail "Genesis must not dereference any pointer")
    (M.genesis content)

let () =
  Qcheck_tezt.register ~__FILE__ ~tags:Tezos_test_helpers.Tag.[layer1; base]
  @@
  let generator =
    QCheck2.Gen.(
      let* basis = frequency [(5, pure 4); (1, 2 -- 73)] in
      let large_index =
        let* bytes = list_size (0 -- 128) (0 -- 255) in
        return
          (List.fold_left
             (fun n byte -> Z.(add (shift_left n 8) (of_int byte)))
             Z.zero
             bytes)
      in
      let power =
        let* exponent = 0 -- 128 in
        return (Z.pow (Z.of_int basis) exponent)
      in
      let boundary =
        let* power = power in
        let* multiple = 1 -- (basis - 1) in
        return Z.(mul power (of_int multiple))
      in
      let steps = frequency [(1, pure 0); (3, pure 1); (2, 2 -- 8)] in
      let* initial_index, steps =
        frequency
          [
            (* Crossing a power adds a new back-pointer slot. *)
            (4, map (fun power -> (Z.pred power, 1)) power);
            (* Crossing a multiple refreshes existing pointer slots. *)
            (3, map (fun boundary -> (Z.pred boundary, 1)) boundary);
            ( 2,
              let* boundary = boundary in
              let* offset = -1 -- 1 in
              let* steps = steps in
              return (Z.max Z.zero Z.(add boundary (of_int offset)), steps) );
            ( 2,
              let* offset = -1 -- 1 in
              let* steps = 1 -- 8 in
              return (Z.of_int (15_000_000 + offset), steps) );
            (1, pair large_index steps);
          ]
      in
      return (basis, initial_index, steps))
  in
  QCheck2.Test.make
    ~name:"Skip list: next preserves valid indices"
    ~count:10_000
    ~print:(fun (basis, initial_index, steps) ->
      Printf.sprintf
        "basis=%d, initial_index=%s, steps=%d"
        basis
        (Z.to_string initial_index)
        steps)
    generator
  @@ fun (basis, initial_index, steps) ->
  let module M = Skip_list.Make (struct
    let basis = basis
  end) in
  let encoding = M.encoding Data_encoding.n Data_encoding.unit in
  let raw_encoding =
    Data_encoding.(
      obj3 (req "index" n) (req "content" unit) (req "back_pointers" (list n)))
  in
  let cell_at index pointers =
    Data_encoding.Json.destruct
      encoding
      (Data_encoding.Json.construct raw_encoding (index, (), pointers))
  in
  (* Pointers are indices. Only target indices are inspected by the predicate,
     so dereferencing needs no recursively materialised ancestor list. *)
  let deref index = Some (cell_at index []) in
  let rec pointers power =
    if Z.gt power initial_index then []
    else
      Z.(pred (mul (div initial_index power) power))
      :: pointers Z.(mul power (of_int basis))
  in
  let initial_cell = cell_at initial_index (pointers Z.one) in
  let rec check cell remaining =
    M.valid_indices ~deref cell
    && (remaining = 0
       || check
            (M.next ~prev_cell:cell ~prev_cell_ptr:(M.index cell) ())
            (remaining - 1))
  in
  check initial_cell steps

let () =
  Test.register
    ~__FILE__
    ~title:"Skip list: reject tampered path indices"
    ~tags:Tezos_test_helpers.Tag.[layer1; base]
  @@ fun () ->
  let module M = TestNat (struct
    let basis = 2
  end) in
  let list = M.nlist 2 13 in
  let path =
    match M.back_path list 13 3 with
    | Some path -> path
    | None -> Test.fail "Could not produce fixture proof"
  in
  if not (M.valid_back_path list 13 3 path) then
    Test.fail "Original proof rejected by valid_back_path" ;
  let proof =
    List.map
      (fun ptr ->
        match M.deref list ptr with
        | Some cell -> (ptr, cell)
        | None -> Test.fail "Missing fixture cell")
      path
  in
  let make_cell index content pointers =
    let raw_encoding =
      Data_encoding.(
        obj3
          (req "index" n)
          (req "content" int31)
          (req "back_pointers" (list int31)))
    in
    Data_encoding.Json.destruct
      (M.encoding Data_encoding.int31 Data_encoding.int31)
      (Data_encoding.Json.construct raw_encoding (index, content, pointers))
  in
  let valid ?(start_index = Z.of_int 13) proof path =
    M.valid_back_path_indices
      ~equal_ptr:Int.equal
      ~deref:(fun ptr -> List.assoc ~equal:Int.equal ptr proof)
      ~start_index
      path
  in
  if not (valid proof path) then Test.fail "Valid sparse proof rejected" ;
  List.iter
    (fun changed_ptr ->
      let tampered =
        List.map
          (fun (ptr, cell) ->
            ( ptr,
              if ptr = changed_ptr then
                make_cell
                  (Z.pred (M.index cell))
                  (M.content cell)
                  (M.back_pointers cell)
              else cell ))
          proof
      in
      if valid tampered path then Test.fail "Tampered index accepted")
    path ;
  if valid ~start_index:(Z.of_int 12) proof path then
    Test.fail "Incorrect starting index accepted" ;
  if valid proof [] then Test.fail "Empty path accepted" ;
  if valid proof [13; 3] then Test.fail "Nonexistent edge accepted" ;
  if valid proof [13; 7; 2] then Test.fail "Missing path cell accepted" ;
  if not (valid proof [13]) then Test.fail "Valid singleton rejected" ;
  if not (valid ~start_index:Z.zero [(0, M.genesis 0)] [0]) then
    Test.fail "Genesis path rejected" ;
  let malformed = make_cell (Z.of_int 3) 0 [] in
  let malformed_proof =
    List.map
      (fun (ptr, cell) -> (ptr, if ptr = 3 then malformed else cell))
      proof
  in
  if valid malformed_proof path then
    Test.fail "Malformed final pointer count accepted" ;
  let large_index = Z.shift_left Z.one 100 in
  let large_proof =
    [
      (13, make_cell large_index 0 (7 :: Stdlib.List.init 100 (fun _ -> 0)));
      (7, make_cell (Z.pred large_index) 0 (Stdlib.List.init 100 (fun _ -> 0)));
    ]
  in
  if not (valid ~start_index:large_index large_proof [13; 7]) then
    Test.fail "Large path indices rejected" ;
  unit

let () =
  Test.register
    ~__FILE__
    ~title:"Skip list: reject relabelled generated proofs"
    ~tags:Tezos_test_helpers.Tag.[layer1; base]
  @@ fun () ->
  List.iter
    (fun basis ->
      let module M = TestNat (struct
        let basis = basis
      end) in
      let history = M.nlist basis 32 in
      let encoding = M.encoding Data_encoding.int31 Data_encoding.int31 in
      let relabel cell new_index =
        match Data_encoding.Json.construct encoding cell with
        | `O fields ->
            Data_encoding.Json.destruct
              encoding
              (`O
                 (List.map
                    (fun (name, value) ->
                      ( name,
                        if String.equal name "index" then
                          Data_encoding.Json.construct Data_encoding.n new_index
                        else value ))
                    fields))
        | _ -> Test.fail "Expected a cell object"
      in
      for start = 0 to 32 do
        for target = 0 to start do
          let path =
            match M.back_path history start target with
            | Some path -> path
            | None -> Test.fail "Could not produce original proof"
          in
          if not (M.valid_back_path history start target path) then
            Test.fail "Original proof rejected by valid_back_path" ;
          (* Only proof cells are available; pointer identities and contents
             stay unchanged when the encoded indices are tampered with. *)
          let proof =
            List.map
              (fun ptr ->
                match M.deref history ptr with
                | Some cell -> (ptr, cell)
                | None -> Test.fail "Missing original proof cell")
              path
          in
          let valid proof =
            M.valid_back_path_indices
              ~equal_ptr:Int.equal
              ~deref:(fun ptr -> List.assoc ~equal:Int.equal ptr proof)
              ~start_index:(Z.of_int start)
              path
          in
          if not (valid proof) then
            Test.fail
              "Original sparse proof rejected: basis=%d start=%d target=%d"
              basis
              start
              target ;
          List.iter
            (fun changed_ptr ->
              List.iter
                (fun delta ->
                  if changed_ptr + delta >= 0 then
                    let tampered =
                      List.map
                        (fun (ptr, cell) ->
                          ( ptr,
                            if ptr = changed_ptr then
                              relabel cell Z.(add (M.index cell) (of_int delta))
                            else cell ))
                        proof
                    in
                    if valid tampered then
                      Test.fail
                        "Relabelled proof accepted: basis=%d start=%d \
                         target=%d changed_ptr=%d delta=%d"
                        basis
                        start
                        target
                        changed_ptr
                        delta)
                [-1; 1; basis])
            path ;
          let shifted =
            List.map
              (fun (ptr, cell) -> (ptr, relabel cell (Z.succ (M.index cell))))
              proof
          in
          if valid shifted then Test.fail "Uniformly shifted proof accepted"
        done
      done)
    [2; 3; 4; 16] ;
  unit

let () =
  Test.register
    ~__FILE__
    ~title:"Skip list: validate cell indices"
    ~tags:Tezos_test_helpers.Tag.[layer1; base]
  @@ fun () ->
  List.iter
    (fun basis ->
      let module M = TestNat (struct
        let basis = basis
      end) in
      let l = M.nlist basis 64 in
      let deref = M.deref l in
      List.iter
        (fun (_, cell) ->
          if not (M.valid_indices ~deref cell) then
            Test.fail "Valid cell rejected (basis %d)" basis)
        l.cells ;
      let _, cell = M.head l in
      if M.valid_indices ~deref:(fun _ -> None) cell then
        Test.fail "Missing target accepted" ;
      if M.valid_indices ~deref:(fun _ -> Some cell) cell then
        Test.fail "Incorrect target index accepted" ;
      let encoding = M.encoding Data_encoding.int31 Data_encoding.int31 in
      let with_pointers cell pointers =
        match Data_encoding.Json.construct encoding cell with
        | `O fields ->
            Data_encoding.Json.destruct
              encoding
              (`O
                 (List.map
                    (fun (name, value) ->
                      ( name,
                        if String.equal name "back_pointers" then
                          Data_encoding.Json.construct
                            Data_encoding.(list int31)
                            pointers
                        else value ))
                    fields))
        | _ -> Test.fail "Expected a cell object"
      in
      let pointers = M.back_pointers cell in
      let fewer = match pointers with [] -> [] | _ :: rest -> rest in
      List.iter
        (fun malformed ->
          if M.valid_indices ~deref malformed then
            Test.fail "Incorrect pointer count accepted")
        [
          with_pointers cell fewer;
          with_pointers cell (0 :: pointers);
          with_pointers (M.genesis 0) [0];
        ])
    [2; 3; 4; 16] ;
  unit

let () =
  Test.register
    ~__FILE__
    ~title:"Skip list: `search` may not produce minimal path"
    ~tags:Tezos_test_helpers.Tag.[layer1; base]
  @@ fun () ->
  let basis = 4 in
  let module M = TestNat (struct
    let basis = basis
  end) in
  let l = M.nlist basis 100 in
  let index_of_content candidate =
    match List.find (fun (_, cell) -> cell = candidate) l.cells with
    | None -> assert false
    | Some (x, _) -> x
  in
  let deref x = match M.deref l x with None -> assert false | Some x -> x in
  (* This target is chosen to demonstrate that the path is not always
     minimal, but this happens only on the very last node. [target]
     must be odd to ensure the content is not in the list. *)
  let target = 17 in
  let start_index = 100 in
  let start = deref start_index in
  (* Since we are only checking the minimality of the path returned by
     search, we assume the other part of the [search] specification to
     be correct below (hence the [assert false]). *)
  (match M.search l start target with
  | M.{last_cell = Nearest {lower; upper = Some upper}; rev_path} -> (
      match rev_path with
      | [] ->
          (* By specification of the function [search]. *)
          assert false
      | _lower :: upper_path as lower_path -> (
          (* We check the upper path is minimal. *)
          let upper_index = index_of_content upper in
          match M.back_path l start_index upper_index with
          | None ->
              (* By specification of the function [search]. *)
              assert false
          | Some upper_expected_path ->
              if List.rev upper_path = List.map deref upper_expected_path then
                (* We check the lower path is not minimal. *)
                let lower_index = index_of_content lower in
                match M.back_path l start_index lower_index with
                | None ->
                    (* By specification of the function [search]. *)
                    assert false
                | Some lower_expected_path ->
                    if List.rev lower_path = List.map deref lower_expected_path
                    then
                      Test.fail
                        ~__LOC__
                        "The path returned is minimal while it should not be \
                         the case."
                    else ()
              else (* By specification of the function [search]. *)
                assert false))
  | _ ->
      (* The cell does not exist in the list. *)
      assert false) ;
  unit

let index_proof_case ?(min_head = 0) () =
  QCheck2.Gen.(
    let* basis = frequency [(5, pure 4); (1, 2 -- 16)] in
    let* head = min_head -- 500 in
    let* target = 0 -- head in
    return (basis, head, target))

let () =
  Qcheck_tezt.register ~__FILE__ ~tags:Tezos_test_helpers.Tag.[layer1; base]
  @@ QCheck2.Test.make
       ~name:"Skip list: produce and verify index proofs"
       ~count:1_000
       (index_proof_case ())
  @@ fun (basis, head, target) ->
  let module M = TestNat (struct
    let basis = basis
  end) in
  let history = M.nlist basis head in
  let index_proof =
    match
      M.produce_index_proof
        ~equal_ptr:Int.equal
        ~deref:(M.deref history)
        ~cell_ptr:head
    with
    | Some proof -> proof
    | None -> Test.fail "Could not produce index proof"
  in
  let inclusion_proof =
    match M.back_path history head target with
    | Some proof -> proof
    | None -> Test.fail "Could not produce inclusion proof"
  in
  M.verify_index_proof
    ~equal_ptr:Int.equal
    ~deref:(M.deref history)
    ~cell_ptr:head
    ~inclusion_proof
    index_proof

let () =
  Qcheck_tezt.register ~__FILE__ ~tags:Tezos_test_helpers.Tag.[layer1; base]
  @@ QCheck2.Test.make
       ~name:"Skip list: reject malformed index proofs"
       ~count:1_000
       (index_proof_case ~min_head:1 ())
  @@ fun (basis, head, target) ->
  let module M = TestNat (struct
    let basis = basis
  end) in
  let target = if Int.equal target head then head - 1 else target in
  let history = M.nlist basis head in
  let index_proof =
    match
      M.produce_index_proof
        ~equal_ptr:Int.equal
        ~deref:(M.deref history)
        ~cell_ptr:head
    with
    | Some proof -> proof
    | None -> Test.fail "Could not produce index proof"
  in
  let inclusion_proof =
    match M.back_path history head target with
    | Some proof -> proof
    | None -> Test.fail "Could not produce inclusion proof"
  in
  let encoding = M.encoding Data_encoding.int31 Data_encoding.int31 in
  let relabel cell =
    match Data_encoding.Json.construct encoding cell with
    | `O fields ->
        Data_encoding.Json.destruct
          encoding
          (`O
             (List.map
                (fun (name, value) ->
                  ( name,
                    if String.equal name "index" then
                      Data_encoding.Json.construct
                        Data_encoding.n
                        Z.(succ (M.index cell))
                    else value ))
                fields))
    | _ -> Test.fail "Expected a cell object"
  in
  let rejects_relabelled ptr =
    let deref candidate =
      match M.deref history candidate with
      | Some cell when Int.equal candidate ptr -> Some (relabel cell)
      | cell -> cell
    in
    not
      (M.verify_index_proof
         ~equal_ptr:Int.equal
         ~deref
         ~cell_ptr:head
         ~inclusion_proof
         index_proof)
  in
  let witness_ptr =
    match List.tl index_proof with
    | Some (ptr :: _) -> ptr
    | Some [] | None -> Test.fail "Index proof is unexpectedly a singleton"
  in
  let inclusion_ptr =
    match List.tl inclusion_proof with
    | Some (ptr :: _) -> ptr
    | Some [] | None ->
        (* [target = head] produces a singleton path; use the witness mutation
           for this generated case. *)
        witness_ptr
  in
  let truncated =
    match List.rev index_proof with
    | _last :: rest -> List.rev rest
    | [] -> Test.fail "Index proof is unexpectedly empty"
  in
  rejects_relabelled witness_ptr
  && rejects_relabelled inclusion_ptr
  && not
       (M.verify_index_proof
          ~equal_ptr:Int.equal
          ~deref:(M.deref history)
          ~cell_ptr:head
          ~inclusion_proof
          truncated)
