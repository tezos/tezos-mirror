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
    Component:  Protocol (skip lists)
    Invocation: dune exec src/proto_015_PtLimaPt/lib_protocol/test/unit/main.exe
    Subject:    Test skip list implementation
*)

open Protocol

exception Skip_list_test_error of string

let err x = Exn (Skip_list_test_error x)

module TestNat (Parameters : sig
  val basis : int
end) =
struct
  open Parameters
  include Skip_list_repr.Make (Parameters)

  (* This represents cells of skip lists whose content are even
     numbers from {!val:initial_value} and increase 2 by 2. *)
  type t = {size : int; cells : (int * (int, int) cell) list}

  let deref list i = List.assoc ~equal:Compare.Int.equal i list.cells

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
    match List.hd list.cells with None -> assert false | Some h -> h

  let zero = {size = 1; cells = [(0, genesis initial_value)]}

  let succ list =
    let prev_cell_ptr, prev_cell = head list in
    (* Content of cells are only even numbers so that searching odd numbers will always fail. *)
    let cell =
      next ~prev_cell ~prev_cell_ptr ((2 * list.size) + initial_value)
    in
    {size = list.size + 1; cells = (list.size, cell) :: list.cells}

  let back_path list start stop =
    back_path ~deref:(deref list) ~cell_ptr:start ~target_index:stop

  let search list start target_content =
    Lwt_main.run
      (search
         ~deref:(deref list)
         ~compare:(fun x -> Lwt.return Compare.Int.(compare x target_content))
         ~cell:start)

  let valid_back_path list start stop path =
    valid_back_path
      ~equal_ptr:( = )
      ~deref:(deref list)
      ~cell_ptr:start
      ~target_ptr:stop
      path

  let rec nlist basis n = if n = 0 then zero else succ (nlist basis (n - 1))

  let check_path i j back_path_fn =
    let l = nlist basis i in
    match back_path_fn l i j with
    | None -> fail (err (Printf.sprintf "There must be path from %d to %d" i j))
    | Some path ->
        let len = List.length path in
        let log_basis x =
          int_of_float @@ ceil (log (float_of_int x) /. log (float_of_int basis))
        in
        let log_ij = log_basis (i - j + 1) in
        let expected = max 1 (log_ij * basis) in
        fail_unless
          (len <= expected)
          (err
             (Format.sprintf
                "The proof is too long! Expected = %d < len = %d [basis = %d, \
                 i = %d, log = %d, j = %d]\n"
                expected
                len
                basis
                i
                log_ij
                j))
        >>=? fun () ->
        fail_unless
          (valid_back_path l i j path)
          (err
             (Printf.sprintf
                "The path %s does not connect %d to %d (or is \
                 invalid/non-minimal)"
                (show_path path)
                i
                j))

  let check_invalid_paths i =
    let l = nlist basis i in
    let rec aux j =
      if i <= j then return ()
      else
        (match back_path l j i with
        | None -> return ()
        | Some _path ->
            fail
              (err
                 (Printf.sprintf
                    "There should be no path connecting %d to %d"
                    j
                    i)))
        >>=? fun () -> aux (j + 1)
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
          Compare.Int.(content cell_x < target)
        then return ()
        else fail (err (Printf.sprintf "Invalid path: %d" target))
    | rev_path -> (
        (* The path is returned from the start cell to the target. The
           invariant we want to check is in the opposite direction. *)
        match rev_path with
        | cell_x :: cell_z :: _ -> (
            let i = index cell_x in
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
                then return ()
                else
                  fail (err (Printf.sprintf "Invariant for 'Lower' is broken")))
        | _ -> assert false)

  let check_invalid_search_paths i =
    let l = nlist basis i in
    let rec aux j =
      if i <= j then return ()
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
                | [] -> fail (err (Printf.sprintf "unexpected empty path"))
                | head :: _ ->
                    if Compare.Int.(content head > t) then return ()
                    else
                      fail
                        (err
                           (Printf.sprintf
                              "Invariant for 'No_exact_or_lower_ptr' broken")))
            | {last_cell = Nearest _; rev_path} ->
                (* In that case, we check the property of being a lower path. *)
                check_lower_path l rev_path t
            | {last_cell = Deref_returned_none; _} ->
                (* deref should always work *)
                assert false
            | {last_cell = Found _; _} ->
                (* Because we search for a cell that which is not in
                   the list, if the cell was found, we fail. *)
                fail
                  (err
                     (Printf.sprintf
                        "There should be no search path connecting %d to a \
                         node with content %d"
                        i
                        t)))
            >>=? fun () -> aux (j + 1)
    in
    aux 0

  let pp_search_result fmt =
    pp_search_result
      ~pp_cell:(fun fmt cell -> Format.fprintf fmt "%s" (show_cell cell))
      fmt
end

let test_skip_list_nat_check_path (basis, i, j) =
  let module M = TestNat (struct
    let basis = basis
  end) in
  M.check_path i j M.back_path

let test_skip_list_nat_check_invalid_path (basis, i) =
  let module M = TestNat (struct
    let basis = basis
  end) in
  M.check_invalid_paths i

let test_minimal_back_path () =
  let basis = 2 in
  let module M = TestNat (struct
    let basis = basis
  end) in
  let l = M.nlist basis 20 in
  let check_minimal_path = function
    | None, _ -> failwith "empty path"
    | Some path, expected_path ->
        if path = expected_path then return ()
        else
          failwith
            "non-minimal path:[%s] != expected_path:[%s]"
            (M.show_path path)
            (M.show_path expected_path)
  in
  let cases =
    [
      (6, 1, [6; 3; 1]);
      (6, 3, [6; 3]);
      (10, 3, [10; 7; 3]);
      (10, 5, [10; 7; 5]);
      (10, 7, [10; 7]);
      (10, 9, [10; 9]);
    ]
  in
  List.iter_es
    check_minimal_path
    (List.map
       (fun (start, target, expected_path) ->
         (M.back_path l start target, expected_path))
       cases)

let test_skip_list_nat_check_path_with_search (basis, i, j) =
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
      | _result -> None)

let test_skip_list_nat_check_invalid_path_with_search (basis, i) =
  let module M = TestNat (struct
    let basis = basis
  end) in
  M.check_invalid_search_paths i

let tests =
  [
    Tztest.tztest_qcheck2
      ~name:"Skip list: produce paths with `back_path` and check"
      ~count:10
      QCheck2.Gen.(
        let* basis = frequency [(5, pure 2); (1, 2 -- 73)] in
        let* i = 0 -- 100 in
        let* j = 0 -- i in
        return (basis, i, j))
      test_skip_list_nat_check_path;
    Tztest.tztest_qcheck2
      ~name:"Skip list: `back_path` won't produce invalid paths"
      ~count:10
      QCheck2.Gen.(
        let* basis = frequency [(5, pure 2); (1, 2 -- 73)] in
        let* i = 0 -- 100 in
        return (basis, i))
      test_skip_list_nat_check_invalid_path;
    Tztest.tztest
      "Skip list: check if the back_path is minimal"
      `Quick
      test_minimal_back_path;
    Tztest.tztest_qcheck2
      ~name:"Skip list: produce paths with `search` and check"
      ~count:10
      QCheck2.Gen.(
        let* basis = frequency [(5, pure 2); (1, 2 -- 73)] in
        let* i = 0 -- 100 in
        let* j = 0 -- i in
        return (basis, i, j))
      test_skip_list_nat_check_path_with_search;
    Tztest.tztest_qcheck2
      ~name:"Skip list: `search` won't produce invalid paths"
      ~count:10
      QCheck2.Gen.(
        let* basis = frequency [(5, pure 2); (1, 2 -- 73)] in
        let* i = 0 -- 10 in
        return (basis, i))
      test_skip_list_nat_check_invalid_path_with_search;
  ]
