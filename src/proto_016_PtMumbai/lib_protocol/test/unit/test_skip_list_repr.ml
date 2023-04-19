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
    Invocation: dune exec src/proto_016_PtMumbai/lib_protocol/test/unit/main.exe \
                  -- --file test_skip_list_repr.ml
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
    back_path ~deref:(deref list) ~cell_ptr:start ~target_index:(Z.of_int stop)

  let find list start stop =
    find ~deref:(deref list) ~cell_ptr:start ~target_index:(Z.of_int stop)

  let search list start target_content =
    search
      ~deref:(deref list)
      ~compare:(fun x -> Compare.Int.(compare x target_content))
      ~cell:start

  let valid_back_path list start stop path =
    valid_back_path
      ~equal_ptr:( = )
      ~deref:(deref list)
      ~cell_ptr:start
      ~target_ptr:stop
      path

  let rec nlist basis n = if n = 0 then zero else succ (nlist basis (n - 1))

  let check_find i j =
    let open Lwt_result_syntax in
    let l = nlist basis i in
    let*? () =
      match find l i j with
      | None -> error (err (Printf.sprintf "There must be a cell (%d)" i))
      | Some cell ->
          let index = Z.to_int (index cell) in
          error_unless
            (index = j)
            (err
               (Printf.sprintf
                  "Found cell is not the correct one (found %d, expected %d)"
                  index
                  j))
    in
    let*? path =
      match back_path l i j with
      | None ->
          error (err (Printf.sprintf "There must be path from %d to %d" i j))
      | Some path -> ok path
    in
    let*? () =
      match List.(hd (rev path)) with
      | None ->
          error
            (err
               (Printf.sprintf
                  " There can't be an empty path from %d to %d"
                  i
                  j))
      | Some stop_cell ->
          error_unless
            (j = stop_cell)
            (err
               (Printf.sprintf
                  "Found cell is not equal to stop cell of back path (%d to %d)"
                  i
                  j))
    in
    return_unit

  let check_invalid_find i =
    let open Lwt_result_syntax in
    let l = nlist basis i in
    let check_nothing_found i j =
      match find l i j with
      | None -> ok ()
      | Some _v ->
          error
            (err
               (Printf.sprintf
                  "There should be no value found at %d from %d"
                  i
                  j))
    in
    let*? () = check_nothing_found i (-1) in
    let rec aux j =
      if i <= j then return_unit
      else
        let*? () = check_nothing_found j i in
        aux (j + 1)
    in
    aux 0

  let check_path i j back_path_fn =
    let open Lwt_result_syntax in
    let l = nlist basis i in
    let*! path = back_path_fn l i j in
    match path with
    | None ->
        tzfail (err (Printf.sprintf "There must be path from %d to %d" i j))
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
                then return ()
                else
                  fail (err (Printf.sprintf "Invariant for 'Lower' is broken")))
        | _ -> assert false)

  let check_invalid_search_paths i =
    let open Lwt_result_syntax in
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
                | [] -> tzfail (err (Printf.sprintf "unexpected empty path"))
                | head :: _ ->
                    if Compare.Int.(content head > t) then return ()
                    else
                      tzfail
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
                tzfail
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
  let back_path list start stop = Lwt.return (M.back_path list start stop) in
  M.check_path i j back_path

let test_skip_list_nat_check_find (basis, i, j) =
  let module M = TestNat (struct
    let basis = basis
  end) in
  M.check_find i j

let test_skip_list_nat_check_invalid_find (basis, i) =
  let module M = TestNat (struct
    let basis = basis
  end) in
  M.check_invalid_find i

let test_skip_list_nat_check_invalid_path (basis, i) =
  let module M = TestNat (struct
    let basis = basis
  end) in
  M.check_invalid_paths i

let test_minimal_back_path () =
  let basis = 4 in
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
      (6, 1, [6; 3; 2; 1]);
      (6, 3, [6; 3]);
      (10, 3, [10; 7; 3]);
      (10, 5, [10; 7; 6; 5]);
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

let test_search_non_minimal_back_path () =
  let open Lwt_result_syntax in
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
  match M.search l start target with
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
                      failwith
                        "The path returned is minimal while it should not be \
                         the case."
                    else return ()
              else (* By specification of the function [search]. *)
                assert false))
  | _ ->
      (* The cell does not exist in the list. *)
      assert false

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
          |> Lwt.return_some
      | _result -> Lwt.return_none)

let test_skip_list_nat_check_invalid_path_with_search (basis, i) =
  let module M = TestNat (struct
    let basis = basis
  end) in
  M.check_invalid_search_paths i

(*

   In this test, we check that [best_basis] should be used to optimize
   the size of Merkel proofs based on skip lists. In such a context,
   the skip lists are referenced by Blake2B hashes (32 bytes-long) and
   their contents are also Blake2B hashes. Besides, a Merkel proof is
   a list of cells.

   To that end, we consider a deterministic sample of pairs [(n,
   target)] when [n] is the length of the skip list and [target] is
   the index of a cell in the skip list. Then, for each basis in [2
   .. max_basis] distinct from [best_basis], we check that the largest
   proof is larger than the largest proof of [best_basis].

*)
let test_skip_list_proof_size () =
  let module H = Sc_rollup_inbox_merkelized_payload_hashes_repr.Hash in
  (*

     Basis [4] is very close to [3] as the best basis... therefore, we
     use a fixed seed for the random number generator to avoid any
     flakiness in the test.

  *)
  let () = Random.init 0xC0FFEE in
  let best_basis = 4 in

  (*

     For the CI, we use relatively small values to avoid slowdowns.

     We choose [max_length] to be of the same order of magnitude as
     the longest lists we can meet in practice in the smart rollups
     inboxes.

     The real [max_length] can be found in [constants_repr.ml]. At the time
     of writing this message, the value is [1_000_000].

  *)
  let max_basis = 7 and nsample = 2048 and max_length = 100_000 in

  (*

     Locally, one can we use large values to get higher confidence:

   *)
  (* let max_basis = 13 and nsample = 4096 and max_length = 200_000 in *)

  (* A sample is a pair [(n, k)] where [k <= n]. *)
  let samples =
    let get () =
      let n = 1 + Random.int (max_length - 1) in
      let target = Random.int (1 + n) in
      (n, Z.of_int target)
    in
    let rec aux r n = if n = 0 then r else aux (get () :: r) (n - 1) in
    aux [] nsample
  in

  (*

     For a given [basis], we compute the largest proof when processing
     [samples]. The considered lists hold the same contents in each cell,
     allowing us to store lists of size [k] in a cache.

  *)
  let largest_proof basis =
    let module M = Skip_list_repr.Make (struct
      let basis = basis
    end) in
    let cell_encoding = M.encoding H.encoding H.encoding in
    let proof_encoding = Data_encoding.list cell_encoding in
    let hash_cell cell =
      let payload_hash = M.content cell in
      let back_pointers_hashes = M.back_pointers cell in
      H.to_bytes payload_hash :: List.map H.to_bytes back_pointers_hashes
      |> H.hash_bytes
    in
    let dummy_content = H.hash_string ["HumptyDumpty"] in
    let cache =
      let cache = Stdlib.Hashtbl.create 13 in
      let rec make_list k n map prev_cell =
        if n = k then
          let prev_cell_ptr = hash_cell prev_cell in
          let map = H.Map.add prev_cell_ptr prev_cell map in
          Stdlib.Hashtbl.add cache k (map, prev_cell_ptr)
        else
          let prev_cell_ptr = hash_cell prev_cell in
          let next_cell = M.next ~prev_cell ~prev_cell_ptr dummy_content in
          let map = H.Map.add prev_cell_ptr prev_cell map in
          Stdlib.Hashtbl.add cache k (map, prev_cell_ptr) ;
          make_list (succ k) n map next_cell
      in
      make_list 0 max_length H.Map.empty (M.genesis dummy_content) ;
      cache
    in
    let proof_of_path deref =
      List.map (fun ptr -> Stdlib.Option.get (deref ptr))
    in
    let proof_size (n, target_index) =
      let make_list n = Stdlib.Hashtbl.find cache n in
      let map, cell_ptr = make_list n in
      let deref ptr = H.Map.find ptr map in
      let path =
        Stdlib.Option.get @@ M.back_path ~deref ~cell_ptr ~target_index
      in
      let proof = proof_of_path deref path in
      let encoded_proof =
        Data_encoding.Binary.to_bytes_exn proof_encoding proof
      in
      Bytes.length encoded_proof
    in
    List.map proof_size samples |> List.fold_left max min_int
  in
  let largest_proofs =
    List.map (fun basis -> (basis, largest_proof basis)) (2 -- max_basis)
  in
  let () =
    List.iter
      (fun (b, p) ->
        Format.eprintf "@[Basis = %d,@, Largest proof = %d@]@;" b p)
      largest_proofs
  in
  let smallest_largest_proofs_basis, _ =
    List.fold_left
      (fun (b1, p1) (b2, p2) -> if p1 < p2 then (b1, p1) else (b2, p2))
      (Stdlib.List.hd largest_proofs)
      (Stdlib.List.tl largest_proofs)
  in
  fail_unless
    (smallest_largest_proofs_basis = best_basis)
    (err
       (Format.asprintf
          "According to the test, %d is the best basis, not %d."
          smallest_largest_proofs_basis
          best_basis))

let tests =
  [
    Tztest.tztest_qcheck2
      ~name:"Skip list: produce paths with `back_path` and check"
      ~count:10
      QCheck2.Gen.(
        let* basis = frequency [(5, pure 4); (1, 2 -- 73)] in
        let* i = 0 -- 100 in
        let* j = 0 -- i in
        return (basis, i, j))
      test_skip_list_nat_check_path;
    Tztest.tztest_qcheck2
      ~name:"Skip list: find cell with `find` and `check`"
      ~count:10
      QCheck2.Gen.(
        let* basis = frequency [(5, pure 4); (1, 2 -- 73)] in
        let* i = 0 -- 100 in
        let* j = 0 -- i in
        return (basis, i, j))
      test_skip_list_nat_check_find;
    Tztest.tztest_qcheck2
      ~name:"Skip list: `find` won't produce invalid value"
      ~count:10
      QCheck2.Gen.(
        let* basis = frequency [(5, pure 4); (1, 2 -- 73)] in
        let* i = 0 -- 100 in
        return (basis, i))
      test_skip_list_nat_check_invalid_find;
    Tztest.tztest_qcheck2
      ~name:"Skip list: `back_path` won't produce invalid paths"
      ~count:10
      QCheck2.Gen.(
        let* basis = frequency [(5, pure 4); (1, 2 -- 73)] in
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
        let* basis = frequency [(5, pure 4); (1, 2 -- 73)] in
        let* i = 0 -- 100 in
        let* j = 0 -- i in
        return (basis, i, j))
      test_skip_list_nat_check_path_with_search;
    Tztest.tztest_qcheck2
      ~name:"Skip list: `search` won't produce invalid paths"
      ~count:10
      QCheck2.Gen.(
        let* basis = frequency [(5, pure 4); (1, 2 -- 73)] in
        let* i = 0 -- 10 in
        return (basis, i))
      test_skip_list_nat_check_invalid_path_with_search;
    (* We cheat here to avoid mixing non-pbt tests with pbt tests. *)
    Tztest.tztest_qcheck2
      ~name:"Skip list: `search` may not produce minimal path"
      ~count:10
      QCheck2.Gen.unit
      test_search_non_minimal_back_path;
    Tztest.tztest
      "Skip list: check if the best basis for merkelized skip list is indeed \
       the best"
      `Quick
      test_skip_list_proof_size;
  ]

let () =
  Alcotest_lwt.run ~__FILE__ Protocol.name [("skip list", tests)]
  |> Lwt_main.run
