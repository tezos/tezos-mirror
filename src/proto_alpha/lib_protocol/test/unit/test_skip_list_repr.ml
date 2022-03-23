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
    Invocation: dune exec src/proto_alpha/lib_protocol/test/unit/main.exe \
                -- test "^\[Unit\] skip list$"
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

  type t = {size : int; cells : (int * (unit, int) cell) list}

  let rec deref list i = List.assoc ~equal:Compare.Int.equal i list.cells

  and show_cell cell =
    Printf.sprintf
      "{ back_pointers = %s }"
      (back_pointers cell |> List.map string_of_int |> String.concat " ")

  and show_cells cells =
    String.concat
      "; "
      (List.map
         (fun (i, cell) -> Printf.sprintf "%d:%s" i (show_cell cell))
         cells)

  and show_list list =
    Printf.sprintf
      "basis: %d, size: %d, cells = %s"
      basis
      list.size
      (show_cells list.cells)

  let show_path path = String.concat " " (List.map string_of_int path)

  let head list =
    match List.hd list.cells with None -> assert false | Some h -> h

  let zero = {size = 1; cells = [(0, genesis ())]}

  let succ list =
    let (prev_cell_ptr, prev_cell) = head list in
    let cell = next ~prev_cell ~prev_cell_ptr () in
    {size = list.size + 1; cells = (list.size, cell) :: list.cells}

  let back_path list start stop =
    back_path ~deref:(deref list) ~cell_ptr:start ~target_index:stop

  let valid_back_path list start stop path =
    valid_back_path
      ~equal_ptr:( = )
      ~deref:(deref list)
      ~cell_ptr:start
      ~target_ptr:stop
      path

  let rec nlist basis n = if n = 0 then zero else succ (nlist basis (n - 1))

  let check_path i j =
    let l = nlist basis i in
    if i <= j then return ()
    else
      match back_path l i j with
      | None ->
          fail (err (Printf.sprintf "There must be path from %d to %d" i j))
      | Some path ->
          let len = List.length path in
          let log_basis x =
            int_of_float
            @@ ceil (log (float_of_int x) /. log (float_of_int basis))
          in
          let log_ij = log_basis (i - j + 1) in
          let expected = 2 + (log_ij * basis) in
          fail_unless
            (len <= expected)
            (err
               (Format.sprintf
                  "The proof is too long! Expected = %d < len = %d [basis = \
                   %d, i = %d, log = %d, j = %d]\n"
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
                  "The path %s does not connect %d to %d"
                  (show_path path)
                  i
                  j))

  let check_invalid_paths i =
    let l = nlist basis i in
    let rec aux j =
      if i <= j then return ()
      else
        match back_path l j i with
        | None -> return ()
        | Some _path ->
            fail
              (err
                 (Printf.sprintf
                    "There should be no path connecting %d to %d"
                    j
                    i))
            >>=? fun () -> aux (j + 1)
    in
    aux 0
end

let test_skip_list_nat_check_path (basis, i, j) =
  let basis = 2 + basis in
  let module M = TestNat (struct
    let basis = basis
  end) in
  M.check_path i j

let test_skip_list_nat_check_invalid_path (basis, i) =
  let basis = 2 + basis in
  let module M = TestNat (struct
    let basis = basis
  end) in
  M.check_invalid_paths i

let tests =
  [
    Tztest.tztest_qcheck
      ~name:"Skip list: check produced paths for multiple basis."
      ~count:10
      QCheck.(
        make
          Gen.(
            let* basis = 2 -- 73 in
            let* i = 0 -- 100 in
            let* j = 0 -- i in
            return (basis, i, j)))
      test_skip_list_nat_check_path;
    Tztest.tztest_qcheck
      ~name:"Skip list: reject invalid paths for multiple basis."
      ~count:10
      QCheck.(
        make
          Gen.(
            let* basis = 2 -- 73 in
            let* i = 0 -- 100 in
            return (basis, i)))
      test_skip_list_nat_check_invalid_path;
  ]
