(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

(* open Tezos_rewriting *)
open Mikhailsky

let unopt x = match x with Some x -> x | None -> assert false

let time f =
  let now = Unix.gettimeofday () in
  let res = f () in
  let later = Unix.gettimeofday () in
  (later -. now, res)

let add_ii = Instructions.(add Mikhailsky.int_ty Mikhailsky.int_ty)

let add_in = Instructions.(add Mikhailsky.int_ty Mikhailsky.nat_ty)

let mul_ii = Instructions.(mul Mikhailsky.int_ty Mikhailsky.int_ty)

let push_int = Instructions.push int_ty (Data.big_integer (Z.of_int 100))

let push_nat = Instructions.push nat_ty (Data.big_natural (Z.of_int 100))

module Test1 = struct
  open Data
  open Instructions

  let program = seq [add_ii; push bool_ty false_; dip instr_hole; dip swap]

  let timing, (bef, aft) = time @@ fun () -> Inference.infer program

  let _ =
    Format.printf "Testing type inference\n" ;
    Format.printf "EXPECTING SUCCESS\n" ;
    Format.printf "Program\n" ;
    Format.printf "%a\n" Mikhailsky.pp program ;
    Format.printf "In %f seconds:\n" timing ;
    Format.printf "bef: %a@." Type.Stack.pp bef ;
    Format.printf "aft: %a@." Type.Stack.pp aft ;
    print_newline ()
end

module Test2 = struct
  open Instructions

  let program = seq [loop swap; and_]

  let () =
    Format.printf "Testing type inference\n" ;
    Format.printf "EXPECTING FAILURE\n" ;
    Format.printf "Program: %a\n" Mikhailsky.pp program ;
    let exception Test_failed in
    try
      ignore
        ( time @@ fun () ->
          ignore @@ Inference.infer program ;
          raise Test_failed )
    with
    | Inference.Ill_typed_script error ->
        Format.printf "Error:\n" ;
        Format.printf "%a\n" Inference.pp_inference_error error
    | Test_failed -> Format.printf "No type error: Test failed!"

  let _ = print_newline ()
end

module Test3 = struct
  open Instructions

  let program =
    seq
      [
        dip (seq [swap; dup]);
        swap;
        dip cdr;
        loop (seq [dip instr_hole; cdr; loop instr_hole]);
        car;
        car;
        push int_ty (Data.integer 10);
        compare;
      ]

  let _ =
    Format.printf "Testing rewriting and type inference\n" ;
    Format.printf "Source program: %a\n" Mikhailsky.pp program

  open Tezos_micheline_rewriting

  module Lang =
    Micheline_with_hash_consing.Make
      (Mikhailsky.Mikhailsky_signature)
      (struct
        let initial_size = None
      end)

  module Path = Mikhailsky.Path
  module Patt = Pattern.Make (Mikhailsky.Mikhailsky_signature) (Lang) (Path)
  module Rewriter =
    Rewrite.Make (Mikhailsky.Mikhailsky_signature) (Lang) (Path) (Patt)

  let timing, ((bef, aft), state) =
    try time @@ fun () -> Inference.infer_with_state program
    with Inference.Ill_typed_script error ->
      let s = Mikhailsky.to_string program in
      Format.printf
        "Ill-typed script:%a\n%s\n"
        Inference.pp_inference_error
        error
        s ;
      Format.printf "Test failed\n" ;
      exit 1

  let () =
    Format.printf "In %f seconds:\n" timing ;
    Format.printf "bef: %a@." Type.Stack.pp bef ;
    Format.printf "aft: %a@." Type.Stack.pp aft

  let () =
    try
      ignore
        ((let open Inference in
         let open M in
         M.uf_lift Uf.UF.show >>= fun uf_state ->
         Inference.M.repr_lift (fun s -> (Inference.Repr_store.to_string s, s))
         >>= fun repr_state ->
         Printf.printf "uf_state:\n%s\n" uf_state ;
         Printf.printf "repr_state:\n%s\n" repr_state ;
         let path =
           Path.(at_index 2 (at_index 0 (at_index 0 (at_index 3 root))))
         in
         let subterm = Rewriter.get_subterm ~term:program ~path in
         Format.printf
           "subterm at path %s:\n%a\n"
           (Path.to_string path)
           Mikhailsky.pp
           subterm ;
         Inference.M.annot_instr_lift (Inference.Annot_instr_sm.get path)
         >>= fun typ ->
         (match typ with
         | None -> assert false
         | Some {bef; aft} ->
             Inference.instantiate bef >>= fun bef ->
             Inference.instantiate aft >>= fun aft ->
             Format.printf "Type of subterm:\n" ;
             Format.printf "bef: %a@." Type.Stack.pp bef ;
             Format.printf "aft: %a@." Type.Stack.pp aft ;
             return ())
         >>= fun () -> return ())
           state)
    with Inference.Ill_typed_script error ->
      let s = Mikhailsky.to_string program in
      Format.printf
        "Ill-typed script:\n%a\n%s\n"
        Inference.pp_inference_error
        error
        s

  let _ = print_newline ()
end

module Test4 = struct
  open Instructions

  let program =
    seq
      [
        empty_set;
        push Type.(unopt (unparse_ty bool)) Data.true_;
        push
          Type.(unopt (unparse_ty (pair int int)))
          Data.(pair (integer 0) (integer 0));
        update_set;
      ]

  let timing, (bef, aft) = time @@ fun () -> Inference.infer program

  let _ =
    Format.printf "Testing type inference\n" ;
    Format.printf "EXPECTING SUCCESS\n" ;
    Format.printf "Program\n" ;
    Format.printf "%a\n" Mikhailsky.pp program ;
    Format.printf "In %f seconds:\n" timing ;
    Format.printf "bef: %a@." Type.Stack.pp bef ;
    Format.printf "aft: %a@." Type.Stack.pp aft ;
    print_newline ()
end

module Test5 = struct
  open Instructions

  let unopt x = match x with Some x -> x | None -> assert false

  let program =
    seq
      [
        empty_map;
        push Type.(unopt (unparse_ty (option (set int)))) Data.none;
        push
          Type.(unopt (unparse_ty (pair int int)))
          Data.(pair (integer 0) (integer 0));
        update_map;
      ]

  let timing, (bef, aft) = time @@ fun () -> Inference.infer program

  let _ =
    Format.printf "Testing type inference\n" ;
    Format.printf "EXPECTING SUCCESS\n" ;
    Format.printf "Program\n" ;
    Format.printf "%a\n" Mikhailsky.pp program ;
    Format.printf "In %f seconds:\n" timing ;
    Format.printf "bef: %a@." Type.Stack.pp bef ;
    Format.printf "aft: %a@." Type.Stack.pp aft ;
    print_newline ()

  let program =
    seq
      [
        push
          Type.(unopt (unparse_ty (map (pair int int) (set int))))
          Data.(
            map
              [
                map_elt
                  (pair (integer 0) (integer 1))
                  (set [integer 42; integer 44]);
                map_elt
                  (pair (integer 1) (integer 2))
                  (set [integer 42; integer 48]);
              ]);
      ]

  let timing, (bef, aft) = time @@ fun () -> Inference.infer program

  let _ =
    Format.printf "Testing type inference\n" ;
    Format.printf "EXPECTING SUCCESS\n" ;
    Format.printf "Program\n" ;
    Format.printf "%a\n" Mikhailsky.pp program ;
    Format.printf "In %f seconds:\n" timing ;
    Format.printf "bef: %a@." Type.Stack.pp bef ;
    Format.printf "aft: %a@." Type.Stack.pp aft ;
    print_newline ()
end

module Test6 = struct
  open Instructions

  (* We remove a chunk from a well-typed program to make it ill-typed, and
     expect the type inference to fail *)
  let program =
    seq
      [
        push int_ty (Data.integer 0);
        push int_ty (Data.integer 100);
        swap;
        drop;
        drop;
        drop;
        push unit_ty Data.unit;
        push bool_ty Data.false_;
        push unit_ty Data.unit;
        push int_ty (Data.integer 4073851221413541140);
        push string_ty (string "n");
        push string_ty (string "k");
        push int_ty (Data.integer 1391989767887046289);
        (* push int_ty (integer 100);
         * abs;
         * drop; *)
        dip (prim I_CONCAT [] []);
        compare;
      ]

  let () =
    Format.printf "Testing type inference\n" ;
    Format.printf "EXPECTING FAILURE\n" ;
    Format.printf "Program: %a\n" Mikhailsky.pp program ;
    let exception Test_failed in
    try
      ignore (Inference.infer program) ;
      raise Test_failed
    with
    | Inference.Ill_typed_script error ->
        Format.printf "Got error, as expected:\n" ;
        Format.printf "%a@." Inference.pp_inference_error error
    | Test_failed ->
        Format.printf "No type error: Test failed!" ;
        exit 1
end

module Test7 = struct
  open Instructions

  let program =
    seq
      [
        push int_ty (Data.integer 42);
        left;
        push string_ty (Data.string "forty-two");
        right;
        pair;
        left;
      ]

  let timing, (bef, aft) = time @@ fun () -> Inference.infer program

  let _ =
    Format.printf "Testing type inference\n" ;
    Format.printf "EXPECTING SUCCESS\n" ;
    Format.printf "Program\n" ;
    Format.printf "%a\n" Mikhailsky.pp program ;
    Format.printf "In %f seconds:\n" timing ;
    Format.printf "bef: %a@." Type.Stack.pp bef ;
    Format.printf "aft: %a@." Type.Stack.pp aft ;
    print_newline ()
end

module Test8 = struct
  open Instructions

  let program =
    seq
      [
        hole;
        add_ii;
        push int_ty (Data.big_integer (Z.of_int 100));
        abs;
        right;
        dup;
        push int_ty (Data.big_integer (Z.of_int 100));
        dip (loop_left hole);
        push_int;
        hole;
        mul_ii;
        hole;
        loop_left left;
        sha512;
        push_int;
        dup;
        add_ii;
        right;
        swap;
        hole;
        drop;
        compare;
        mul_ii;
        push_int;
      ]

  let timing, (bef, aft) = time @@ fun () -> Inference.infer program

  let _ =
    Format.printf "Testing type inference\n" ;
    Format.printf "EXPECTING SUCCESS\n" ;
    Format.printf "Program\n" ;
    Format.printf "%a\n" Mikhailsky.pp program ;
    Format.printf "In %f seconds:\n" timing ;
    Format.printf "bef: %a@." Type.Stack.pp bef ;
    Format.printf "aft: %a@." Type.Stack.pp aft ;
    print_newline ()
end

module Test9 = struct
  open Instructions

  let program = seq [car; if_none hole hole]

  let timing, (bef, aft) = time @@ fun () -> Inference.infer program

  let _ =
    Format.printf "Testing type inference\n" ;
    Format.printf "EXPECTING SUCCESS\n" ;
    Format.printf "Program\n" ;
    Format.printf "%a\n" Mikhailsky.pp program ;
    Format.printf "In %f seconds:\n" timing ;
    Format.printf "bef: %a@." Type.Stack.pp bef ;
    Format.printf "aft: %a@." Type.Stack.pp aft ;
    print_newline ()
end

module Test10 = struct
  open Instructions

  let program = seq [hash_key]

  let timing, (bef, aft) = time @@ fun () -> Inference.infer program

  let _ =
    Format.printf "Testing type inference\n" ;
    Format.printf "EXPECTING SUCCESS\n" ;
    Format.printf "Program\n" ;
    Format.printf "%a\n" Mikhailsky.pp program ;
    Format.printf "In %f seconds:\n" timing ;
    Format.printf "bef: %a@." Type.Stack.pp bef ;
    Format.printf "aft: %a@." Type.Stack.pp aft ;
    print_newline ()
end

module Test11 = struct
  open Instructions

  let program =
    seq [lambda [dup; car; dip cdr; add_in]; push_int; apply; push_nat; exec]

  let timing, (bef, aft) = time @@ fun () -> Inference.infer program

  let _ =
    Format.printf "Testing type inference\n" ;
    Format.printf "EXPECTING SUCCESS\n" ;
    Format.printf "Program\n" ;
    Format.printf "%a\n" Mikhailsky.pp program ;
    Format.printf "In %f seconds:\n" timing ;
    Format.printf "bef: %a@." Type.Stack.pp bef ;
    Format.printf "aft: %a@." Type.Stack.pp aft ;
    print_newline ()
end

module Test12 = struct
  open Instructions

  let program = seq [dup; dup; if_none hole (seq [drop]); dup; compare]

  let timing, (bef, aft) = time @@ fun () -> Inference.infer program

  let _ =
    Format.printf "Testing type inference\n" ;
    Format.printf "EXPECTING SUCCESS\n" ;
    Format.printf "Program\n" ;
    Format.printf "%a\n" Mikhailsky.pp program ;
    Format.printf "In %f seconds:\n" timing ;
    Format.printf "bef: %a@." Type.Stack.pp bef ;
    Format.printf "aft: %a@." Type.Stack.pp aft ;
    print_newline ()
end

module Test13 = struct
  open Instructions

  let program =
    seq [push Type.(unparse_ty_exn (lambda int int)) (Data.lambda [])]

  let timing, (bef, aft) = time @@ fun () -> Inference.infer program

  let _ =
    Format.printf "Testing type inference\n" ;
    Format.printf "EXPECTING SUCCESS\n" ;
    Format.printf "Program\n" ;
    Format.printf "%a\n" Mikhailsky.pp program ;
    Format.printf "In %f seconds:\n" timing ;
    Format.printf "bef: %a@." Type.Stack.pp bef ;
    Format.printf "aft: %a@." Type.Stack.pp aft ;
    print_newline ()
end

module Test14 = struct
  open Instructions

  let program = seq [nil; push_int; cons]

  let timing, (bef, aft) = time @@ fun () -> Inference.infer program

  let _ =
    Format.printf "Testing type inference\n" ;
    Format.printf "EXPECTING SUCCESS\n" ;
    Format.printf "Program\n" ;
    Format.printf "%a\n" Mikhailsky.pp program ;
    Format.printf "In %f seconds:\n" timing ;
    Format.printf "bef: %a@." Type.Stack.pp bef ;
    Format.printf "aft: %a@." Type.Stack.pp aft ;
    print_newline ()
end

module Test15 = struct
  open Instructions

  let program = seq [empty_set; size_set; empty_map; size_map; nil; size_list]

  let timing, (bef, aft) = time @@ fun () -> Inference.infer program

  let _ =
    Format.printf "Testing type inference\n" ;
    Format.printf "EXPECTING SUCCESS\n" ;
    Format.printf "Program\n" ;
    Format.printf "%a\n" Mikhailsky.pp program ;
    Format.printf "In %f seconds:\n" timing ;
    Format.printf "bef: %a@." Type.Stack.pp bef ;
    Format.printf "aft: %a@." Type.Stack.pp aft ;
    print_newline ()
end

module Test16 = struct
  open Instructions

  let program =
    seq
      [
        empty_set;
        push bool_ty Data.true_;
        push_int;
        update_set;
        iter_set [dup; add_ii; add_ii];
      ]

  let timing, (bef, aft) = time @@ fun () -> Inference.infer program

  let _ =
    Format.printf "Testing type inference\n" ;
    Format.printf "EXPECTING SUCCESS\n" ;
    Format.printf "Program\n" ;
    Format.printf "%a\n" Mikhailsky.pp program ;
    Format.printf "In %f seconds:\n" timing ;
    Format.printf "bef: %a@." Type.Stack.pp bef ;
    Format.printf "aft: %a@." Type.Stack.pp aft ;
    print_newline ()
end

module Test17 = struct
  open Instructions

  let program =
    seq
      [
        empty_map;
        push (option_ty (list_ty bool_ty)) Data.(some (list [false_; true_]));
        push_int;
        update_map;
        map_map
          [
            cdr;
            map_list
              [
                if_
                  (seq [push bool_ty Data.false_])
                  (seq [push bool_ty Data.true_]);
              ];
          ];
      ]

  let timing, (bef, aft) = time @@ fun () -> Inference.infer program

  let _ =
    Format.printf "Testing type inference\n" ;
    Format.printf "EXPECTING SUCCESS\n" ;
    Format.printf "Program\n" ;
    Format.printf "%a\n" Mikhailsky.pp program ;
    Format.printf "In %f seconds:\n" timing ;
    Format.printf "bef: %a@." Type.Stack.pp bef ;
    Format.printf "aft: %a@." Type.Stack.pp aft ;
    print_newline ()
end

module Test18 = struct
  open Instructions

  let program =
    seq
      [
        empty_map;
        push (option_ty (list_ty bool_ty)) Data.(some (list [false_; true_]));
        push_int;
        update_map;
        map_map
          [
            cdr;
            map_list
              [
                if_
                  (seq [push bool_ty Data.false_])
                  (seq [push bool_ty Data.true_]);
              ];
          ];
        dup;
        dip push_int;
        push_int;
        mem_map;
        if_
          (seq [get_map])
          (seq [drop; drop; push (option_ty (list_ty bool_ty)) Data.none]);
      ]

  let timing, (bef, aft) = time @@ fun () -> Inference.infer program

  let _ =
    Format.printf "Testing type inference\n" ;
    Format.printf "EXPECTING SUCCESS\n" ;
    Format.printf "Program\n" ;
    Format.printf "%a\n" Mikhailsky.pp program ;
    Format.printf "In %f seconds:\n" timing ;
    Format.printf "bef: %a@." Type.Stack.pp bef ;
    Format.printf "aft: %a@." Type.Stack.pp aft ;
    print_newline ()
end
