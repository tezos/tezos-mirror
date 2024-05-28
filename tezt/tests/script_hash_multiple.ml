(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2020 Nomadic Labs <contact@nomadic-labs.com>                *)
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

(* Testing
   -------
   Component:    Client's hash script command
   Invocation:   dune exec tezt/tests/main.exe -- --file script_hash_multiple.ml
   Subject:      Tests various invocations of the [octez-client hash script]
*)

(* Test octez-client hash script with different number and type of
   arguments *)
let test_script_hash_multiple =
  Protocol.register_test
    ~__FILE__
    ~title:"Script hash multiple"
    ~tags:["script"; "hash"; "multiple"]
    ~uses_node:false
  @@ fun protocol ->
  let id_script_path =
    Michelson_script.(find ["attic"; "empty"] protocol |> path)
  in
  let id_script_literal = Base.read_file id_script_path in
  let id_script_hash =
    "expruat2BS4KCwn9kbopeX1ZwxtrtJbyFhpnpnG6A5KdCBCwHNsdod"
  in
  let* client = Client.init_mockup ~protocol () in
  Log.info "Contract hashes empty" ;
  let* hashes_empty = Client.hash_scripts [] client in
  Check.(
    (hashes_empty = [])
      (list string)
      ~__LOC__
      ~error_msg:"Expected an empty list, got %L") ;
  Log.info "Contract hashes single" ;
  let* hashes_single = Client.hash_scripts [id_script_literal] client in
  Check.(
    (hashes_single = [id_script_hash])
      (list string)
      ~__LOC__
      ~error_msg:"Expected single hash %R, got %L") ;
  Log.info "Contract hashes single display names" ;
  let* hashes_single =
    Client.hash_scripts ~display_names:true [id_script_literal] client
  in
  let expected_output_line ?(format = Client.TSV) ?name hash =
    match (format, name) with
    | CSV, Some name -> sf "%S,%S" hash name
    | CSV, None -> sf "%S" hash
    | TSV, Some name -> sf "%s\t%s" hash name
    | TSV, None -> hash
  in
  Check.(
    (hashes_single
    = [expected_output_line ~name:"Literal script 1" id_script_hash])
      (list string)
      ~__LOC__
      ~error_msg:"Expected %R, got %L") ;
  Log.info "Contract hashes mixed" ;
  let* hashes_mixed =
    Client.hash_scripts [id_script_path; id_script_literal] client
  in
  Check.(
    (hashes_mixed = [id_script_hash; id_script_hash])
      (list string)
      ~__LOC__
      ~error_msg:"Expected %R, got %L") ;
  let* hashes_mixed_named =
    Client.hash_scripts
      ~display_names:true
      [id_script_path; id_script_literal]
      client
  in
  Check.(
    (hashes_mixed_named
    = [
        expected_output_line ~name:id_script_path id_script_hash;
        expected_output_line ~name:"Literal script 1" id_script_hash;
      ])
      (list string)
      ~__LOC__
      ~error_msg:"Expected %R, got %L") ;
  Log.info "Contract hashes for script" ;
  let* () =
    Client.
      [
        ( CSV,
          true,
          expected_output_line
            ~format:CSV
            ~name:"Literal script 1"
            id_script_hash );
        (CSV, false, expected_output_line ~format:CSV id_script_hash);
        ( TSV,
          true,
          expected_output_line
            ~format:TSV
            ~name:"Literal script 1"
            id_script_hash );
        (TSV, false, expected_output_line ~format:TSV id_script_hash);
      ]
    |> Lwt_list.iter_s (fun (for_script, display_names, expected_results) ->
           let* hashes =
             Client.hash_scripts
               ~display_names
               ~for_script
               [id_script_literal]
               client
           in
           Check.(
             (hashes = [expected_results])
               (list string)
               ~__LOC__
               ~error_msg:"Expected %R, got %L") ;
           unit)
  in
  unit

let register ~protocols = test_script_hash_multiple protocols
