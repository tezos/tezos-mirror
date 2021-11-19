(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2019-2020 Nomadic Labs <contact@nomadic-labs.com>           *)
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

open Alpha_context

let custom_root =
  (RPC_path.(open_root / "context" / "sapling")
    : RPC_context.t RPC_path.context)

type diff_query = {
  offset_commitment : Int64.t option;
  offset_nullifier : Int64.t option;
}

module S = struct
  module Args = struct
    type ('query_type, 'output_type) t = {
      name : string;
      description : string;
      query : 'query_type RPC_query.t;
      output : 'output_type Data_encoding.t;
      f : context -> Sapling.Id.t -> 'query_type -> 'output_type tzresult Lwt.t;
    }

    let get_diff_query : diff_query RPC_query.t =
      let open RPC_query in
      query (fun offset_commitment offset_nullifier ->
          {offset_commitment; offset_nullifier})
      |+ opt_field
           ~descr:
             "Commitments and ciphertexts are returned from the specified \
              offset up to the most recent."
           "offset_commitment"
           RPC_arg.uint63
           (fun {offset_commitment; _} -> offset_commitment)
      |+ opt_field
           ~descr:
             "Nullifiers are returned from the specified offset up to the most \
              recent."
           "offset_nullifier"
           RPC_arg.uint63
           (fun {offset_nullifier; _} -> offset_nullifier)
      |> seal

    let encoding =
      let open Data_encoding in
      merge_objs (obj1 (req "root" Sapling.root_encoding)) Sapling.diff_encoding

    let get_diff =
      {
        name = "get_diff";
        description =
          "Returns the root and a diff of a state starting from an optional \
           offset which is zero by default.";
        query = get_diff_query;
        output = encoding;
        f =
          (fun ctxt id {offset_commitment; offset_nullifier} ->
            Sapling.get_diff ctxt id ?offset_commitment ?offset_nullifier ());
      }
  end

  let make_service Args.{name; description; query; output; f} =
    let path = RPC_path.(custom_root /: Sapling.rpc_arg / name) in
    let service = RPC_service.get_service ~description ~query ~output path in
    (service, fun ctxt id q () -> f ctxt id q)

  let get_diff = make_service Args.get_diff
end

let register () =
  let reg ~chunked (service, f) =
    Services_registration.register1 ~chunked service f
  in
  reg ~chunked:false S.get_diff

let mk_call1 (service, _f) ctxt block id q =
  RPC_context.make_call1 service ctxt block id q ()

let get_diff ctxt block id ?offset_commitment ?offset_nullifier () =
  mk_call1 S.get_diff ctxt block id {offset_commitment; offset_nullifier}
