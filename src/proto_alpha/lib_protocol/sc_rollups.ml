(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Nomadic Labs <contact@nomadic-labs.com>                *)
(* Copyright (c) 2022 Trili Tech, <contact@trili.tech>                       *)
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

module PVM = struct
  type boot_sector = string

  module type S = sig
    val name : string

    val parse_boot_sector : string -> boot_sector option

    val pp_boot_sector : Format.formatter -> boot_sector -> unit

    include Sc_rollup_PVM_sig.S
  end

  type t = (module S)
end

module Kind = struct
  (*
      Each time we add a data constructor to [t], we also need:
      - to extend [Sc_rollups.all] with this new constructor ;
      - to update [Sc_rollups.of_name] and [encoding] ;
      - to update [Sc_rollups.wrapped_proof] and [wrapped_proof_encoding].

  *)
  type t = Example_arith | Wasm_2_0_0

  let encoding =
    Data_encoding.string_enum
      [("arith_pvm_kind", Example_arith); ("wasm_2_0_0_pvm_kind", Wasm_2_0_0)]

  let equal x y =
    match (x, y) with
    | Example_arith, Example_arith -> true
    | Wasm_2_0_0, Wasm_2_0_0 -> true
    | _ -> false

  let all = [Example_arith; Wasm_2_0_0]

  let of_name = function
    | "arith" -> Some Example_arith
    | "wasm_2_0_0" -> Some Wasm_2_0_0
    | _ -> None

  let example_arith_pvm =
    (module Sc_rollup_arith.Protocol_implementation : PVM.S)

  let wasm_2_0_0_pvm =
    (module Sc_rollup_wasm.V2_0_0.Protocol_implementation : PVM.S)

  let pvm_of = function
    | Example_arith -> example_arith_pvm
    | Wasm_2_0_0 -> wasm_2_0_0_pvm

  let of_pvm (module M : PVM.S) =
    match of_name M.name with
    | Some k -> k
    | None ->
        failwith
          (Format.sprintf
             "The module named %s is not in Sc_rollups.all."
             M.name)

  let pvm_of_name ~name = Option.map pvm_of (of_name name)

  let all_names =
    List.map
      (fun k ->
        let (module M : PVM.S) = pvm_of k in
        M.name)
      all

  let name_of k =
    let (module M) = pvm_of k in
    M.name

  let pp fmt k = Format.fprintf fmt "%s" (name_of k)
end

module type PVM_with_proof = sig
  include PVM.S

  val proof : proof
end

type wrapped_proof =
  | Unencodable of (module PVM_with_proof)
  | Arith_pvm_with_proof of
      (module PVM_with_proof
         with type proof = Sc_rollup_arith.Protocol_implementation.proof)
  | Wasm_2_0_0_pvm_with_proof of
      (module PVM_with_proof
         with type proof = Sc_rollup_wasm.V2_0_0.Protocol_implementation.proof)

let wrapped_proof_module p =
  match p with
  | Unencodable p -> p
  | Arith_pvm_with_proof (module P) -> (module P)
  | Wasm_2_0_0_pvm_with_proof (module P) -> (module P)

let wrapped_proof_kind_exn : wrapped_proof -> Kind.t = function
  | Unencodable _ ->
      raise (Invalid_argument "wrapped_proof_kind_exn: Unencodable")
  | Arith_pvm_with_proof _ -> Kind.Example_arith
  | Wasm_2_0_0_pvm_with_proof _ -> Kind.Wasm_2_0_0

(* TODO: #3704
   Change to an encoding that produces bytes
*)
let wrapped_proof_encoding =
  let open Data_encoding in
  let encoding =
    union
      ~tag_size:`Uint8
      [
        case
          ~title:"Arithmetic PVM with proof"
          (Tag 0)
          (obj2
             (req "kind" @@ constant "arith_pvm_kind")
             (req
                "proof"
                Sc_rollup_arith.Protocol_implementation.proof_encoding))
          (function
            | Arith_pvm_with_proof (module P) -> Some ((), P.proof) | _ -> None)
          (fun ((), proof) ->
            Arith_pvm_with_proof
              (module struct
                include Sc_rollup_arith.Protocol_implementation

                let proof = proof
              end));
        case
          ~title:"Wasm 2.0.0 PVM with proof"
          (Tag 1)
          (obj2
             (req "kind" @@ constant "wasm_2_0_0_pvm_kind")
             (req
                "proof"
                Sc_rollup_wasm.V2_0_0.Protocol_implementation.proof_encoding))
          (function
            | Wasm_2_0_0_pvm_with_proof (module P) -> Some ((), P.proof)
            | _ -> None)
          (fun ((), proof) ->
            Wasm_2_0_0_pvm_with_proof
              (module struct
                include Sc_rollup_wasm.V2_0_0.Protocol_implementation

                let proof = proof
              end));
        (* The later case is provided solely in order to provide error
           messages in case someone tries to encode an [Unencodable]
           proof. *)
        case
          ~title:"Unencodable"
          (Tag 255)
          empty
          (function
            | Unencodable (module P) ->
                raise
                  (Invalid_argument
                     Format.(
                       sprintf "cannot encode Unencodable (PVM %s)" P.name))
            | _ -> None)
          (fun () -> raise (Invalid_argument "cannot decode Unencodable"));
      ]
  in
  check_size Constants_repr.sc_max_wrapped_proof_binary_size encoding

let wrap_proof pvm_with_proof =
  let (module P : PVM_with_proof) = pvm_with_proof in
  match Kind.of_name P.name with
  | None -> Some (Unencodable pvm_with_proof)
  | Some Kind.Example_arith ->
      Option.map
        (fun arith_proof ->
          let module P_arith = struct
            include Sc_rollup_arith.Protocol_implementation

            let proof = arith_proof
          end in
          Arith_pvm_with_proof (module P_arith))
        (Option.bind
           (Data_encoding.Binary.to_bytes_opt P.proof_encoding P.proof)
           (fun bytes ->
             Data_encoding.Binary.of_bytes_opt
               Sc_rollup_arith.Protocol_implementation.proof_encoding
               bytes))
  | Some Kind.Wasm_2_0_0 ->
      Option.map
        (fun wasm_proof ->
          let module P_wasm2_0_0 = struct
            include Sc_rollup_wasm.V2_0_0.Protocol_implementation

            let proof = wasm_proof
          end in
          Wasm_2_0_0_pvm_with_proof (module P_wasm2_0_0))
        (Option.bind
           (Data_encoding.Binary.to_bytes_opt P.proof_encoding P.proof)
           (fun bytes ->
             Data_encoding.Binary.of_bytes_opt
               Sc_rollup_wasm.V2_0_0.Protocol_implementation.proof_encoding
               bytes))
