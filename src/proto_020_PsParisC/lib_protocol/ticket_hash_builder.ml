(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Trili Tech, <contact@trili.tech>                       *)
(* Copyright (c) 2022 Oxhead Alpha <info@oxhead-alpha.com>                   *)
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

type error += Failed_to_hash_node

let () =
  register_error_kind
    `Branch
    ~id:"Failed_to_hash_node"
    ~title:"Failed to hash node"
    ~description:"Failed to hash node for a key in the ticket-balance table"
    ~pp:(fun ppf () ->
      Format.fprintf
        ppf
        "Failed to hash node for a key in the ticket-balance table")
    Data_encoding.empty
    (function Failed_to_hash_node -> Some () | _ -> None)
    (fun () -> Failed_to_hash_node)

(* No model is given. The original definition was a copy of
   Global_constants_costs.expr_to_address_in_context_cost.
*)
let hash_bytes_cost = Global_constants_costs.expr_to_address_in_context_cost

let hash_of_node ctxt node =
  let open Result_syntax in
  let* ctxt =
    Raw_context.consume_gas ctxt (Script_repr.strip_locations_cost node)
  in
  let node = Micheline.strip_locations node in
  let* bytes =
    Result.of_option
      ~error:(Error_monad.trace_of_error Failed_to_hash_node)
      (Data_encoding.Binary.to_bytes_opt Script_repr.expr_encoding node)
  in
  let+ ctxt = Raw_context.consume_gas ctxt (hash_bytes_cost bytes) in
  ( Ticket_hash_repr.of_script_expr_hash @@ Script_expr_hash.hash_bytes [bytes],
    ctxt )

let hash_of_node_uncarbonated node =
  let open Result_syntax in
  let node = Micheline.strip_locations node in
  let+ bytes =
    Result.of_option
      ~error:(Error_monad.trace_of_error Failed_to_hash_node)
      (Data_encoding.Binary.to_bytes_opt Script_repr.expr_encoding node)
  in
  Ticket_hash_repr.of_script_expr_hash @@ Script_expr_hash.hash_bytes [bytes]

let make ctxt ~ticketer ~ty ~contents ~owner =
  hash_of_node ctxt
  @@ Micheline.Seq (Micheline.dummy_location, [ticketer; ty; contents; owner])

let make_uncarbonated ~ticketer ~ty ~contents ~owner =
  hash_of_node_uncarbonated
  @@ Micheline.Seq (Micheline.dummy_location, [ticketer; ty; contents; owner])

module Internal_for_tests = struct
  let make_uncarbonated = make_uncarbonated
end
