(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
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

type meth = [`GET | `POST | `DELETE | `PUT | `PATCH]

let string_of_meth = Resto.string_of_meth

let meth_of_string = Resto.meth_of_string

let meth_encoding =
  let open Data_encoding in
  conv
    string_of_meth
    (fun m ->
      match meth_of_string m with
      | None -> Stdlib.failwith "Cannot parse methods"
      | Some s -> s)
    string

module MethMap = Resto.MethMap

type (+'m, 'pr, 'p, 'q, 'i, 'o, 'e) raw =
  ('m, 'pr, 'p, 'q, 'i, 'o, 'e) Resto.MakeService(RPC_encoding).t
  constraint 'meth = [< meth]

type error = Error_monad.error list

type (+'meth, 'prefix, 'params, 'query, 'input, 'output) t =
  ('meth, 'prefix, 'params, 'query, 'input, 'output, error) raw
  constraint 'meth = [< meth]

type (+'meth, 'prefix, 'params, 'query, 'input, 'output) service =
  ('meth, 'prefix, 'params, 'query, 'input, 'output, error) raw
  constraint 'meth = [< meth]

include (
  Resto.MakeService
    (RPC_encoding) :
      module type of struct
        include Resto.MakeService (RPC_encoding)
      end
      with type (+'m, 'pr, 'p, 'q, 'i, 'o, 'e) t :=
            ('m, 'pr, 'p, 'q, 'i, 'o, 'e) raw
       and type (+'m, 'pr, 'p, 'q, 'i, 'o, 'e) service :=
            ('m, 'pr, 'p, 'q, 'i, 'o, 'e) raw)

let error_path = ref None

type Error_monad.error += Unparsable_RPC_error of Data_encoding.json

type Error_monad.error += Empty_error_list

let () =
  let open Error_monad in
  register_error_kind
    `Branch
    ~id:"RPC.Unexpected_error_encoding"
    ~title:"RPC fails with an unparsable error message"
    ~description:
      "The RPC returned with an error code, and the associated body was not a \
       valid error trace. It is likely that the answer does not comes directly \
       from a compatible node."
    ~pp:(fun ppf msg ->
      Format.fprintf
        ppf
        "@[<v 2>The RPC returned with an error code, and the associated body \
         was not a valid error trace:@[%a@]@ It is likely that the answer does \
         not comes directly from a compatible node.@]@."
        Data_encoding.Json.pp
        msg)
    Data_encoding.(obj1 (req "unparsable message" json))
    (function Unparsable_RPC_error msg -> Some msg | _ -> None)
    (fun msg -> Unparsable_RPC_error msg)

let () =
  let open Error_monad in
  register_error_kind
    `Branch
    ~id:"RPC.Empty_error_list"
    ~title:"RPC returned an empty list of errors"
    ~description:"The RPC returned with an error code but no associated error."
    ~pp:(fun ppf () ->
      Format.fprintf
        ppf
        "@[<v 2>The RPC returned with an error code but no associated \
         error.@]@.")
    Data_encoding.empty
    (function Empty_error_list -> Some () | _ -> None)
    (fun () -> Empty_error_list)

let error_encoding =
  let open Data_encoding in
  delayed (fun () ->
      let {meth; uri; _} =
        match !error_path with None -> assert false | Some p -> p
      in
      def
        "error"
        ~description:
          (Printf.sprintf
             "The full list of errors is available with the global RPC `%s %s`"
             (string_of_meth meth)
             (Uri.path_and_query uri))
      @@ conv
           ~schema:Json_schema.any
           (fun errors -> `A (List.map Error_monad.json_of_error errors))
           (function
             | `A [] -> [Empty_error_list]
             | `A errors -> List.map Error_monad.error_of_json errors
             | msg -> [Unparsable_RPC_error msg])
           json)

let error_opt_encoding =
  let open Data_encoding in
  delayed (fun () ->
      let {meth; uri; _} =
        match !error_path with None -> assert false | Some p -> p
      in
      def
        "error_opt"
        ~description:
          (Printf.sprintf
             "An optional error-trace (None indicates no error). The full list \
              of errors is available with the global RPC `%s %s`"
             (string_of_meth meth)
             (Uri.path_and_query uri))
      @@ conv
           ~schema:Json_schema.any
           (function
             | None | Some [] -> `Null
             | Some (_ :: _ as errors) ->
                 `A (List.map Error_monad.json_of_error errors))
           (function
             | `A [] | `Null | `O [] ->
                 (* in BSON, which is used as an intermediate step when
                    serialising in binary, [`A []] and [`O []] are
                    indistinguishable. For this reason, we add [`O []] as a
                    pattern to match. *)
                 None
             | `A (_ :: _ as errors) ->
                 Some (List.map Error_monad.error_of_json errors)
             | msg -> Some [Unparsable_RPC_error msg])
           json)

let get_service = get_service ~error:error_encoding

let post_service = post_service ~error:error_encoding

let delete_service = delete_service ~error:error_encoding

let patch_service = patch_service ~error:error_encoding

let put_service = put_service ~error:error_encoding

let error_service =
  get_service
    ~description:"Schema for all the RPC errors from the shell"
    ~query:RPC_query.empty
    ~output:Data_encoding.json_schema
    RPC_path.(root / "errors")

let () = error_path := Some (forge_request error_service () ())

let description_service =
  description_service
    ~description:"RPCs documentation and input/output schema"
    error_encoding
    RPC_path.(root / "describe")
