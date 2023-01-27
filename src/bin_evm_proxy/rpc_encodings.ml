(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2023 Nomadic Labs <contact@nomadic-labs.com>                *)
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

(** Encodings for the JSON-RPC standard. See
    https://www.jsonrpc.org/specification.
*)
module JSONRPC = struct
  let version = "2.0"

  let version_field = Data_encoding.(dft "jsonrpc" string version)

  (** Ids in the JSON-RPC specification can be either a string, a number or NULL
      (which is represented by the option type). *)
  type id = Id_string of string | Id_float of float

  let id_encoding =
    let open Data_encoding in
    union
      [
        case
          ~title:"id-string"
          (Tag 0)
          string
          (function Id_string s -> Some s | _ -> None)
          (fun s -> Id_string s);
        case
          ~title:"id-int"
          (Tag 1)
          float
          (function Id_float i -> Some i | _ -> None)
          (fun i -> Id_float i);
      ]

  type 'params request = {
    method_ : string;
    parameters : 'params option;
    id : id option;
  }

  let request_encoding method_ parameters_encoding =
    Data_encoding.(
      conv
        (fun {parameters; id; _} -> (version, (), parameters, id))
        (fun (_, _, parameters, id) -> {method_; parameters; id})
        (obj4
           version_field
           (req "method" (constant method_))
           (opt "params" parameters_encoding)
           (opt "id" id_encoding)))

  type 'data error = {code : int; message : string; data : 'data option}

  let error_encoding data_encoding =
    Data_encoding.(
      conv
        (fun {code; message; data} -> (code, message, data))
        (fun (code, message, data) -> {code; message; data})
        (obj3
           (req "code" int31)
           (req "message" string)
           (opt "data" data_encoding)))

  type ('result, 'data_error) response = {
    value : ('result, 'data_error error) result;
    id : id option;
  }

  let response_encoding result_encoding error_data_encoding =
    Data_encoding.(
      conv
        (fun {value; id} ->
          let result, error =
            match value with Ok r -> (Some r, None) | Error e -> (None, Some e)
          in
          (version, result, error, id))
        (fun (_, result, error, id) ->
          let value =
            match (result, error) with
            | Some r, None -> Ok r
            | None, Some e -> Error e
            | _ -> assert false
            (* Impossible case according to the JSON-RPC standard: result XOR
               error. *)
          in
          {value; id})
        (obj4
           version_field
           (opt "result" result_encoding)
           (opt "error" (error_encoding error_data_encoding))
           (req "id" (option id_encoding))))
end

type input = ..

type output = ..

module Error = struct
  type t = unit

  let encoding = Data_encoding.unit
end

type 'result rpc_result = ('result, Error.t JSONRPC.error) result

module type METHOD_DEF = sig
  val method_ : string

  type input

  type output

  val input_encoding : input Data_encoding.t

  val output_encoding : output Data_encoding.t
end

module type METHOD = sig
  type m_input

  type m_output

  (* The parameters MAY be omitted. See JSONRPC Specification. *)
  type input += Input of m_input option

  type output += Output of m_output rpc_result

  val method_ : string

  val request_encoding : m_input JSONRPC.request Data_encoding.t

  val request : m_input option -> m_input JSONRPC.request

  val response_encoding : (m_output, Error.t) JSONRPC.response Data_encoding.t

  val response :
    (m_output, Error.t JSONRPC.error) result ->
    (m_output, Error.t) JSONRPC.response

  val response_ok : m_output -> (m_output, Error.t) JSONRPC.response
end

module MethodMaker (M : METHOD_DEF) :
  METHOD with type m_input = M.input and type m_output = M.output = struct
  type m_input = M.input

  type m_output = M.output

  type input += Input of m_input option

  type output += Output of m_output rpc_result

  let method_ = M.method_

  let request_encoding = JSONRPC.request_encoding M.method_ M.input_encoding

  let request parameters = JSONRPC.{method_; parameters; id = None}

  let response_encoding =
    JSONRPC.response_encoding M.output_encoding Error.encoding

  let response value = JSONRPC.{value; id = None}

  let response_ok result = response (Ok result)
end

let methods : (module METHOD) list = []

module Input = struct
  type t = input

  let case_maker tag_id (module M : METHOD) =
    let open Data_encoding in
    case
      ~title:M.method_
      (Tag tag_id)
      M.request_encoding
      (function M.Input input -> Some (M.request input) | _ -> None)
      (fun {parameters; _} -> M.Input parameters)

  let encoding =
    let open Data_encoding in
    union @@ List.mapi case_maker methods
end

module Output = struct
  type nonrec 'a result = ('a, error JSONRPC.error) result

  let case_maker tag_id (module M : METHOD) =
    let open Data_encoding in
    case
      ~title:M.method_
      (Tag tag_id)
      M.response_encoding
      (function
        | M.Output accounts -> Some JSONRPC.{value = accounts; id = None}
        | _ -> None)
      (fun {value = req; _} -> M.Output req)

  let encoding =
    let open Data_encoding in
    union @@ List.mapi case_maker methods
end
