(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2020 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

open Error_monad

type error +=
  | Encoding_error of Data_encoding.Binary.write_error
  | Unexpected_size_of_encoded_value

type error +=
  | Decoding_error of Data_encoding.Binary.read_error
  | Unexpected_size_of_decoded_buffer

let () =
  register_error_kind
    `Permanent
    ~id:"encoding_error"
    ~title:"Encoding error"
    ~description:"Error while encoding a value for a socket"
    ~pp:(fun ppf we ->
      Format.fprintf
        ppf
        "Could not encode a value: %a"
        Data_encoding.Binary.pp_write_error
        we)
    Data_encoding.(obj1 (req "error" Binary.write_error_encoding))
    (function Encoding_error we -> Some we | _ -> None)
    (fun we -> Encoding_error we) ;
  register_error_kind
    `Permanent
    ~id:"unexpected_size_of_encoded_value"
    ~title:"Unexpected size of encoded value"
    ~description:"An encoded value is not of the expected size."
    ~pp:(fun ppf () ->
      Format.fprintf ppf "An encoded value is not of the expected size.")
    Data_encoding.empty
    (function Unexpected_size_of_encoded_value -> Some () | _ -> None)
    (fun () -> Unexpected_size_of_encoded_value) ;
  register_error_kind
    `Permanent
    ~id:"decoding_error"
    ~title:"Decoding error"
    ~description:"Error while decoding a value"
    ~pp:(fun ppf re ->
      Format.fprintf
        ppf
        "Could not decode a value: %a"
        Data_encoding.Binary.pp_read_error
        re)
    Data_encoding.(obj1 (req "error" Binary.read_error_encoding))
    (function Decoding_error re -> Some re | _ -> None)
    (fun re -> Decoding_error re) ;
  register_error_kind
    `Permanent
    ~id:"socket.unexpected_size_of_decoded_value"
    ~title:"Unexpected size of decoded value"
    ~description:"A decoded value comes from a buffer of unexpected size."
    ~pp:(fun ppf () ->
      Format.fprintf
        ppf
        "A decoded value's origin buffer is not of the expected size.")
    Data_encoding.empty
    (function Unexpected_size_of_decoded_buffer -> Some () | _ -> None)
    (fun () -> Unexpected_size_of_decoded_buffer)

let () =
  Printexc.register_printer (function
    | Json_encoding.Cannot_destruct (path, exc) ->
        Format.kasprintf
          Option.some
          "Json_encoding.Cannot_destruct at %a: %a"
          (Json_query.print_path_as_json_path ~wildcards:true)
          path
          (Data_encoding.Json.print_error ?print_unknown:None)
          exc
    | Binary.Read_error re ->
        Format.kasprintf
          Option.some
          "Data_encoding.Read_error(%a)"
          Binary.pp_read_error
          re
    | Binary.Write_error we ->
        Format.kasprintf
          Option.some
          "Data_encoding.Write_error(%a)"
          Binary.pp_write_error
          we
    | _ -> None)
