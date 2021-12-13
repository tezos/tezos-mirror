(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2019 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

open Lwt.Infix
open Clic

let group = {name = "encoding"; title = "Commands to handle encodings"}

let id_parameter =
  parameter (fun (cctxt : #Client_context.printer) id ->
      match Data_encoding.Registration.find id with
      | Some record -> return record
      | None -> cctxt#error "Unknown encoding id: %s" id)

let json_parameter =
  parameter (fun (cctxt : #Client_context.printer) file_or_data ->
      (Lwt_unix.file_exists file_or_data >>= function
       | true -> Tezos_stdlib_unix.Lwt_utils_unix.read_file file_or_data
       | false -> Lwt.return file_or_data)
      >>= fun data ->
      match Json.from_string data with
      | Ok json -> return json
      | Error err -> cctxt#error "%s" err)

let bytes_parameter =
  parameter (fun (cctxt : #Client_context.printer) hex ->
      match Hex.to_bytes (`Hex hex) with
      | Some s -> return s
      | None -> cctxt#error "Invalid hex string: %s" hex)

let commands () =
  [
    command
      ~group
      ~desc:"List the registered encoding in Tezos."
      no_options
      (fixed ["list"; "encodings"])
      (fun () (cctxt : #Client_context.printer) ->
        let bindings =
          Data_encoding.Registration.list ()
          |> List.map (fun (id, elem) ->
                 (id, Data_encoding.Registration.description elem))
        in
        cctxt#message
          "@[<v>%a@]@."
          (Format.pp_print_list
             ~pp_sep:Format.pp_print_cut
             (fun ppf (id, desc) ->
               let desc =
                 Option.value ~default:"No description available." desc
               in
               Format.fprintf
                 ppf
                 "@[<v 2>%s:@ @[%a@]@]"
                 id
                 Format.pp_print_text
                 desc))
          bindings
        >>= fun () -> return_unit);
    command
      ~group
      ~desc:"Dump a json description of all registered encodings."
      (args1
      @@ switch
           ~doc:
             "Output json descriptions without extraneous whitespace characters"
           ~long:"compact"
           ())
      (fixed ["dump"; "encodings"])
      (fun minify (cctxt : #Client_context.printer) ->
        cctxt#message
          "%s"
          (Json.to_string
             ~minify
             (`A
               (Registration.list ()
               |> List.map (fun (id, enc) ->
                      `O
                        [
                          ("id", `String id);
                          ( "json",
                            Json.construct
                              Json.schema_encoding
                              (Registration.json_schema enc) );
                          ( "binary",
                            Json.construct
                              Binary_schema.encoding
                              (Registration.binary_schema enc) );
                        ]))))
        >>= fun () -> return_unit);
    (* JSON -> Binary *)
    command
      ~group
      ~desc:
        "Encode the given JSON data into binary using the provided encoding \
         identifier."
      no_options
      (prefix "encode"
      @@ param ~name:"id" ~desc:"Encoding identifier" id_parameter
      @@ prefix "from"
      @@ param ~name:"json" ~desc:"JSON file or data" json_parameter
      @@ stop)
      (fun () registered_encoding json (cctxt : #Client_context.printer) ->
        match
          Data_encoding.Registration.bytes_of_json registered_encoding json
        with
        | exception exn ->
            cctxt#error "%a" (fun ppf exn -> Json.print_error ppf exn) exn
        | None ->
            cctxt#error
              "Impossible to the JSON convert to binary.@,\
               This error should not happen."
        | Some bytes ->
            cctxt#message "%a" Hex.pp (Hex.of_bytes bytes) >>= fun () ->
            return_unit);
    (* Binary -> JSON *)
    command
      ~group
      ~desc:
        "Decode the binary encoded data into JSON using the provided encoding \
         identifier."
      no_options
      (prefix "decode"
      @@ param ~name:"id" ~desc:"Encoding identifier" id_parameter
      @@ prefix "from"
      @@ param ~name:"hex" ~desc:"Binary encoded data" bytes_parameter
      @@ stop)
      (fun () registered_encoding bytes (cctxt : #Client_context.printer) ->
        match
          Data_encoding.Registration.json_of_bytes registered_encoding bytes
        with
        | None -> cctxt#error "Cannot parse the binary with the given encoding"
        | Some bytes ->
            cctxt#message "%a" Json.pp bytes >>= fun () -> return_unit);
    command
      ~group
      ~desc:
        "Display the binary encoded data using the provided encoding \
         identifier."
      no_options
      (prefix "display"
      @@ param ~name:"id" ~desc:"Encoding identifier" id_parameter
      @@ prefixes ["from"; "binary"]
      @@ param ~name:"hex" ~desc:"Binary encoded data" bytes_parameter
      @@ stop)
      (fun () registered_encoding bytes (cctxt : #Client_context.printer) ->
        let pp_bytes fmt bytes =
          Data_encoding.Registration.binary_pretty_printer
            registered_encoding
            fmt
            bytes
        in
        cctxt#message "%a" pp_bytes bytes >>= fun () -> return_unit);
    command
      ~group
      ~desc:
        "Display the JSON encoded data using the provided encoding identifier."
      no_options
      (prefix "display"
      @@ param ~name:"id" ~desc:"Encoding identifier" id_parameter
      @@ prefixes ["from"; "json"]
      @@ param ~name:"json" ~desc:"JSON file or data" json_parameter
      @@ stop)
      (fun () registered_encoding json (cctxt : #Client_context.printer) ->
        let pp_json fmt json =
          Data_encoding.Registration.json_pretty_printer
            registered_encoding
            fmt
            json
        in
        cctxt#message "%a" pp_json json >>= fun () -> return_unit);
    command
      ~group
      ~desc:
        "Describe the binary schema associated to the provided encoding \
         identifier."
      no_options
      (prefix "describe"
      @@ param ~name:"id" ~desc:"Encoding identifier" id_parameter
      @@ prefixes ["binary"; "schema"]
      @@ stop)
      (fun () registered_encoding (cctxt : #Client_context.printer) ->
        let schema =
          Data_encoding.Registration.binary_schema registered_encoding
        in
        cctxt#message "%a" Data_encoding.Binary_schema.pp schema >>= fun () ->
        return_unit);
    command
      ~group
      ~desc:
        "Describe the JSON schema associated to the provided encoding \
         identifier."
      no_options
      (prefix "describe"
      @@ param ~name:"id" ~desc:"Encoding identifier" id_parameter
      @@ prefixes ["json"; "schema"]
      @@ stop)
      (fun () registered_encoding cctxt ->
        let schema =
          Data_encoding.Registration.json_schema registered_encoding
        in
        cctxt#message "%a" Json_schema.pp schema >>= fun () -> return_unit);
  ]
