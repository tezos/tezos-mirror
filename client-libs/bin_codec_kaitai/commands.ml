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

open Tezos_clic

let group = {name = "encoding"; title = "Commands to handle encodings"}

let raw_id_parameter =
  parameter (fun (cctxt : #Client_context.printer) id ->
      match Data_encoding.Registration.find id with
      | Some _ -> Lwt.return_ok id
      | None -> cctxt#error "Unknown encoding id: %s" id)

let id_parameter =
  parameter (fun (cctxt : #Client_context.printer) id ->
      match Data_encoding.Registration.find id with
      | Some record -> Lwt.return_ok record
      | None -> cctxt#error "Unknown encoding id: %s" id)

let json_parameter =
  let open Lwt_syntax in
  parameter (fun (cctxt : #Client_context.printer) file_or_data ->
      let* data =
        let* file_exists = Lwt_unix.file_exists file_or_data in
        if file_exists then
          Tezos_stdlib_unix.Lwt_utils_unix.read_file file_or_data
        else Lwt.return file_or_data
      in
      match Json.from_string data with
      | Ok json -> return_ok json
      | Error err -> cctxt#error "%s" err)

let bytes_parameter =
  parameter (fun (cctxt : #Client_context.printer) hex ->
      let hex =
        if Sys.file_exists hex then (
          let ic = open_in hex in
          let contents =
            let rec loop acc =
              match input_line ic with
              | s -> loop (s :: acc)
              | exception End_of_file -> String.concat "" (List.rev acc)
            in
            loop []
          in
          close_in ic ;
          contents)
        else hex
      in
      match Hex.to_bytes (`Hex hex) with
      | Some s -> Lwt.return_ok s
      | None -> cctxt#error "Invalid hex string: %s" hex)

let full_bytes_parameter =
  param
    ~name:"data"
    ~desc:"Hex-encoded binary-encoded data or name of file containing the data"
    bytes_parameter

let format_arg =
  default_arg
    ~doc:"The format to print the output in: json, pretty, or waterfall."
    ~long:"format"
    ~placeholder:"FORMAT"
    ~default:"pretty"
    (parameter (fun (cctxt : #Client_context.printer) format ->
         match format with
         | "json" -> Lwt.return_ok `Json
         | "pretty" -> Lwt.return_ok `Pp
         | "waterfall" -> Lwt.return_ok `Waterfall
         | _ ->
             cctxt#error
               "Cannot decode --format argument, use 'json', 'pretty', or \
                'waterfall'."))

let slices_encoding =
  let open Data_encoding in
  list
    (conv
       (fun {Binary.Slicer.name; value; pretty_printed} ->
         let pretty_printed =
           if pretty_printed = "" then None else Some pretty_printed
         in
         let hex_slice = Format.asprintf "%a" Hex.pp (Hex.of_string value) in
         (name, value, hex_slice, pretty_printed))
       (fun (name, value, _, pretty_printed) ->
         let pretty_printed = Option.value ~default:"" pretty_printed in
         {Binary.Slicer.name; value; pretty_printed})
       (obj4
          (req "name" string)
          (req "raw-slice" string)
          (req "hex-slice" string)
          (opt "pretty" string)))

let pp_slices format ppf slices =
  match format with
  | `Json ->
      let j = Data_encoding.Json.construct slices_encoding slices in
      let () = Data_encoding.Json.pp ppf j in
      ()
  | `Pp ->
      Format.pp_print_list
        ~pp_sep:Format.pp_print_newline
        (fun ppf {Data_encoding.Binary.Slicer.name; value; pretty_printed} ->
          let value = Format.asprintf "%a" Hex.pp (Hex.of_string value) in
          if String.length value <= 18 then
            Format.fprintf
              ppf
              "%s%s%s%a"
              value
              (String.make (20 - String.length value) ' ')
              name
              (fun ppf ppv ->
                if ppv = "" then () else Format.fprintf ppf " = %s" ppv)
              pretty_printed
          else
            Format.fprintf
              ppf
              "%s\n%s%s%a"
              value
              (String.make 20 ' ')
              name
              (fun ppf ppv ->
                if ppv = "" then () else Format.fprintf ppf " = %s" ppv)
              pretty_printed)
        ppf
        slices
  | `Waterfall ->
      let (_ : int) =
        List.fold_left
          (fun margin
               {Data_encoding.Binary.Slicer.name; value; pretty_printed}
             ->
            let value = Format.asprintf "%a" Hex.pp (Hex.of_string value) in
            Format.fprintf
              ppf
              "%s%s   %s%a\n"
              (String.make margin ' ')
              value
              name
              (fun ppf ppv ->
                if ppv = "" then () else Format.fprintf ppf " = %s" ppv)
              pretty_printed ;
            margin + String.length value)
          0
          slices
      in
      ()

let commands () =
  let open Lwt_syntax in
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
        let* () =
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
        in
        Lwt_result_syntax.return_unit);
    command
      ~group
      ~desc:"Dump a JSON description of all registered encodings."
      (args1
      @@ switch
           ~doc:
             "Output JSON descriptions without extraneous whitespace characters"
           ~long:"compact"
           ())
      (fixed ["dump"; "encodings"])
      (fun minify (cctxt : #Client_context.printer) ->
        let* () =
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
        in
        Lwt_result_syntax.return_unit);
    command
      ~group
      ~desc:"Dump a JSON description of a given registered encoding."
      (args1
      @@ switch
           ~doc:
             "Output JSON description without extraneous whitespace characters"
           ~long:"compact"
           ())
      (prefix "dump" @@ prefix "encoding"
      @@ param ~name:"id" ~desc:"Encoding identifier" id_parameter
      @@ stop)
      (fun minify registered_encoding (cctxt : #Client_context.printer) ->
        let* () =
          cctxt#message
            "%s"
            (Json.to_string
               ~minify
               (`O
                  [
                    ( "json",
                      Json.construct
                        Json.schema_encoding
                        (Registration.json_schema registered_encoding) );
                    ( "binary",
                      Json.construct
                        Binary_schema.encoding
                        (Registration.binary_schema registered_encoding) );
                  ]))
        in
        Lwt_result_syntax.return_unit);
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
      @@ param ~name:"data" ~desc:"JSON file or data" json_parameter
      @@ stop)
      (fun () registered_encoding data (cctxt : #Client_context.printer) ->
        match
          Data_encoding.Registration.bytes_of_json registered_encoding data
        with
        | exception exn ->
            cctxt#error "%a" (fun ppf exn -> Json.print_error ppf exn) exn
        | None ->
            cctxt#error
              "Impossible to the JSON convert to binary.@,\
               This error should not happen."
        | Some bytes ->
            let* () = cctxt#message "%a" Hex.pp (Hex.of_bytes bytes) in
            Lwt_result_syntax.return_unit);
    (* Binary -> JSON *)
    command
      ~group
      ~desc:
        "Decode the binary encoded data into JSON using the provided encoding \
         identifier."
      no_options
      (prefix "decode"
      @@ param ~name:"id" ~desc:"Encoding identifier" id_parameter
      @@ prefix "from" @@ full_bytes_parameter @@ stop)
      (fun () registered_encoding bytes (cctxt : #Client_context.printer) ->
        match
          Data_encoding.Registration.json_of_bytes registered_encoding bytes
        with
        | None -> cctxt#error "Cannot parse the binary with the given encoding"
        | Some bytes ->
            let* () = cctxt#message "%a" Json.pp bytes in
            Lwt_result_syntax.return_unit);
    command
      ~group
      ~desc:
        "Display the binary encoded data using the provided encoding \
         identifier."
      no_options
      (prefix "display"
      @@ param ~name:"id" ~desc:"Encoding identifier" id_parameter
      @@ prefixes ["from"; "binary"]
      @@ full_bytes_parameter @@ stop)
      (fun () registered_encoding bytes (cctxt : #Client_context.printer) ->
        let pp_bytes fmt bytes =
          Data_encoding.Registration.binary_pretty_printer
            registered_encoding
            fmt
            bytes
        in
        let* () = cctxt#message "%a" pp_bytes bytes in
        Lwt_result_syntax.return_unit);
    command
      ~group
      ~desc:
        "Display the JSON encoded data using the provided encoding identifier."
      no_options
      (prefix "display"
      @@ param ~name:"id" ~desc:"Encoding identifier" id_parameter
      @@ prefixes ["from"; "json"]
      @@ param ~name:"data" ~desc:"JSON file or data" json_parameter
      @@ stop)
      (fun () registered_encoding data (cctxt : #Client_context.printer) ->
        let pp_json fmt json =
          Data_encoding.Registration.json_pretty_printer
            registered_encoding
            fmt
            json
        in
        let* () = cctxt#message "%a" pp_json data in
        Lwt_result_syntax.return_unit);
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
        let* () = cctxt#message "%a" Data_encoding.Binary_schema.pp schema in
        Lwt_result_syntax.return_unit);
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
        let* () = cctxt#message "%a" Json_schema.pp schema in
        Lwt_result_syntax.return_unit);
    command
      ~group
      ~desc:
        "Attempts to slice an hex-encoded binary value with all known \
         encodings."
      no_options
      (prefix "slice" @@ full_bytes_parameter @@ stop)
      (fun () bytes cctxt ->
        let bytes = Bytes.to_string bytes in
        let all = Data_encoding.Registration.slice_all bytes in
        match all with
        | [] -> cctxt#error "No matching encoding found"
        | _ ->
            let* () =
              List.iter_s
                (fun (encoding_name, slices) ->
                  cctxt#message
                    "%s:\n%a\n\n"
                    encoding_name
                    (pp_slices `Pp)
                    slices)
                all
            in
            Lwt_result_syntax.return_unit);
    command
      ~group
      ~desc:"Slice an hex-encoded binary value with the specified encoding."
      (args1 format_arg)
      (prefix "slice"
      @@ param
           ~name:"data"
           ~desc:
             "Hex-encoded binary-encoded data or name of file containing the \
              data"
           bytes_parameter
      @@ prefixes ["with"; "encoding"]
      @@ param ~name:"id" ~desc:"Encoding identifier" id_parameter
      @@ stop)
      (fun format bytes encoding_id cctxt ->
        let bytes = Bytes.to_string bytes in
        match Data_encoding.Registration.slice encoding_id bytes with
        | Error read_error ->
            cctxt#error "%a" Data_encoding.Binary.pp_read_error read_error
        | Ok slices ->
            let* () = cctxt#message "%a\n" (pp_slices format) slices in
            Lwt_result_syntax.return_unit);
    command
      ~group
      ~desc:"Dump a Kaitai Struct specification of a given registered encoding."
      no_options
      (prefix "dump" @@ prefix "kaitai" @@ prefix "for"
      @@ param ~name:"id" ~desc:"Encoding identifier" raw_id_parameter
      @@ stop)
      (fun () encoding_id cctxt ->
        let open Lwt_syntax in
        let introspectable =
          Data_encoding.Registration.find_introspectable encoding_id
        in
        let* kaitai_spec =
          match introspectable with
          | None -> cctxt#error "Error retrieving introspectable"
          | Some (Any encoding) ->
              return
              @@ Kaitai_of_data_encoding.Translate.from_data_encoding
                   ~id:encoding_id
                   encoding
        in
        let* () =
          cctxt#answer
            "%a"
            (fun fmt s ->
              if s.[String.length s - 1] = '\n' then
                let w, _ = Format.pp_get_formatter_output_functions fmt () in
                w s 0 (String.length s - 1)
              else Format.pp_print_string fmt s)
            (Kaitai.Print.print kaitai_spec)
        in
        Lwt_result_syntax.return_unit);
    command
      ~group
      ~desc:"Dump Kaitai Struct specifications for all registered encodings."
      no_options
      (prefix "dump" @@ prefix "kaitai" @@ prefix "specs" @@ prefix "in"
      @@ string ~name:"dir" ~desc:"Directory in which to output the ksy files"
      @@ stop)
      (fun () dir (cctxt : #Client_context.printer) ->
        let open Lwt_syntax in
        let all = Data_encoding.Registration.list () in
        let all =
          List.map
            (fun (id, _registered) ->
              (* TODO: avoid this lookup by exporting `t -> introspectable` *)
              match Data_encoding.Registration.find_introspectable id with
              | None ->
                  (* [id] is from iterating over the list of registered encodings *)
                  assert false
              | Some (Any e) ->
                  (id, Kaitai_of_data_encoding.Translate.AnyEncoding.pack e))
            all
        in
        let name_of_id id = Kaitai_of_data_encoding.Translate.escape_id id in
        let extern =
          let t =
            Kaitai_of_data_encoding.Translate.AnyEncoding.Tbl.create 200
          in
          List.iter
            (fun (id, encoding) ->
              Kaitai_of_data_encoding.Translate.AnyEncoding.Tbl.add
                t
                encoding
                (name_of_id id))
            all ;
          t
        in
        let* () =
          List.iter_s
            (fun ( id,
                   Kaitai_of_data_encoding.Translate.AnyEncoding.AnyEncoding e
                 )
               ->
              match
                Kaitai_of_data_encoding.Translate.from_data_encoding
                  ~id
                  ~extern
                  e
              with
              | exception e ->
                  cctxt#warning
                    "Failed to generate ksy file for %s (%s)"
                    id
                    (Printexc.to_string e)
              | spec -> (
                  match Kaitai.Print.print spec with
                  | exception e ->
                      cctxt#warning
                        "Failed to print ksy file for %s (%s)"
                        id
                        (Printexc.to_string e)
                  | yml ->
                      Lwt_io.with_file
                        ~mode:Output
                        (dir ^ "/" ^ name_of_id id ^ ".ksy")
                        (fun oc -> Lwt_io.write oc yml)))
            all
        in
        return_ok_unit);
  ]
