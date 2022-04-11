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

(* Tezos Command line interface - Generic JSON RPC interface *)

open Clic
open Json_schema

(*-- Assisted, schema directed input fill in --------------------------------*)

exception Unsupported_construct

type input = {
  int : int -> int -> string option -> string list -> int Lwt.t;
  float : string option -> string list -> float Lwt.t;
  string : string option -> string list -> string Lwt.t;
  bool : string option -> string list -> bool Lwt.t;
  continue : string option -> string list -> bool Lwt.t;
  display : string -> unit Lwt.t;
}

let no_service_at_valid_prefix (cctxt : #Client_context.full) =
  cctxt#error "No service found at this URL (but this is a valid prefix)\n"

(* generic JSON generation from a schema with callback for random or
   interactive filling *)
let fill_in ?(show_optionals = true) input schema =
  let rec element path {title; kind; _} =
    let open Lwt_syntax in
    match kind with
    | Integer {minimum; maximum; _} ->
        let minimum =
          match minimum with
          | None -> min_int
          | Some (m, `Inclusive) -> int_of_float m
          | Some (m, `Exclusive) -> int_of_float m + 1
        in
        let maximum =
          match maximum with
          | None -> max_int
          | Some (m, `Inclusive) -> int_of_float m
          | Some (m, `Exclusive) -> int_of_float m - 1
        in
        let+ i = input.int minimum maximum title path in
        `Float (float i)
    | Number _ ->
        let+ f = input.float title path in
        `Float f
    | Boolean ->
        let+ f = input.bool title path in
        `Bool f
    | String _ ->
        let+ f = input.string title path in
        `String f
    | Combine ((One_of | Any_of), elts) ->
        let nb = List.length elts in
        let* n =
          input.int 0 (nb - 1) (Some "Select the schema to follow") path
        in
        element path (WithExceptions.Option.get ~loc:__LOC__ @@ List.nth elts n)
    | Combine ((All_of | Not), _) -> Lwt.fail Unsupported_construct
    | Def_ref name ->
        Lwt.return (`String (Json_query.json_pointer_of_path name))
    | Id_ref _ | Ext_ref _ -> Lwt.fail Unsupported_construct
    | Array (elts, _) ->
        let+ a =
          List.mapi_s (fun n elt -> element (string_of_int n :: path) elt) elts
        in
        `A a
    | Object {properties; _} ->
        if show_optionals then
          let+ o =
            List.map_s
              (fun (n, elt, _, _) ->
                let+ json = element (n :: path) elt in
                (n, json))
              properties
          in
          `O o
        else
          let+ o =
            List.filter_map_s
              (fun (n, elt, optional, _) ->
                if optional then
                  let+ json = element (n :: path) elt in
                  Some (n, json)
                else Lwt.return_none)
              properties
          in
          `O o
    | Monomorphic_array (elt, specs) ->
        let rec fill_loop acc min n max =
          if n > max then Lwt.return acc
          else
            let* json = element (string_of_int n :: path) elt in
            let* b =
              if n < min then Lwt.return_true else input.continue title path
            in
            if b then fill_loop (json :: acc) min (succ n) max
            else Lwt.return (json :: acc)
        in
        let max = Option.value specs.max_items ~default:max_int in
        let+ a = fill_loop [] specs.min_items 0 max in
        let a = List.rev a in
        `A a
    | Any -> Lwt.fail Unsupported_construct
    | Dummy -> Lwt.fail Unsupported_construct
    | Null -> Lwt.return `Null
  in
  element [] (Json_schema.root schema)

let random_fill_in ?(show_optionals = true) schema =
  let display _ = Lwt.return_unit in
  let int min max _ _ =
    let max = Int64.of_int max and min = Int64.of_int min in
    let range = Int64.sub max min in
    let random_int64 = Int64.add (Random.int64 range) min in
    Lwt.return (Int64.to_int random_int64)
  in
  let string _title _ = Lwt.return "" in
  let float _ _ = Lwt.return (Random.float infinity) in
  let bool _ _ = Lwt.return (Random.int 2 = 0) in
  let continue _ _ = Lwt.return (Random.int 4 = 0) in
  Lwt.catch
    (fun () ->
      Lwt_result.ok
      @@ fill_in
           ~show_optionals
           {int; float; string; bool; display; continue}
           schema)
    (fun e ->
      let msg = Printf.sprintf "Fill-in failed %s\n%!" (Printexc.to_string e) in
      Lwt.return_error msg)

let editor_fill_in ?(show_optionals = true) schema =
  let tmp = Filename.temp_file "tezos_rpc_call_" ".json" in
  let open Lwt_syntax in
  let rec init () =
    (* write a temp file with instructions *)
    let* r = random_fill_in ~show_optionals schema in
    match r with
    | Error msg -> Lwt.return_error msg
    | Ok json ->
        let* () =
          Lwt_io.(
            with_file ~mode:Output tmp (fun fp ->
                write_line fp (Data_encoding.Json.to_string json)))
        in
        edit ()
  and edit () =
    (* launch the user's editor on it *)
    let editor_cmd =
      let ed =
        match (Sys.getenv_opt "EDITOR", Sys.getenv_opt "VISUAL") with
        | (Some ed, _) -> ed
        | (None, Some ed) -> ed
        | (None, None) when Sys.win32 ->
            (* TODO: I have no idea what I'm doing here *)
            "notepad.exe"
        | _ ->
            (* TODO: vi on MacOSX ? *)
            "nano"
      in
      Lwt_process.shell (ed ^ " " ^ tmp)
    in
    let* s = (Lwt_process.open_process_none editor_cmd)#status in
    match s with
    | Unix.WEXITED 0 ->
        let* json = reread () in
        let* () = delete () in
        Lwt.return json
    | Unix.WSIGNALED x | Unix.WSTOPPED x | Unix.WEXITED x ->
        let msg = Printf.sprintf "FAILED %d \n%!" x in
        let* () = delete () in
        Lwt.return_error msg
  and reread () =
    (* finally reread the file *)
    let* text = Lwt_io.(with_file ~mode:Input tmp (fun fp -> read fp)) in
    match Data_encoding.Json.from_string text with
    | Ok r -> Lwt.return_ok r
    | Error msg -> Lwt.return_error (Format.asprintf "bad input: %s" msg)
  and delete () =
    (* and delete the temp file *)
    Lwt_unix.unlink tmp
  in
  init ()

(*-- Nice list display ------------------------------------------------------*)

let rec count =
  let open RPC_description in
  function
  | Empty -> 0
  | Dynamic _ -> 1
  | Static {services; subdirs} ->
      let service = RPC_service.MethMap.cardinal services in
      let subdirs =
        match subdirs with
        | None -> 0
        | Some (Suffixes subdirs) ->
            Resto.StringMap.fold (fun _ t r -> r + count t) subdirs 0
        | Some (Arg (_, subdir)) -> count subdir
      in
      service + subdirs

(*-- Commands ---------------------------------------------------------------*)

let list url (cctxt : #Client_context.full) =
  let open Lwt_result_syntax in
  let args = String.split_no_empty '/' url in
  let* tree = RPC_description.describe cctxt ~recurse:true args in
  let open RPC_description in
  let collected_args = ref [] in
  let collect arg =
    if
      not
        (arg.RPC_arg.descr <> None
        && List.mem ~equal:RPC_arg.eq_descr arg !collected_args)
    then collected_args := arg :: !collected_args
  in
  let display_paragraph ppf description =
    Format.fprintf
      ppf
      "@,    @[%a@]"
      (fun ppf words -> List.iter (Format.fprintf ppf "%s@ ") words)
      (String.split_no_empty ' ' description)
  in
  let display_arg ppf arg =
    match arg.RPC_arg.descr with
    | None -> Format.fprintf ppf "%s" arg.RPC_arg.name
    | Some descr ->
        Format.fprintf ppf "<%s>%a" arg.RPC_arg.name display_paragraph descr
  in
  let display_service ppf (_path, tpath, service) =
    Format.fprintf
      ppf
      "- %s /%s"
      (RPC_service.string_of_meth service.meth)
      (String.concat "/" tpath) ;
    match service.description with
    | None | Some "" -> ()
    | Some description -> display_paragraph ppf description
  in
  let display_services ppf (_path, tpath, services) =
    Format.pp_print_list
      (fun ppf (_, s) -> display_service ppf (_path, tpath, s))
      ppf
      (RPC_service.MethMap.bindings services)
  in
  let rec display ppf (path, tpath, tree) =
    match tree with
    | Dynamic description -> (
        Format.fprintf ppf "- /%s <dynamic>" (String.concat "/" tpath) ;
        match description with
        | None | Some "" -> ()
        | Some description -> display_paragraph ppf description)
    | Empty -> ()
    | Static {services; subdirs = None} ->
        display_services ppf (path, tpath, services)
    | Static {services; subdirs = Some (Suffixes subdirs)} -> (
        match
          ( RPC_service.MethMap.cardinal services,
            Resto.StringMap.bindings subdirs )
        with
        | (0, []) -> ()
        | (0, [(n, solo)]) -> display ppf (path @ [n], tpath @ [n], solo)
        | (_, items) when count tree >= 3 && path <> [] ->
            Format.fprintf
              ppf
              "@[<v 2>+ %s/@,%a@]"
              (String.concat "/" path)
              (display_list tpath)
              items
        | (_, items) when count tree >= 3 && path <> [] ->
            Format.fprintf
              ppf
              "@[<v 2>+ %s@,%a@,%a@]"
              (String.concat "/" path)
              display_services
              (path, tpath, services)
              (display_list tpath)
              items
        | (0, (n, t) :: items) ->
            Format.fprintf ppf "%a" display (path @ [n], tpath @ [n], t) ;
            List.iter
              (fun (n, t) ->
                Format.fprintf ppf "@,%a" display (path @ [n], tpath @ [n], t))
              items
        | (_, items) ->
            display_services ppf (path, tpath, services) ;
            List.iter
              (fun (n, t) ->
                Format.fprintf ppf "@,%a" display (path @ [n], tpath @ [n], t))
              items)
    | Static {services; subdirs = Some (Arg (arg, solo))}
      when RPC_service.MethMap.cardinal services = 0 ->
        collect arg ;
        let name = Printf.sprintf "<%s>" arg.RPC_arg.name in
        display ppf (path @ [name], tpath @ [name], solo)
    | Static {services; subdirs = Some (Arg (arg, solo))} ->
        collect arg ;
        display_services ppf (path, tpath, services) ;
        Format.fprintf ppf "@," ;
        let name = Printf.sprintf "<%s>" arg.RPC_arg.name in
        display ppf (path @ [name], tpath @ [name], solo)
  and display_list tpath =
    Format.pp_print_list (fun ppf (n, t) -> display ppf ([n], tpath @ [n], t))
  in
  let*! () =
    cctxt#message
      "@ @[<v 2>Available services:@ @ %a@]@."
      display
      (args, args, tree)
  in
  if !collected_args <> [] then
    let*! () =
      cctxt#message
        "@,@[<v 2>Dynamic parameter description:@ @ %a@]@."
        (Format.pp_print_list display_arg)
        !collected_args
    in
    return_unit
  else return_unit

let schema meth url (cctxt : #Client_context.full) =
  let open Lwt_result_syntax in
  let args = String.split_no_empty '/' url in
  let open RPC_description in
  let* s = RPC_description.describe cctxt ~recurse:false args in
  match s with
  | Static {services; _} -> (
      match RPC_service.MethMap.find_opt meth services with
      | None -> no_service_at_valid_prefix cctxt
      | Some {input = Some input; output; _} ->
          let json =
            `O
              [
                ("input", Json_schema.to_json (fst (Lazy.force input)));
                ("output", Json_schema.to_json (fst (Lazy.force output)));
              ]
          in
          let*! () = cctxt#message "%a" Json_repr.(pp (module Ezjsonm)) json in
          return_unit
      | Some {input = None; output; _} ->
          let json =
            `O [("output", Json_schema.to_json (fst (Lazy.force output)))]
          in
          let*! () = cctxt#message "%a" Json_repr.(pp (module Ezjsonm)) json in
          return_unit)
  | _ -> no_service_at_valid_prefix cctxt

let format binary meth url (cctxt : #Client_context.io_rpcs) =
  let open Lwt_result_syntax in
  let args = String.split_no_empty '/' url in
  let open RPC_description in
  let pp =
    if binary then fun ppf (_, schema) ->
      Data_encoding.Binary_schema.pp ppf schema
    else fun ppf (schema, _) -> Json_schema.pp ppf schema
  in
  let* s = RPC_description.describe cctxt ~recurse:false args in
  match s with
  | Static {services; _} -> (
      match RPC_service.MethMap.find_opt meth services with
      | None -> no_service_at_valid_prefix cctxt
      | Some {input = Some input; output; _} ->
          let*! () =
            cctxt#message
              "@[<v 0>@[<v 2>Input format:@,\
               %a@]@,\
               @[<v 2>Output format:@,\
               %a@]@,\
               @]"
              pp
              (Lazy.force input)
              pp
              (Lazy.force output)
          in
          return_unit
      | Some {input = None; output; _} ->
          let*! () =
            cctxt#message
              "@[<v 0>@[<v 2>Output format:@,%a@]@,@]"
              pp
              (Lazy.force output)
          in
          return_unit)
  | _ -> no_service_at_valid_prefix cctxt

let fill_in ?(show_optionals = true) schema =
  let open Json_schema in
  match (root schema).kind with
  | Null -> Lwt.return_ok `Null
  | Any | Object {properties = []; _} -> Lwt.return_ok (`O [])
  | _ -> editor_fill_in ~show_optionals schema

let display_answer (cctxt : #Client_context.full) :
    RPC_context.generic_call_result -> unit Lwt.t = function
  | `Json (`Ok json) -> cctxt#answer "%a" Json_repr.(pp (module Ezjsonm)) json
  | `Binary (`Ok binary) -> cctxt#answer "%a" Hex.pp (Hex.of_string binary)
  | `Json (`Error (Some error)) ->
      cctxt#error
        "@[<v 2>Command failed: @[%a@]@]@."
        (Format.pp_print_list Error_monad.pp)
        (Data_encoding.Json.destruct
           (Data_encoding.list Error_monad.error_encoding)
           error)
  | `Binary (`Error (Some error)) -> (
      match Data_encoding.Binary.of_string Error_monad.trace_encoding error with
      | Ok trace ->
          cctxt#error
            "@[<v 2>Command failed: @[%a@]@]@."
            Error_monad.pp_print_trace
            trace
      | Error msg ->
          cctxt#error
            "@[<v 2>Error whilst decoding the server response: @[%a@]@]@."
            Data_encoding.Binary.pp_read_error
            msg)
  | `Json (`Not_found _) | `Binary (`Not_found _) | `Other (_, `Not_found _) ->
      cctxt#error "No service found at this URL\n%!"
  | `Json (`Gone _) | `Binary (`Gone _) | `Other (_, `Gone _) ->
      cctxt#error
        "Requested data concerns a pruned block and target resource is no \
         longer available\n\
         %!"
  | `Json (`Unauthorized _)
  | `Binary (`Unauthorized _)
  | `Other (_, `Unauthorized _) ->
      cctxt#error "@[<v 2>[HTTP 403] Access denied to: %a@]@." Uri.pp cctxt#base
  | _ -> cctxt#error "Unexpected server answer\n%!"

let call ?body meth raw_url (cctxt : #Client_context.full) =
  let open Lwt_result_syntax in
  let uri = Uri.of_string raw_url in
  let args = String.split_no_empty '/' (Uri.path uri) in
  if not cctxt#verbose_rpc_error_diagnostics then
    let body =
      (* This code is similar to a piece of code in [fill_in]
         function. An RPC is declared as POST, PATCH or PUT, but the
         body is not given. In that case, the body should be an empty
         JSON object. *)
      match (meth, body) with
      | (_, Some _) -> body
      | (`DELETE, None) | (`GET, None) -> None
      | (`PATCH, None) | (`PUT, None) | (`POST, None) -> Some (`O [])
    in
    let* answer = cctxt#generic_media_type_call meth ?body uri in
    let*! () = display_answer cctxt answer in
    return_unit
  else
    let* s = RPC_description.describe cctxt ~recurse:false args in
    match s with
    | Static {services; _} -> (
        match RPC_service.MethMap.find_opt meth services with
        | None -> no_service_at_valid_prefix cctxt
        | Some {input = None; _} ->
            let*! () =
              match body with
              | None -> Lwt.return_unit
              | Some _ ->
                  cctxt#warning
                    "This URL did not expect a JSON input but one was provided\n\
                     %!"
            in
            let* answer = cctxt#generic_media_type_call meth ?body uri in
            let*! () = display_answer cctxt answer in
            return_unit
        | Some {input = Some input; _} -> (
            let*! r =
              match body with
              | None -> fill_in ~show_optionals:false (fst (Lazy.force input))
              | Some body -> Lwt.return (Ok body)
            in
            match r with
            | Error msg -> cctxt#error "%s" msg
            | Ok body ->
                let* answer = cctxt#generic_media_type_call meth ~body uri in
                let*! () = display_answer cctxt answer in
                return_unit))
    | _ -> cctxt#error "No service found at this URL\n%!"

let call_with_json meth raw_url json (cctxt : #Client_context.full) =
  match Data_encoding.Json.from_string json with
  | exception Assert_failure _ ->
      (* Ref : https://github.com/mirage/ezjsonm/issues/31 *)
      cctxt#error "Failed to parse the provided json: unwrapped JSON value.\n%!"
  | Error err -> cctxt#error "Failed to parse the provided json: %s\n%!" err
  | Ok body -> call meth ~body raw_url cctxt

let call_with_file_or_json meth url maybe_file (cctxt : #Client_context.full) =
  let open Lwt_result_syntax in
  let* json =
    match TzString.split_exact ':' ~limit:1 maybe_file with
    | ["file"; filename] ->
        Lwt.catch
          (fun () ->
            Lwt_result.ok @@ Lwt_io.(with_file ~mode:Input filename read))
          (fun exn -> failwith "cannot read file (%s)" (Printexc.to_string exn))
    | _ -> return maybe_file
  in
  call_with_json meth url json cctxt

let meth_params ?(name = "HTTP method") ?(desc = "") params =
  param
    ~name
    ~desc
    (parameter
       ~autocomplete:(fun _ ->
         Lwt.return_ok
         @@ List.map String.lowercase_ascii
         @@ List.map Resto.string_of_meth
         @@ [`GET; `POST; `DELETE; `PUT; `PATCH])
       (fun _ name ->
         let open Lwt_tzresult_syntax in
         match Resto.meth_of_string (String.uppercase_ascii name) with
         | None -> failwith "Unknown HTTP method: %s" name
         | Some meth -> return meth))
    params

let group = {Clic.name = "rpc"; title = "Commands for the low level RPC layer"}

let commands =
  [
    command
      ~group
      ~desc:
        "List RPCs under a given URL prefix.\n\
         Some parts of the RPC service hierarchy depend on parameters,\n\
         they are marked by a suffix `<dynamic>`.\n\
         You can list these sub-hierarchies by providing a concrete URL prefix \
         whose arguments are set to a valid value."
      no_options
      (prefixes ["rpc"; "list"]
      @@ string ~name:"url" ~desc:"the URL prefix"
      @@ stop)
      (fun () -> list);
    command
      ~group
      ~desc:"Alias to `rpc list /`."
      no_options
      (prefixes ["rpc"; "list"] @@ stop)
      (fun () -> list "/");
    command
      ~group
      ~desc:"Get the input and output JSON schemas of an RPC."
      no_options
      (prefixes ["rpc"; "schema"]
      @@ meth_params
      @@ string ~name:"url" ~desc:"the RPC url"
      @@ stop)
      (fun () -> schema);
    command
      ~group
      ~desc:"Get the humanoid readable input and output formats of an RPC."
      (args1 (switch ~doc:"Binary format" ~short:'b' ~long:"binary" ()))
      (prefixes ["rpc"; "format"]
      @@ meth_params
      @@ string ~name:"url" ~desc:"the RPC URL"
      @@ stop)
      format;
    command
      ~group
      ~desc:"Call an RPC with the GET method."
      no_options
      (prefixes ["rpc"; "get"] @@ string ~name:"url" ~desc:"the RPC URL" @@ stop)
      (fun () -> call `GET);
    command
      ~group
      ~desc:
        "Call an RPC with the POST method.\n\
         It invokes $EDITOR if input data is needed."
      no_options
      (prefixes ["rpc"; "post"]
      @@ string ~name:"url" ~desc:"the RPC URL"
      @@ stop)
      (fun () -> call `POST);
    command
      ~group
      ~desc:
        "Call an RPC with the POST method, providing input data via the \
         command line."
      no_options
      (prefixes ["rpc"; "post"]
      @@ string ~name:"url" ~desc:"the RPC URL"
      @@ prefix "with"
      @@ string
           ~name:"input"
           ~desc:
             "the raw JSON input to the RPC\n\
              For instance, use `{}` to send the empty document.\n\
              Alternatively, use `file:path` to read the JSON data from a file."
      @@ stop)
      (fun () -> call_with_file_or_json `POST);
    command
      ~group
      ~desc:
        "Call an RPC with the PATCH method.\n\
         It invokes $EDITOR if input data is needed."
      no_options
      (prefixes ["rpc"; "patch"]
      @@ string ~name:"url" ~desc:"the RPC URL"
      @@ stop)
      (fun () -> call `PATCH);
    command
      ~group
      ~desc:
        "Call an RPC with the PATCH method, providing input data via the \
         command line."
      no_options
      (prefixes ["rpc"; "patch"]
      @@ string ~name:"url" ~desc:"the RPC URL"
      @@ prefix "with"
      @@ string
           ~name:"input"
           ~desc:
             "the raw JSON input to the RPC\n\
              For instance, use `{}` to send the empty document.\n\
              Alternatively, use `file:path` to read the JSON data from a file."
      @@ stop)
      (fun () -> call_with_file_or_json `PATCH);
    command
      ~group
      ~desc:
        "Call an RPC with the PUT method.\n\
         It invokes $EDITOR if input data is needed."
      no_options
      (prefixes ["rpc"; "put"] @@ string ~name:"url" ~desc:"the RPC URL" @@ stop)
      (fun () -> call `PUT);
    command
      ~group
      ~desc:
        "Call an RPC with the PUT method, providing input data via the command \
         line."
      no_options
      (prefixes ["rpc"; "put"]
      @@ string ~name:"url" ~desc:"the RPC URL"
      @@ prefix "with"
      @@ string
           ~name:"input"
           ~desc:
             "the raw JSON input to the RPC\n\
              For instance, use `{}` to send the empty document.\n\
              Alternatively, use `file:path` to read the JSON data from a file."
      @@ stop)
      (fun () -> call_with_file_or_json `PUT);
    command
      ~group
      ~desc:"Call an RPC with the DELETE method."
      no_options
      (prefixes ["rpc"; "delete"]
      @@ string ~name:"url" ~desc:"the RPC URL"
      @@ stop)
      (fun () -> call `DELETE);
  ]
