(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2018-2022 Nomadic Labs, <contact@nomadic-labs.com>          *)
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

let protocols =
  (* version, title that appears in the doc, an optional path to an introduction, protocol hash *)
  (* the optional introduction is inserted between the title "RPCs index"
     and the generated directory description *)
  [
    ( "alpha",
      "Alpha",
      Some "/include/rpc_introduction.rst.inc",
      "ProtoALphaALphaALphaALphaALphaALphaALphaALphaDdp3zK" );
    (* TODO tezos/tezos#2170: adapt rest of this list *)
    ( "jakarta",
      "Jakarta",
      Some "/include/rpc_introduction.rst.inc",
      "PtJakart2xVj7pYXJBXrqHgd82rdkLey5ZeeGwDgPp9rhQUbSqY" );
    ( "kathmandu",
      "Kathmandu",
      Some "/include/rpc_introduction.rst.inc",
      "PtKathmankSpLLDALzWw7CGD2j2MtyveTwboEYokqUCP4a1LxMg" );
  ]

let pp_name ppf = function
  | [] | [""] -> Format.pp_print_string ppf "/"
  | prefix -> Format.pp_print_string ppf (String.concat "/" prefix)

let ref_of_service (prefix, meth) =
  Format.asprintf
    "%s_%s"
    (Resto.string_of_meth meth)
    (Re.Str.global_replace
       (Re.Str.regexp "<\\([^>]*\\)>")
       "\\1"
       (String.concat "--" prefix))

(** Encode HTML special characters in string s using escape sequences "&xxx;" *)
let html_encode =
  let rexp = Str.regexp ".*[&><]" in
  fun s ->
    (* ensure no string allocation is done in the common case *)
    if Str.string_match rexp s 0 then
      let s1 = Str.global_replace (Str.regexp "&") "&amp;" s in
      let s2 = Str.global_replace (Str.regexp "<") "&lt;" s1 in
      let s3 = Str.global_replace (Str.regexp ">") "&gt;" s2 in
      s3
    else s

module Index = struct
  let rec pp prefix ppf dir =
    let open Resto.Description in
    match dir with
    | Empty -> Format.fprintf ppf "Empty"
    | Static {services; subdirs = None} -> pp_services prefix ppf services
    | Static {services; subdirs = Some (Suffixes map)} ->
        pp_services prefix ppf services ;
        pp_suffixes prefix ppf (Resto.StringMap.bindings map)
    | Static {services; subdirs = Some (Arg (arg, dir))} ->
        pp_services prefix ppf services ;
        let name = Format.asprintf "<%s>" arg.name in
        pp_suffixes prefix ppf [(name, dir)]
    | Dynamic _ ->
        Format.fprintf ppf "@[<h>* %a (<dynamic>)@]@\n" pp_name prefix

  and pp_suffixes prefix ppf l =
    List.iter (fun (name, dir) -> pp (prefix @ [name]) ppf dir) l

  and pp_services prefix ppf service =
    match Resto.MethMap.bindings service with
    | [] -> ()
    | services ->
        Format.fprintf
          ppf
          "@[<h>* %a (%a)@]@\n"
          pp_name
          prefix
          (Format.pp_print_list
             ~pp_sep:Format.pp_print_space
             (pp_service_method prefix))
          services

  and pp_service_method prefix ppf (meth, _service) =
    Format.fprintf
      ppf
      "`%s <%s_>`_"
      (Resto.string_of_meth meth)
      (ref_of_service (prefix, meth))
end

module Description = struct
  let pp_h ppf n content = Format.fprintf ppf "<h%d>%s</h%d>" n content n

  let pp_p ppf content = Format.fprintf ppf "<p>%s</p>" content

  module Query = struct
    let pp_arg fmt =
      let open RPC_arg in
      function {name; _} -> Format.fprintf fmt "<%s>" name

    let pp_title_item ppf =
      let open RPC_description in
      function
      | {name; kind; _} -> (
          match kind with
          | Single arg | Optional arg ->
              Format.fprintf ppf "[%s=%a]" name pp_arg arg
          | Flag -> Format.fprintf ppf "[%s]" name
          | Multi arg -> Format.fprintf ppf "(%s=%a)*" name pp_arg arg)

    let pp_title ppf query =
      Format.fprintf
        ppf
        "%s%a"
        (if query = [] then "" else "?")
        (Format.pp_print_list
           ~pp_sep:(fun ppf () -> Format.fprintf ppf "&")
           pp_title_item)
        query

    let pp_html_arg fmt =
      let open RPC_arg in
      function {name; _} -> Format.fprintf fmt "&lt;%s&gt;" name

    let pp_item ppf =
      let open RPC_description in
      function
      | {name; description; kind} -> (
          (match kind with
          | Single arg | Optional arg | Multi arg ->
              Format.fprintf
                ppf
                "<span class=\"query\">%s = %a</span>"
                name
                pp_html_arg
                arg
          | Flag -> Format.fprintf ppf "<span class=\"query\">%s</span>" name) ;
          match description with
          | None -> ()
          | Some descr -> Format.fprintf ppf " : %s" (html_encode descr))

    let pp ppf query =
      match query with
      | [] -> ()
      | _ :: _ as query ->
          pp_h ppf 6 "Optional query arguments:" ;
          Format.fprintf
            ppf
            "<ul><li>%a</li></ul>"
            (Format.pp_print_list
               ~pp_sep:(fun ppf () -> Format.fprintf ppf "</li><li>")
               pp_item)
            query
  end

  module Tabs = struct
    let pp_div ?id ?classes ppf f =
      Format.fprintf
        ppf
        "<div%a%a>%a</div>"
        (fun fmt -> function
          | None -> ()
          | Some c -> Format.fprintf fmt " id=\"%s\"" c)
        id
        (fun fmt -> function
          | None -> ()
          | Some cl -> Format.fprintf fmt " class=\"%s\"" (String.concat " " cl))
        classes
        (fun ppf () -> f ppf)
        ()

    let pp_main_description ppf content =
      pp_h ppf 6 "Description" ;
      let content = html_encode content in
      let contents_l = Stdlib.String.trim content |> String.split '\n' in
      List.iter (pp_p ppf) (List.map (Format.sprintf "%s") contents_l)

    let pp_description ppf (service : _ RPC_description.service) =
      pp_div ~classes:["tabcontent"] ppf (fun ppf ->
          (* let open RPC_description in *)
          (* TODO collect and display arg description (in path and in query) *)
          Format.fprintf
            ppf
            "@[<v 2>%a%a@]"
            Format.(pp_print_option pp_main_description)
            service.description
            Query.pp
            service.query)

    let pp_dynamic_tail fmt service =
      List.last_opt service.Resto.Description.path
      |> Option.iter (function
             | Resto.Description.PDynamicTail {name; _} ->
                 Format.fprintf fmt "(/<%s>)*" name
             | _ -> ())

    let pp_service_title ppf prefix service =
      Format.kasprintf
        html_encode
        "%a%a%a"
        pp_name
        prefix
        pp_dynamic_tail
        service
        Query.pp_title
        service.query
      |> Format.fprintf ppf "%s"

    let class_of_method meth =
      String.lowercase_ascii (Resto.string_of_meth meth) ^ "-meth"

    let pp ppf prefix (meth, service) =
      Rst.pp_html ppf (fun ppf ->
          pp_div ppf (fun ppf ->
              pp_div ~classes:["tab"] ppf (fun ppf ->
                  pp_div
                    ~classes:["meth"; class_of_method meth]
                    ppf
                    (fun ppf ->
                      Format.pp_print_string ppf (Resto.string_of_meth meth)) ;
                  pp_div ~classes:["rpc-path"] ppf (fun ppf ->
                      pp_service_title ppf prefix service)) ;
              pp_description ppf service))
  end

  let rec pp prefix ppf dir =
    let open Resto.Description in
    match dir with
    | Empty -> ()
    | Static {services; subdirs = None} -> pp_services prefix ppf services
    | Static {services; subdirs = Some (Suffixes map)} ->
        pp_services prefix ppf services ;
        Format.pp_print_list
          (pp_suffixes prefix)
          ppf
          (Resto.StringMap.bindings map)
    | Static {services; subdirs = Some (Arg (arg, dir))} ->
        let name = Format.asprintf "<%s>" arg.name in
        pp_services prefix ppf services ;
        pp_suffixes prefix ppf (name, dir)
    | Dynamic _ -> ()

  and pp_suffixes prefix ppf (name, dir) = pp (prefix @ [name]) ppf dir

  and pp_services prefix ppf services =
    List.iter (pp_service prefix ppf) (Resto.MethMap.bindings services)

  and pp_service prefix ppf (meth, service) =
    Rst.pp_ref ppf (ref_of_service (prefix, meth)) ;
    Format.fprintf ppf "------------@\n@\n" ;
    Tabs.pp ppf prefix (meth, service)
end

let pp_document ppf descriptions version =
  (* Style : hack *)
  Format.fprintf ppf "%a@\n" Rst.pp_raw_html Rst.style ;
  (* Index *)
  Format.pp_set_margin ppf 10000 ;
  Format.pp_set_max_indent ppf 9000 ;
  let version_suffix = if version = "" then "" else "_" ^ version in
  Format.fprintf ppf "%a" Rst.pp_ref ("rpc_index" ^ version_suffix) ;
  Rst.pp_h1 ppf "RPCs - Reference" ;
  List.iter
    (fun (name, intro, prefix, rpc_dir) ->
      (* If provided, insert the introductory include *)
      Option.iter (Format.fprintf ppf ".. include:: %s@\n@\n") intro ;
      Rst.pp_h3 ppf name ;
      Format.fprintf ppf "%a@\n" (Index.pp prefix) rpc_dir)
    descriptions ;
  (* Full description *)
  Rst.pp_h2 ppf "RPCs - Full description" ;
  Format.pp_print_flush ppf () ;
  Format.pp_set_margin ppf 80 ;
  Format.pp_set_max_indent ppf 76 ;
  List.iter
    (fun (name, _, prefix, rpc_dir) ->
      Rst.pp_h3 ppf name ;
      Format.fprintf ppf "%a@\n@\n" (Description.pp prefix) rpc_dir)
    descriptions

let make_index node required_version =
  let open Lwt_syntax in
  let dir =
    if required_version = "shell" then
      let shell_dir = Node.build_rpc_directory node in
      ("shell", "Shell", Some "/shell/rpc_introduction.rst.inc", [""], shell_dir)
    else
      let make_protocol_index (version, name, intro, hash) =
        let hash = Protocol_hash.of_b58check_exn hash in
        let (module Proto) =
          match Registered_protocol.get hash with
          | Some proto -> proto
          | None ->
              (* This is probably an indication that a line for the
                 requested protocol is missing in the dune file of
                 this repository *)
              Format.kasprintf
                Stdlib.failwith
                "Hash not found: %a"
                Protocol_hash.pp
                hash
        in
        let open RPC_directory in
        let proto_rpc_directory =
          Block_directory.build_raw_rpc_directory (module Proto) (module Proto)
          |> map (fun () -> assert false)
        in
        ( version,
          "Protocol " ^ name,
          intro,
          [".."; "<block_id>"],
          proto_rpc_directory )
      in
      WithExceptions.Option.get ~loc:__LOC__
      @@ List.find
           (fun (version, _name, _intro, _hash) -> version = required_version)
           protocols
      |> make_protocol_index
  in
  let _version, name, intro, path, dir = dir in
  let* dir = RPC_directory.describe_directory ~recurse:true ~arg:() dir in
  let ppf = Format.std_formatter in
  pp_document ppf [(name, intro, path, dir)] required_version ;
  return_ok_unit

let make_default_acl _node =
  let addr_of_string addr = P2p_point.Id.{addr; port = None; peer_id = None} in
  let policy =
    let open Tezos_rpc_http_server.RPC_server.Acl in
    put_policy (addr_of_string "127.0.0.1", allow_all) empty_policy
    |> put_policy (addr_of_string "any.public.address", secure)
    |> Data_encoding.Json.construct policy_encoding
  in
  Data_encoding.Json.pp Format.std_formatter policy ;
  Lwt.return_ok ()

let main node =
  let cmd = Sys.argv.(1) in
  match cmd with
  | "index" -> make_index node Sys.argv.(2)
  | "acl" -> make_default_acl node
  | _ -> raise (Invalid_argument cmd)

let () = Lwt_main.run (Node_helpers.with_node main)
