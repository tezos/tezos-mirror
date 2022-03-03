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
    ( "",
      "Hangzhou",
      Some "/include/rpc_introduction.rst.inc",
      "PtHangz2aRngywmSRGGvrcTyMbbdpWdpFKuS4uMWxg2RaH9i1qx" );
    ( "ithaca",
      "Ithaca",
      Some "/include/rpc_introduction.rst.inc",
      "Psithaca2MLRFYargivpo7YvUr7wUDqyxrdhC5CQq78mRvimz6A" );
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

module Index = struct
  let rec pp prefix ppf dir =
    let open Resto.Description in
    match dir with
    | Empty -> Format.fprintf ppf "Empty"
    | Static {services; subdirs = None} -> pp_services prefix ppf services
    | Static {services; subdirs = Some (Suffixes map)} ->
        Format.fprintf
          ppf
          "@[<v 2>%a@ @ %a@]"
          (pp_services prefix)
          services
          (Format.pp_print_list
             ~pp_sep:(fun ppf () -> Format.fprintf ppf "@ @ ")
             (pp_suffixes prefix))
          (Resto.StringMap.bindings map)
    | Static {services; subdirs = Some (Arg (arg, dir))} ->
        let name = Format.asprintf "<%s>" arg.name in
        Format.fprintf
          ppf
          "@[<v 2>%a@ @ %a@]"
          (pp_services prefix)
          services
          (pp_suffixes prefix)
          (name, dir)
    | Dynamic _ -> Format.fprintf ppf "* %a (<dyn>)" pp_name prefix

  and pp_suffixes prefix ppf (name, dir) = pp (prefix @ [name]) ppf dir

  and pp_services prefix ppf services =
    match Resto.MethMap.bindings services with
    | [] -> Format.fprintf ppf "* %a" pp_name prefix
    | _ :: _ as services ->
        Format.fprintf
          ppf
          "* %a (@[<h>%a@])"
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
          | Multi arg -> Format.fprintf ppf "(%s=%a)\\*" name pp_arg arg)

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
          | Some descr -> Format.fprintf ppf " : %s" descr)

    let pp ppf query =
      match query with
      | [] -> ()
      | _ :: _ as query ->
          Format.fprintf
            ppf
            "</p> <p>Optional query arguments :<ul><li>%a</li></ul>"
            (Format.pp_print_list
               ~pp_sep:(fun ppf () -> Format.fprintf ppf "</li><li>")
               pp_item)
            query
  end

  module Tabs = struct
    let pp_tab_div ppf f =
      Format.fprintf
        ppf
        "@[<v 2><div class=\"tab\">%a</div>@]"
        (fun ppf () -> f ppf)
        ()

    let pp_tabcontent_div ~id ~class_ ppf f =
      Format.fprintf
        ppf
        "@[<v 2><div id=\"%s\" class=\"%s tabcontent\">@ %a@ @]</div>@ "
        id
        class_
        (fun ppf () -> f ppf)
        ()

    let pp_button ppf ?(default = false) ~shortlabel ~content target_ref =
      Format.fprintf
        ppf
        "<button class=\"tablinks%s\" onclick=\"showTab(this, '%s', \
         '%s')\">%s</button>@ "
        (if default then " defaultOpen" else "")
        (target_ref ^ shortlabel)
        target_ref
        content

    let pp_content ppf ~tag ~shortlabel target_ref pp_content content =
      pp_tabcontent_div
        ~id:(target_ref ^ shortlabel)
        ~class_:target_ref
        ppf
        (fun ppf ->
          Format.fprintf ppf "<%s>@ %a</%s>" tag pp_content content tag)

    let pp_description ppf (service : _ RPC_description.service) =
      let open RPC_description in
      (* TODO collect and display arg description (in path and in query) *)
      Format.fprintf
        ppf
        "@[<h>%a@]%a"
        Format.pp_print_text
        (Option.value ~default:"" service.description)
        Query.pp
        service.query

    let pp ppf prefix service =
      let open RPC_description in
      let target_ref = ref_of_service (prefix, service.meth) in
      Rst.pp_html ppf (fun ppf ->
          pp_tab_div ppf (fun ppf ->
              pp_button
                ppf
                ~default:true
                ~shortlabel:"descr"
                ~content:"Description"
                target_ref ;
              Option.iter
                (fun _ ->
                  pp_button
                    ppf
                    ~default:false
                    ~shortlabel:"input.json"
                    ~content:"Json input"
                    target_ref ;
                  pp_button
                    ppf
                    ~default:false
                    ~shortlabel:"input.bin"
                    ~content:"Binary input"
                    target_ref)
                service.input ;
              pp_button
                ppf
                ~default:false
                ~shortlabel:"output.json"
                ~content:"Json output"
                target_ref ;
              pp_button
                ppf
                ~default:false
                ~shortlabel:"output.bin"
                ~content:"Binary output"
                target_ref) ;
          pp_content
            ppf
            ~tag:"p"
            ~shortlabel:"descr"
            target_ref
            pp_description
            service ;
          Option.iter
            (fun input ->
              let (schema, bin_schema) = Lazy.force input in
              pp_content
                ppf
                ~tag:"pre"
                ~shortlabel:"input.json"
                target_ref
                Json_schema.pp
                schema ;
              pp_content
                ppf
                ~tag:"pre"
                ~shortlabel:"input.bin"
                target_ref
                Data_encoding.Binary_schema.pp
                bin_schema)
            service.input ;
          pp_content
            ppf
            ~tag:"pre"
            ~shortlabel:"output.json"
            target_ref
            Json_schema.pp
            (fst (Lazy.force service.output)) ;
          pp_content
            ppf
            ~tag:"pre"
            ~shortlabel:"output.bin"
            target_ref
            Data_encoding.Binary_schema.pp
            (snd (Lazy.force service.output)))
  end

  let pp_dynamic_tail fmt service =
    List.last_opt service.Resto.Description.path
    |> Option.iter (function
           | Resto.Description.PDynamicTail {name; _} ->
               Format.fprintf fmt "(/<%s>)*" name
           | _ -> ())

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
    Format.fprintf
      ppf
      "**%s %a%a%a**@\n@\n"
      (Resto.string_of_meth meth)
      pp_name
      prefix
      pp_dynamic_tail
      service
      Query.pp_title
      service.query ;
    Tabs.pp ppf prefix service
end

let pp_document ppf descriptions version =
  (* Style : hack *)
  Format.fprintf ppf "%a@." Rst.pp_raw_html Rst.style ;
  (* Script : hack *)
  Format.fprintf ppf "%a@." Rst.pp_raw_html Rst.script ;
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
      Format.fprintf ppf "%a@\n@\n" (Index.pp prefix) rpc_dir)
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
  let shell_dir = Node.build_rpc_directory node in
  let protocol_dirs =
    List.map
      (fun (version, name, intro, hash) ->
        let hash = Protocol_hash.of_b58check_exn hash in
        let (module Proto) =
          match Registered_protocol.get hash with
          | None ->
              (* This is probably an indication that a line for the
                 requested protocol is missing in the dune file of
                 this repository *)
              Format.eprintf "Hash not found: %a" Protocol_hash.pp hash ;
              assert false
          | Some proto -> proto
        in
        ( version,
          "Protocol " ^ name,
          intro,
          [".."; "<block_id>"],
          RPC_directory.map (fun () -> assert false)
          @@ Block_directory.build_raw_rpc_directory
               (module Proto)
               (module Proto) ))
      protocols
  in
  let dirs =
    ("shell", "Shell", Some "/shell/rpc_introduction.rst.inc", [""], shell_dir)
    :: protocol_dirs
  in
  let (_version, name, intro, path, dir) =
    WithExceptions.Option.get ~loc:__LOC__
    @@ List.find
         (fun (version, _name, _intro, _path, _dir) ->
           version = required_version)
         dirs
  in
  RPC_directory.describe_directory ~recurse:true ~arg:() dir >>= fun dir ->
  let ppf = Format.std_formatter in
  pp_document ppf [(name, intro, path, dir)] required_version ;
  return ()

let make_default_acl _node =
  let addr_of_string addr = P2p_point.Id.{addr; port = None; peer_id = None} in
  let policy =
    let open Tezos_rpc_http_server.RPC_server.Acl in
    put_policy (addr_of_string "127.0.0.1", allow_all) empty_policy
    |> put_policy (addr_of_string "any.public.address", secure)
    |> Data_encoding.Json.construct policy_encoding
  in
  Data_encoding.Json.pp Format.std_formatter policy ;
  return ()

let main node =
  let cmd = Sys.argv.(1) in
  match cmd with
  | "index" -> make_index node Sys.argv.(2)
  | "acl" -> make_default_acl node
  | _ -> raise (Invalid_argument cmd)

let () = Lwt_main.run (Node_helpers.with_node main)
