open Internal_pervasives
module IFmt = More_fmt

let failf ?attach fmt =
  ksprintf (fun s -> fail ?attach (`Scenario_error s)) fmt

type display_policy = [`All | `Lines of int | `No | `On_error of display_policy]

let call ?comment ?(expect = `Status `OK) ?(show_body = `All)
    ?(show_response = `On_error `All) ?(how = `Get) state ~api_prefix ~path =
  let http_method, body =
    let json_body json =
      Some (Cohttp_lwt.Body.of_string (Ezjsonm.to_string ~minify:false json))
    in
    match how with
    | `Get -> (`GET, None)
    | `Post_json json -> (`POST, json_body json)
    | `Delete_json json -> (`DELETE, json_body json) in
  let make_body b =
    let json =
      try Ok (Ezjsonm.value_from_string b) with
      | Ezjsonm.Parse_error (_, s) -> Error (sprintf "Error: %s" s)
      | e -> Error (sprintf "???: %s" (Exn.to_string e)) in
    let lines =
      String.split_lines
        ( match json with
        | Ok j -> Ezjsonm.value_to_string ~minify:false j
        | Error _ -> b ) in
    (json, lines) in
  System_error.catch
    Lwt.(
      fun () ->
        Cohttp_lwt_unix.Client.call http_method ?body
          ~headers:(Cohttp.Header.init_with "content-type" "application/json")
          (Uri.of_string (api_prefix // path))
        >>= fun (r, bod) ->
        Cohttp_lwt.Body.to_string bod >>= fun bl -> return (r, make_body bl))
    ()
  >>= fun (coresp, cobody) ->
  ( match expect with
  | `Status status when Poly.equal (Cohttp.Response.status coresp) status ->
      return `Ok
  | `Status status ->
      return
        (`Failed
          (sprintf "API call %s did not return `%s` but `%s`" path
             (Cohttp.Code.string_of_status status)
             (Cohttp.Response.status coresp |> Cohttp.Code.string_of_status)))
  | `Anything -> return `Ok )
  >>= fun test_status ->
  Console.sayf state
    IFmt.(
      let pp_body ?just_lines ppf (json, lines) =
        cut ppf () ;
        pf ppf "* Body (%d lines, %s)" (List.length lines)
          (match json with Ok _ -> "valid JSON" | Error s -> s) ;
        if List.length lines > 0 then (
          pf ppf ":" ;
          cut ppf () ;
          vertical_box ppf ~indent:0 (fun ppf ->
              cut ppf () ;
              string ppf (String.make 60 '`') ;
              List.iter
                ( match just_lines with
                | None -> lines
                | Some n -> List.take lines n )
                ~f:(fun l -> cut ppf () ; string ppf l) ;
              ( match just_lines with
              | Some n when n < List.length lines ->
                  cut ppf () ; string ppf "° ° °"
              | _ -> () ) ;
              cut ppf () ;
              string ppf (String.make 60 '`')) ) in
      let pp_response ppf () =
        cut ppf () ;
        pf ppf "* Response:" ;
        cut ppf () ;
        Sexplib0.Sexp.pp_hum_indent 4 ppf (Cohttp.Response.sexp_of_t coresp)
      in
      let rec un_option_show f = function
        | `On_error _ when Poly.equal test_status `Ok -> ()
        | `On_error l -> un_option_show f l
        | `No -> ()
        | `All -> f None
        | `Lines l -> f (Some l) in
      fun ppf () ->
        vertical_box ppf (fun ppf ->
            pf ppf "# Called %s /%s → %s `%s`:"
              (Cohttp.Code.string_of_method http_method)
              path
              (if Poly.equal test_status `Ok then "Expected" else "UNEXPECTED")
              (Cohttp.Response.status coresp |> Cohttp.Code.string_of_status) ;
            Option.iter comment ~f:(fun c ->
                cut ppf () ; string ppf "* " ; c ppf) ;
            un_option_show (fun _ -> pp_response ppf ()) show_response ;
            un_option_show
              (fun just_lines -> pp_body ?just_lines ppf cobody)
              show_body))
  >>= fun () ->
  match test_status with
  | `Ok ->
      return
        (object
           method body_lines = snd cobody

           method body_json = fst cobody
        end)
  | `Failed s -> failf "%s" s
