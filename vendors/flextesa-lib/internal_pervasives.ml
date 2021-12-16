(** Local “Pervasives” module for flextesa.

See also ["docs/tutorials/flextesa.rst"]. *)

include Base

let ( // ) = Caml.Filename.concat
let ksprintf, sprintf = Printf.(ksprintf, sprintf)

(** Wrapper around the [EasyFormat] library to use for console display. *)
module EF = struct
  type t = Easy_format.t

  open Easy_format

  let default_list = list
  let default_atom = atom
  let default_label = label
  let atom ?(param = default_atom) s = Atom (s, param)
  let label ?(param = label) a b = Label ((a, param), b)

  let list ?(delimiters = ("", "")) ?(sep = "") ?(param = default_list) l =
    List ((fst delimiters, sep, snd delimiters, param), l)

  let ocaml_list = list ~delimiters:("[", "]") ~sep:";"

  let ocaml_tuple =
    list ~delimiters:("(", ")") ~sep:","
      ~param:
        { default_list with
          space_after_opening= false
        ; space_before_closing= false }

  let shout = atom ~param:{atom_style= Some "shout"}
  let prompt = atom ~param:{atom_style= Some "prompt"}
  let highlight = atom ~param:{atom_style= Some "prompt"}
  let custom pr = Custom pr
  let pr f = custom (fun ppf -> f (Fmt.pf ppf))
  let desc_list s l = label s (list ~sep:"," l)
  let desc s v = label s v
  let af ?param fmt = Fmt.kstr (atom ?param) fmt

  let wrap s =
    String.split ~on:' ' s |> List.map ~f:String.strip
    |> List.filter ~f:(Fn.non (String.equal ""))
    |> List.map ~f:atom |> list

  let wf fmt = Fmt.kstr wrap fmt
  let haf fmt = Fmt.kstr highlight fmt
  let opt f = function None -> atom "-" | Some o -> f o
  let ocaml_string_list l = ocaml_list (List.map l ~f:(af "%S"))
  let exn e = wf "%a" Exn.pp e

  let markdown_verbatim ?(guard_length = 80) s =
    let guard = String.make guard_length '`' in
    af "\n%s\n%s\n%s@." guard s guard

  let ef_json msg json =
    desc (haf "%s" msg)
      (markdown_verbatim Ezjsonm.(to_string ~minify:false (wrap json)))
end

(** Debug-display module (non-cooperative output to [stderr]). *)
module Dbg = struct
  let on = ref false

  let () =
    Option.iter (Caml.Sys.getenv_opt "FLEXTESA_DEBUG") ~f:(function
      | "true" -> on := true
      | _ -> ())

  let e ef =
    if !on then (
      EF.(
        list ~delimiters:("<DBG|", "|DBG>") ~sep:""
          ~param:
            { default_list with
              separator_style= Some "debug"
            ; align_closing= true
            ; space_after_opening= true
            ; space_before_closing= true }
          [ef]
        |> Easy_format.Pretty.to_stderr) ;
      Fmt.epr "\n%!" )

  let i (e : EF.t) = ignore e
  let f f = e (EF.pr f)
  let any v = Dum.to_eformat v
  let pp_any fmt v = Dum.to_formatter fmt v
end

module More_fmt = struct
  include Fmt
  (** Little experiment for fun … *)

  let vertical_box ?indent ppf f = vbox ?indent (fun ppf () -> f ppf) ppf ()
  let wrapping_box ?indent ppf f = box ?indent (fun ppf () -> f ppf) ppf ()

  let wf ppf fmt =
    Fmt.kstr (fun s -> box (fun ppf () -> text ppf s) ppf ()) fmt

  let markdown_verbatim_list ppf l =
    vertical_box ~indent:0 ppf (fun ppf ->
        cut ppf () ;
        string ppf (String.make 45 '`') ;
        List.iter l ~f:(fun l -> cut ppf () ; string ppf l) ;
        cut ppf () ;
        string ppf (String.make 45 '`'))

  let tag tag ppf f =
    Caml.Format.(pp_open_stag ppf (String_tag tag)) ;
    (f ppf : unit) ;
    Caml.Format.pp_close_stag ppf ()

  let shout = tag "shout"
  let prompt = tag "prompt"

  let long_string ?(max = 30) ppf s =
    match String.sub s ~pos:0 ~len:(max - 2) with
    | s -> pf ppf "%S" (s ^ "...")
    | exception _ -> pf ppf "%S" s

  let json ppf json =
    markdown_verbatim_list ppf
      (Ezjsonm.value_to_string ~minify:false json |> String.split ~on:'\n')
end

(** An “decorated result type” based on polymorphic variants *)
module Attached_result = struct
  type content =
    [ `Text of string
    | `String_value of string
    | `Verbatim of string list
    | `String_list of string list ]

  type ('ok, 'error) t =
    {result: ('ok, 'error) Result.t; attachments: (string * content) list}
    constraint 'error = [> ]

  let ok ?(attachments = []) o = {result= Ok o; attachments}
  let error ?(attachments = []) o = {result= Error o; attachments}

  let pp ppf ?pp_ok ?pp_error {result= the_result; attachments} =
    let open More_fmt in
    let result_part ppf =
      match the_result with
      | Ok o ->
          wrapping_box ~indent:4 ppf (fun ppf ->
              prompt ppf (fun ppf -> pf ppf "OK") ;
              Option.iter pp_ok ~f:(fun ppo -> pf ppf ":@ %a." ppo o))
      | Error e ->
          wrapping_box ~indent:4 ppf (fun ppf ->
              shout ppf (fun ppf -> pf ppf "Error") ;
              Option.iter pp_error ~f:(fun ppe -> pf ppf ":@ %a." ppe e)) in
    match attachments with
    | [] -> result_part ppf
    | more ->
        vertical_box ~indent:0 ppf (fun ppf ->
            result_part ppf ;
            List.iter more ~f:(fun (k, v) ->
                cut ppf () ;
                wrapping_box ppf ~indent:2 (fun ppf ->
                    pf ppf "* " ;
                    prompt ppf (fun ppf -> string ppf k) ;
                    match v with
                    | `Text t -> pf ppf ": %a" text t
                    | `String_value s -> pf ppf ": %S" s
                    | `String_list sl ->
                        pf ppf ": [" ;
                        cut ppf () ;
                        wrapping_box ~indent:2 ppf (fun ppf ->
                            list ~sep:sp (fun ppf -> pf ppf "%S;") ppf sl ;
                            pf ppf "]")
                    | `Verbatim [] -> pf ppf ": EMPTY"
                    | `Verbatim sl ->
                        pf ppf ":" ;
                        let lines =
                          let sep = String.make 50 '`' in
                          (sep :: List.drop sl (List.length sl - 20)) @ [sep]
                        in
                        cut ppf () ;
                        vertical_box ~indent:0 ppf (fun ppf ->
                            list ~sep:cut string ppf lines))))
end

(** A wrapper around [('ok, 'a Error.t) result Lwt.t]. *)
module Asynchronous_result = struct
  open Attached_result

  type ('ok, 'error) t = ('ok, 'error) Attached_result.t Lwt.t

  let return o : (_, _) t = Lwt.return (ok o)

  let yield () =
    (* https://github.com/ocsigen/lwt/issues/631 *)
    if false then Lwt_unix.auto_yield 0.005 () else Lwt.pause ()

  let fail ?attach error_value : (_, _) t =
    Lwt.return (error ?attachments:attach error_value)

  (* let error e : (_, _) t = Lwt.return (error e) *)

  let bind (o : (_, _) t) f : (_, _) t =
    let open Lwt.Infix in
    o
    >>= function
    | {result= Ok o; attachments= attach} ->
        yield ()
        >>= fun () ->
        f o
        >>= fun {result; attachments} ->
        Lwt.return {result; attachments= attachments @ attach}
    | {result= Error _; _} as e -> Lwt.return e

  let bind_on_error :
         ('a, 'b) t
      -> f:(   result:('c, 'b) Attached_result.t
            -> 'b
            -> ('a, 'd) Attached_result.t Lwt.t)
      -> ('a, 'd) t =
   fun o ~f ->
    let open Lwt.Infix in
    o
    >>= function
    | {result= Ok _; _} as o -> Lwt.return o
    | {result= Error e; attachments= attach} as res ->
        f ~result:res e
        >>= fun {result; attachments} ->
        Lwt.return
          { result
          ; attachments=
              List.dedup_and_sort ~compare:Poly.compare (attachments @ attach)
          }

  let transform_error o ~f =
    let open Lwt.Infix in
    o
    >>= function
    | {result= Ok _; _} as o -> Lwt.return o
    | {result= Error e; attachments} ->
        Lwt.return {result= Error (f e); attachments}

  let enrich : attachment:(string * content) list -> 'a -> ('b, 'c) t =
   fun ~attachment x ->
    bind_on_error x ~f:(fun ~result _ ->
        Lwt.return
          Attached_result.
            {result with attachments= result.attachments @ attachment})

  let bind_all :
         ('ok, 'error) t
      -> f:(('ok, 'error) Attached_result.t -> ('ok2, 'error2) t)
      -> ('ok2, 'error2) t =
   fun o ~f ->
    let open Lwt.Infix in
    o >>= fun res -> f res

  let bind_on_result :
         ('ok, 'error) t
      -> f:(('ok, 'error) Result.t -> ('ok2, 'error2) t)
      -> ('ok2, 'error2) t =
   fun o ~f ->
    let open Lwt.Infix in
    o
    >>= fun {result; attachments= attach} ->
    f result
    >>= fun {result; attachments} ->
    Lwt.return {result; attachments= attachments @ attach}

  (** The module opened everywhere. *)
  module Std = struct
    let ( >>= ) = bind let ( let* ) = bind let return = return let fail = fail
  end

  open Std

  let run r on_error =
    match Lwt_main.run (r ()) with Ok o -> o | Error e -> on_error e

  let die n = fail (`Die n)

  module List_sequential = struct
    let iter l ~f =
      List.fold l ~init:(return ()) ~f:(fun pm x ->
          pm >>= fun () -> (f x : (_, _) t))

    let iteri l ~f =
      List.fold l ~init:(return 0) ~f:(fun pm x ->
          pm >>= fun n -> (f n x : (_, _) t) >>= fun () -> return (n + 1))
      >>= fun _ -> return ()
  end

  let map_option o ~f =
    match o with
    | None -> return None
    | Some s -> f s >>= fun o -> return (Some o)

  module Loop = struct
    let n_times times f =
      let rec loop n =
        match n with
        | n when n <= 0 -> return ()
        | n -> f (1 + times - n) >>= fun () -> loop (n - 1) in
      loop times
  end

  module Stream = struct
    let fold :
           'elt Lwt_stream.t
        -> f:('b -> 'elt -> ('b, 'error) t)
        -> init:'b
        -> ('b, 'error) t =
     fun stream ~f ~init ->
      let error = ref None in
      Lwt.catch
        (fun () ->
          Lwt_stream.fold_s
            (fun elt prevm ->
              match prevm.result with
              | Ok x -> f x elt
              | Error _ ->
                  error := Some prevm ;
                  Lwt.fail Caml.Not_found)
            stream (Attached_result.ok init))
        (fun e ->
          match !error with
          | Some res -> Lwt.return res
          | None ->
              (* `f` threw a forbidden exception! *)
              Lwt.fail e)
  end

  let run_application r =
    Lwt_main.at_exit
      Lwt.(
        fun () ->
          Dbg.e EF.(wf "Lwt-at-exit: run_application") ;
          return ()) ;
    match
      Lwt_main.run
        Lwt.(
          Lwt.pause ()
          >>= fun () ->
          Dbg.e EF.(wf "Lwt_main.run") ;
          r ())
    with
    | {result= Ok (); _} -> Caml.exit 0
    | {result= Error (`Die ret); _} -> Caml.exit ret
end

include Asynchronous_result.Std
module List_sequential = Asynchronous_result.List_sequential
module Loop = Asynchronous_result.Loop

module System_error = struct
  type static = Exception of exn | Message of string
  type t = [`System_error of [`Fatal] * static]

  let fatal e : [> t] = `System_error (`Fatal, e)
  let fatal_message e : [> t] = `System_error (`Fatal, Message e)
  let fail_fatal ?attach e = fail ?attach (`System_error (`Fatal, e) : [> t])

  let catch ?attach f x =
    Lwt.catch
      (fun () -> Lwt.bind (f x) @@ fun r -> return r)
      (fun exn -> fail_fatal ?attach (Exception exn))

  let catch_exn ?attach f =
    try return (f ()) with exn -> fail_fatal ?attach (Exception exn)

  let fail_fatalf ?attach fmt =
    Fmt.kstr (fun e -> fail_fatal ?attach (Message e)) fmt

  let pp fmt (e : [< t]) =
    match e with
    | `System_error (`Fatal, e) ->
        Fmt.pf fmt "@[<2>Fatal-system-error:@ %a@]"
          (fun ppf -> function Exception e -> Fmt.exn ppf e
            | Message e -> Fmt.string ppf e)
          e
end

(** A wrapper around a structural type describing the result of
    external processes. *)
module Process_result = struct
  type t =
    < err: string list ; out: string list ; status: Unix.process_status >

  let status_to_string s =
    Lwt_unix.(
      match s with
      | WEXITED n -> sprintf "exited with %d" n
      | WSIGNALED n -> sprintf "was signaled: %d" n
      | WSTOPPED n -> sprintf "was stopped: %d" n)

  module Error = struct
    type error =
      | Wrong_status of {status: Unix.process_status; message: string}
      | Wrong_behavior of {message: string}

    type res = t
    type t = [`Process_error of error]

    let make error = (`Process_error error : [> t])
    let fail ?attach error = Asynchronous_result.fail ?attach (make error)

    let wrong_status ?attach result msgf =
      Fmt.kstr
        (fun message ->
          let attach =
            Option.value attach ~default:[]
            @ [ ("stdout", `Verbatim result#out)
              ; ("stderr", `Verbatim result#err) ] in
          fail ~attach (Wrong_status {status= result#status; message}))
        msgf

    let wrong_behavior ?attach msgf =
      Fmt.kstr (fun message -> fail ?attach (Wrong_behavior {message})) msgf

    let pp ppf (`Process_error the_error) =
      let open More_fmt in
      wrapping_box ppf (fun ppf ->
          pf ppf "Process-error:" ;
          sp ppf () ;
          match the_error with
          | Wrong_status {status; message} ->
              text ppf message ;
              pf ppf "; wrong-status: '%s'." (status_to_string status)
          | Wrong_behavior {message} -> text ppf message)

    let fail_if_non_zero (res : res) msg =
      if Poly.( <> ) res#status (Unix.WEXITED 0) then
        wrong_status res "Non-zero exit status: %s" msg
      else return ()
  end
end

(** The state within this library is packed into an open object
    (structural) type, this module just defines the [application_name]
    method. *)
module Base_state = struct
  type base = < application_name: string >
  type 'a t = 'a constraint 'a = < base ; .. >
end

module Manpage_builder = struct
  module Section = struct
    let test_scenario = "TEST SCENARIO OPTIONS"
    let environment_variables = Cmdliner.Manpage.s_environment
    let executables = "EXECUTABLE PATHS"
  end

  module Env_variables = struct
    type doc = Lines of string list

    let man all_known : Cmdliner.Manpage.block list =
      [`S Section.environment_variables]
      @ List.map all_known ~f:(fun (name, Lines lines) ->
            `I (name, String.concat ~sep:"\n" lines))
  end

  module State = struct
    type t =
      { mutable used_sections: (int * string * Cmdliner.Manpage.block list) list
      ; mutable env_variables: (string * Env_variables.doc) list }

    let make () = {used_sections= []; env_variables= []}
    let _state state = state#manpager

    let add_section ?(man = []) state ~rank ~name =
      let t = _state state in
      match
        List.find t.used_sections ~f:(fun (_, n, _) -> String.equal n name)
      with
      | None -> t.used_sections <- (rank, name, man) :: t.used_sections
      | Some _ -> ()

    let all_sections state : Cmdliner.Manpage.block list list =
      let t = _state state in
      List.sort t.used_sections ~compare:Base.Poly.ascending
      |> List.map ~f:(fun (_, s, man) -> `S s :: man)

    let register_env_variable state name lines =
      let t = _state state in
      t.env_variables <- (name, Env_variables.Lines lines) :: t.env_variables
  end

  let section ?man state ~rank ~name =
    State.add_section ?man state ~rank ~name ;
    name

  let section_test_scenario state =
    State.add_section state ~rank:0 ~name:Section.test_scenario ;
    Section.test_scenario

  let make state ~intro_blob extra : Cmdliner.Manpage.block list =
    let sections = State.all_sections state in
    List.concat
      [ [`P intro_blob]; extra; List.concat sections
      ; Env_variables.man state#manpager.State.env_variables
      ; [`S Cmdliner.Manpage.s_options] ]
end

(** Some {!Lwt_unix} functions. *)
module System = struct
  let sleep f = System_error.catch Lwt_unix.sleep f

  let write_file (_state : _ Base_state.t) ?perm path ~content =
    System_error.catch
      (fun () ->
        Lwt_io.with_file ?perm ~mode:Lwt_io.output path (fun out ->
            Lwt_io.write out content))
      ()

  let read_file (_state : _ Base_state.t) path =
    System_error.catch
      (fun () ->
        Lwt_io.with_file ~mode:Lwt_io.input path (fun out -> Lwt_io.read out))
      ()
end

(** WIP [jq]-like manipulation in pure OCaml. *)
module Jqo = struct
  let of_string s = Ezjsonm.from_string s
  let to_string j = Ezjsonm.(to_string (wrap j))
  let of_lines l = Ezjsonm.value_from_string (String.concat ~sep:"\n" l)

  let field ~k = function
    | `O l -> List.Assoc.find_exn l ~equal:String.equal k
    | other -> ksprintf failwith "Jqo.field (%S) in %s" k (to_string other)

  let list_find ~f = function
    | `O l ->
        List.find_map_exn ~f:(fun (_, j) -> if f j then Some j else None) l
    | `A l -> List.find_exn ~f l
    | other -> ksprintf failwith "Jqo.list_find in %s" (to_string other)

  let list_exists ~f o =
    match list_find o ~f with _ -> true | exception _ -> false

  let remove_field o ~name =
    match o with
    | `O l -> `O (List.filter l ~f:Poly.(fun (k, _) -> k <> name))
    | other ->
        ksprintf failwith "Jqo.remove_field %S: No an object: %s" name
          (to_string other)

  let get_string = Ezjsonm.get_string
  let get_strings = Ezjsonm.get_strings
  let get_int = Ezjsonm.get_int
  let get_list = Ezjsonm.get_list (fun e -> e)
end
