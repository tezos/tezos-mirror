(** Local “Pervasives” module for flextesa.

See also ["docs/tutorials/flextesa.rst"]. *)

module List = Base.List
module String = Base.String
module Option = Base.Option
module Int = Base.Int
module Float = Base.Float
module Exn = Base.Exn

let ( // ) = Filename.concat
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
  let pr f = custom (fun ppf -> f (Format.fprintf ppf))
  let desc_list s l = label s (list ~sep:"," l)
  let desc s v = label s v
  let af ?param fmt = Format.kasprintf (atom ?param) fmt

  let wrap s =
    String.split ~on:' ' s |> List.map ~f:String.strip
    |> List.filter ~f:(( <> ) "")
    |> List.map ~f:atom |> list

  let wf fmt = Format.kasprintf wrap fmt
  let haf fmt = Format.kasprintf highlight fmt
  let opt f = function None -> atom "-" | Some o -> f o
  let ocaml_string_list l = ocaml_list (ListLabels.map l ~f:(af "%S"))
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
    Option.iter (Sys.getenv_opt "FLEXTESA_DEBUG") ~f:(function
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
      Printf.eprintf "\n%!" )

  let i (e : EF.t) = ignore e
  let f f = e (EF.pr f)
  let any v = Dum.to_eformat v
  let pp_any fmt v = Dum.to_formatter fmt v
end

(** An “decorated result type” based on polymorphic variants *)
module Attached_result = struct
  type content =
    [`Text of string | `String_value of string | `Verbatim of string list]

  type ('ok, 'error) t =
    {result: ('ok, 'error) result; attachments: (string * content) list}
    constraint 'error = [> ]

  let ok ?(attachments = []) o = {result= Ok o; attachments}
  let error ?(attachments = []) o = {result= Error o; attachments}

  let pp ppf ?pp_ok ?pp_error {result; attachments} =
    let open Format in
    pp_open_hovbox ppf 4 ;
    ( match result with
    | Ok o ->
        pp_open_hvbox ppf 2 ;
        pp_open_tag ppf "success" ;
        pp_print_string ppf "OK" ;
        pp_close_tag ppf () ;
        Option.iter pp_ok ~f:(fun pp -> pp ppf o) ;
        pp_close_box ppf () ;
        ()
    | Error e ->
        pp_open_hvbox ppf 2 ;
        pp_open_tag ppf "shout" ;
        pp_print_string ppf "ERROR:" ;
        pp_print_space ppf () ;
        pp_close_tag ppf () ;
        Option.iter pp_error ~f:(fun pp -> pp ppf e) ;
        pp_close_box ppf () ) ;
    ( match attachments with
    | [] -> ()
    | more ->
        pp_open_vbox ppf 4 ;
        List.iter more ~f:(fun (k, v) ->
            pp_print_cut ppf () ;
            pp_open_hovbox ppf 2 ;
            pp_print_string ppf "* " ;
            fprintf ppf "%s:@ " k ;
            ( match v with
            | `Text s -> pp_print_text ppf s
            | `String_value s -> fprintf ppf "%S" s
            | `Verbatim lines ->
                pp_open_vbox ppf 0 ;
                pp_print_cut ppf () ;
                fprintf ppf "```````````````" ;
                List.iter lines ~f:(fun s ->
                    pp_print_cut ppf () ; pp_print_string ppf s) ;
                pp_print_cut ppf () ;
                fprintf ppf "```````````````" ;
                pp_close_box ppf () ) ;
            pp_close_box ppf ()) ;
        pp_close_box ppf () ) ;
    pp_close_box ppf ()
end

(** A wrapper around [('ok, 'a Error.t) result Lwt.t]. *)
module Asynchronous_result = struct
  open Attached_result

  type ('ok, 'error) t = ('ok, 'error) Attached_result.t Lwt.t

  let return o : (_, _) t = Lwt.return (ok o)

  let yield () =
    (* https://github.com/ocsigen/lwt/issues/631 *)
    if false then Lwt_unix.auto_yield 0.005 () else Lwt_main.yield ()

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
          ; attachments= List.dedup_and_sort ~compare (attachments @ attach) }

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
      -> f:(('ok, 'error) result -> ('ok2, 'error2) t)
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
    let ( >>= ) = bind let return = return let fail = fail
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
                  Lwt.fail Not_found)
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
          Lwt_unix.yield ()
          >>= fun () ->
          Dbg.e EF.(wf "Lwt_main.run") ;
          r ())
    with
    | {result= Ok (); _} -> exit 0
    | {result= Error (`Die ret); _} -> exit ret
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

  let fail_fatalf ?attach fmt =
    Format.kasprintf (fun e -> fail_fatal ?attach (Message e)) fmt

  let pp fmt (e : [< t]) =
    match e with
    | `System_error (`Fatal, e) ->
        Format.fprintf fmt "@[<2>Fatal-system-error:@ %a@]"
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
    type output = t
    type t = [`Wrong_status of output * string]

    let wrong_status (res : output) msgf =
      ksprintf (fun msg -> fail (`Wrong_status (res, msg) : [> t])) msgf

    let pp fmt = function
      | (`Wrong_status (res, msg) : [< t]) ->
          Format.(
            fprintf fmt "Process-error, wrong status:@ '%s':@ %s"
              (status_to_string res#status)
              msg ;
            fprintf fmt "@.```out@." ;
            List.iter res#out ~f:(fprintf fmt "  | %s@.") ;
            fprintf fmt "@.```@." ;
            fprintf fmt "@.```err@." ;
            List.iter res#err ~f:(fprintf fmt "  | %s@.") ;
            fprintf fmt "@.```@.")

    let fail_if_non_zero (res : output) msg =
      if res#status <> Unix.WEXITED 0 then
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
    | `O l -> `O (List.filter l ~f:(fun (k, _) -> k <> name))
    | other ->
        ksprintf failwith "Jqo.remove_field %S: No an object: %s" name
          (to_string other)

  let get_string = Ezjsonm.get_string
  let get_strings = Ezjsonm.get_strings
  let get_int = Ezjsonm.get_int
end
