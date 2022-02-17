open Internal_pervasives

module Process = struct
  type kind =
    [`Process_group | `Docker of string | `Process_group_script of string]

  type t = {id: string; binary: string option; command: string list; kind: kind}

  let make_in_session ?binary id kind command =
    {id; binary; command= "setsid" :: command; kind}

  let genspio id script =
    let script_content =
      Fmt.str "%a" Genspio.Compile.To_slow_flow.Script.pp_posix
        (Genspio.Compile.To_slow_flow.compile script) in
    let command = ["sh"] in
    make_in_session id (`Process_group_script script_content) command

  let docker_run id ~image ~options ~args =
    let name = id in
    let command =
      ["docker"; "run"; "--name"; name; "--rm"] @ options @ [image] @ args
    in
    {id; binary= None; command; kind= `Docker name}
end

module State = struct
  type process_state = {process: Process.t; lwt: Lwt_process.process_none}
  type t = {processes: (string, process_state) Caml.Hashtbl.t}

  let pp fmt {processes} =
    let open Caml.Format in
    fprintf fmt "Processes:@ [@[" ;
    Caml.Hashtbl.iter
      (fun s {lwt; _} -> fprintf fmt "%S:%d" s lwt#pid)
      processes ;
    fprintf fmt "]@]"

  let make () = {processes= Caml.Hashtbl.create 42}
  let processes o = (o#runner : t).processes

  let add_process o process lwt =
    Caml.Hashtbl.add (processes o) process.Process.id {process; lwt} ;
    return ()

  let all_processes t =
    let res = ref [] in
    Caml.Hashtbl.iter (fun _ t -> res := t :: !res) (processes t) ;
    return !res
end

open State
open Process

let output_path t process which =
  let sanitize =
    String.map ~f:(function '\'' | '/' | '"' -> '_' | other -> other) in
  Paths.root t
  // ( "output"
     // sanitize process.Process.id
     //
     match which with
     | `Stdout -> "stdout.log"
     | `Stderr -> "stderr.log"
     | `Meta -> "meta.log"
     | `Script -> "script.sh" )

let ef_processes state processes =
  EF.(
    desc_list (af "Processes")
      (List.map processes ~f:(fun {process; lwt} ->
           desc_list (af "P:%s" process.id)
             [ desc (af "out") (atom (output_path state process `Stdout))
             ; desc (af "err") (atom (output_path state process `Stderr))
             ; desc (af "pid") (af "%d" lwt#pid) ])))

let unix_status_to_string (p : Unix.process_status) =
  match p with
  | Unix.WEXITED i -> sprintf "exited:%d" i
  | Unix.WSIGNALED i -> sprintf "signaled:%d" i
  | Unix.WSTOPPED i -> sprintf "stopped:%d" i

let ef_lwt_state =
  let open Lwt_process in
  function
  | Running -> EF.af "Running" | Exited p -> EF.atom (unix_status_to_string p)

let ef ?(all = false) state =
  EF.(
    let all_procs =
      Caml.Hashtbl.fold
        (fun _ {process; lwt} prev ->
          match (all, lwt#state) with
          | true, _ | false, Lwt_process.Running ->
              ( process.id
              , list ~delimiters:("{", "}")
                  [ haf "%s:" process.id
                  ; desc (af "pid:") (af "%d" lwt#pid)
                  ; desc (af "state:") (ef_lwt_state lwt#state)
                  ; desc (af "kind:")
                      ( match process.kind with
                      | `Docker n -> af "docker:%s" n
                      | `Process_group -> af "process-group"
                      | `Process_group_script _ -> af "shell-script" ) ] )
              :: prev
          | _, _ -> prev)
        (State.processes state) []
      |> List.sort ~compare:(fun (a, _) (b, _) -> String.compare a b)
      |> List.map ~f:snd in
    label (af "Processes:") (list all_procs))

let start t process =
  let date =
    Tezos_base.Time.System.now () |> Tezos_base.Time.System.to_notation
  in
  let open_file f =
    System_error.catch
      ~attach:[("open_file", `String_value f)]
      Lwt.Infix.(
        fun () ->
          Tezos_stdlib_unix.Lwt_utils_unix.create_dir ~perm:0o700
            (Caml.Filename.dirname f)
          >>= fun () ->
          Lwt_unix.file_exists f
          >>= fun exists ->
          ( if exists then Lwt_unix.rename f (sprintf "%s.saved-%s" f date)
          else Lwt.return () )
          >>= fun () ->
          Lwt_unix.(
            openfile f [O_CREAT; O_WRONLY; O_APPEND] 0o600 >|= unix_file_descr))
      () in
  open_file (output_path t process `Stdout)
  >>= fun stdout ->
  open_file (output_path t process `Stderr)
  >>= fun stderr ->
  ( match process.kind with
  | `Process_group | `Docker _ -> return process.command
  | `Process_group_script s ->
      let path = output_path t process `Script in
      System.write_file t path ~content:s
      >>= fun () -> return (process.command @ [path]) )
  >>= fun actual_command ->
  System_error.catch
    (fun () ->
      Lwt_io.with_file ~mode:Lwt_io.output
        ~flags:Unix.[O_CREAT; O_WRONLY; O_APPEND]
        (output_path t process `Meta)
        (fun chan ->
          let msg =
            let sep = String.make 80 '=' in
            sprintf "\n%s\nDate: %s\nStarting: %s\nCmd: [%s]\n%s\n" sep date
              process.Process.id
              ( List.map actual_command ~f:(sprintf "%S")
              |> String.concat ~sep:"; " )
              sep in
          Lwt_io.write chan msg))
    ()
  >>= fun () ->
  let proc =
    Lwt_process.open_process_none ~stdout:(`FD_move stdout)
      ~stderr:(`FD_move stderr)
      (Option.value ~default:"" process.binary, Array.of_list actual_command)
  in
  State.add_process t process proc >>= fun () -> return {process; lwt= proc}

let start_full t process =
  let proc_full =
    Lwt_process.open_process_full
      (Option.value ~default:"" process.binary, Array.of_list process.command)
  in
  let proc = (proc_full :> Lwt_process.process_none) in
  State.add_process t process proc
  >>= fun () ->
  return {process; lwt= proc}
  >>= fun proc_state -> return (proc_state, proc_full)

let wait _t {lwt; _} =
  System_error.catch (fun () -> lwt#close) () >>= fun _status -> return _status

let kill _t {lwt; process} =
  match process.kind with
  | `Process_group | `Process_group_script _ ->
      System_error.catch
        (fun () ->
          Dbg.e EF.(wf "Killing %S" process.id) ;
          let signal = Caml.Sys.sigkill in
          let pid = ~-(lwt#pid) (* Assumes “in session” *) in
          ( try Unix.kill pid signal with
          | Unix.Unix_error (Unix.ESRCH, _, _) -> ()
          | e -> raise e ) ;
          Lwt.return ())
        ()
  | `Docker name -> (
      System_error.catch Lwt_unix.system (sprintf "docker kill %s" name)
      >>= fun status ->
      match status with
      | Lwt_unix.WEXITED 0 -> return ()
      | other ->
          (* Likely already dead *)
          Dbg.e
            EF.(
              desc
                (shout "docker kill failed")
                (list
                   [ af "docker-container: %s" name
                   ; af "status: %s" (unix_status_to_string other) ])) ;
          return () )

let wait_all t =
  State.all_processes t
  >>= fun all ->
  List.fold all ~init:(return ()) ~f:(fun prevm one ->
      prevm >>= fun () -> wait t one >>= fun _ -> return ())

let kill_all t =
  State.all_processes t
  >>= fun all ->
  List.fold all ~init:(return ()) ~f:(fun prevm one ->
      prevm >>= fun () -> kill t one)

let find_process_by_id ?(only_running = false) t ~f =
  State.all_processes t
  >>= fun all ->
  return
    (List.filter all ~f:(fun {process= {id; _}; lwt} ->
         if only_running && not Poly.(lwt#state = Lwt_process.Running) then
           false
         else f id))

let cmds = ref 0

let fresh_id _state prefix ~seed =
  Caml.incr cmds ;
  sprintf "%s-%05d-%s-%08d" prefix !cmds
    Caml.Digest.(string seed |> to_hex)
    Random.(int 10_000_000)

let run_cmdf ?(id_prefix = "cmd") state fmt =
  let get_file path =
    System_error.catch
      Lwt.(
        fun () ->
          Lwt_io.open_file ~mode:Lwt_io.input path
          >>= fun inchan ->
          let stream = Lwt_io.read_lines inchan in
          Lwt_stream.to_list stream
          >>= fun l -> Lwt_io.close inchan >>= fun () -> return l)
      () in
  ksprintf
    (fun s ->
      let id = fresh_id state id_prefix ~seed:s in
      let proc = Process.make_in_session id `Process_group ["sh"; "-c"; s] in
      start state proc
      >>= fun proc ->
      wait state proc
      >>= fun status ->
      get_file (output_path state proc.process `Stdout)
      >>= fun out_lines ->
      get_file (output_path state proc.process `Stderr)
      >>= fun err_lines ->
      return
        (object
           method out = out_lines

           method err = err_lines

           method status = status
        end))
    fmt

let run_successful_cmdf state fmt =
  ksprintf
    (fun cmd ->
      run_cmdf state "%s" cmd
      >>= fun res ->
      Process_result.Error.fail_if_non_zero res
        (sprintf "Shell command: %S" cmd)
      >>= fun () -> return res)
    fmt

let run_genspio state name genspio =
  let proc = Process.genspio (fresh_id state name ~seed:name) genspio in
  start state proc >>= fun proc -> wait state proc

module Async = struct
  let run_cmdf ?(id_base = "async-cmd") state ~f fmt =
    ksprintf
      (fun s ->
        let id = fresh_id state id_base ~seed:s in
        let proc = Process.make_in_session id `Process_group ["sh"; "-c"; s] in
        start_full state proc
        >>= fun (proc_state, proc) ->
        Dbg.e EF.(wf "Started async: %S" proc_state.process.id) ;
        f proc_state proc
        >>= fun res ->
        wait state proc_state >>= fun status -> return (status, res))
      fmt

  let fold_process (process : Lwt_process.process_full) ~init ~f =
    let out = Lwt_io.read_chars process#stdout in
    let err = Lwt_io.read_chars process#stderr in
    let rec go prev_m () =
      prev_m
      >>= fun prev ->
      match (Lwt_stream.get_available out, Lwt_stream.get_available err) with
      | [], [] -> (
          System_error.catch Lwt.choose
            Lwt.
              [ (Lwt_stream.peek out >>= fun x -> return (`Left x))
              ; (Lwt_stream.peek err >>= fun x -> return (`Right x)) ]
          >>= function
          | `Left None -> (
              System_error.catch Lwt_stream.peek err
              >>= function
              | None -> return prev | Some _ -> go (return prev) () )
          | `Right None -> (
              System_error.catch Lwt_stream.peek out
              >>= function
              | None -> return prev | Some _ -> go (return prev) () )
          | _ -> go (return prev) () )
      | outl, errl -> (
          f prev (String.of_char_list outl) (String.of_char_list errl)
          >>= function
          | `Done n -> return n | `Continue next -> go (return next) () ) in
    go (return init) ()
end
