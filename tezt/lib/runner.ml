(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Nomadic Labs <contact@nomadic-labs.com>                *)
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

type t = {
  id : int;
  address : string;
  ssh_alias : string option;
  ssh_user : string option;
  ssh_port : int option;
  ssh_id : string option;
}

let local_public_ip = ref "127.0.0.1"

let get_local_public_ip () = !local_public_ip

let set_local_public_ip ip = local_public_ip := ip

let next_id = ref 0

let create ?ssh_alias ?ssh_user ?ssh_port ?ssh_id ~address () =
  let id = !next_id in
  incr next_id ;
  {id; address; ssh_alias; ssh_user; ssh_port; ssh_id}

let address ?(hostname = false) ?from runner =
  match (from, runner) with
  | None, None -> if hostname then "localhost" else "127.0.0.1"
  | None, Some host -> host.address
  | Some _peer, None -> get_local_public_ip ()
  | Some peer, Some host ->
      if peer.address = host.address then "127.0.0.1" else host.address

(* With ssh-agent, the environment variables SSH_AGENT_PID and SSH_AUTH_SOCK
   must be added in the environment. *)
let ssh_env () =
  match (Sys.getenv_opt "SSH_AGENT_PID", Sys.getenv_opt "SSH_AUTH_SOCK") with
  | Some agent, Some sock ->
      [|"SSH_AGENT_PID=" ^ agent; "SSH_AUTH_SOCK=" ^ sock|]
  | _ ->
      (* Here, we assume we don't have an agent running. *)
      [||]

module Shell = struct
  type command = {
    local_env : (string * string) list;
    name : string;
    arguments : string list;
  }

  type t =
    | Cmd of command
    | Seq of t * t
    | Echo_pid
    | Redirect_stdout of t * string
    | Redirect_stderr of t * string
    | Or_echo_false of t

  let string_of_env env =
    List.fold_right
      (fun (var, value) str_env -> var ^ "=" ^ value ^ " " ^ str_env)
      env
      ""

  let string_of_cmd cmd =
    let env = string_of_env cmd.local_env in
    let cmd = Log.quote_shell_command cmd.name cmd.arguments in
    env ^ cmd

  let rec to_string ?(context = `top) shell =
    let with_parentheses s =
      match context with
      | `top ->
          (* No need for parentheses in the toplevel context. *)
          s
      | `operator -> "(" ^ s ^ ")"
    in
    let in_operator = to_string ~context:`operator in
    match shell with
    | Cmd cmd ->
        (* Commands do not need parentheses, they have the highest precedence. *)
        string_of_cmd cmd
    | Seq (cmd1, cmd2) ->
        with_parentheses @@ in_operator cmd1 ^ " && " ^ in_operator cmd2
    | Echo_pid -> "echo $$"
    | Redirect_stdout (cmd, file) ->
        with_parentheses @@ in_operator cmd ^ " > " ^ Log.quote_shell file
    | Redirect_stderr (cmd, file) ->
        with_parentheses @@ in_operator cmd ^ " 2> " ^ Log.quote_shell file
    | Or_echo_false cmd ->
        with_parentheses @@ in_operator cmd ^ " || echo false"

  let cmd local_env name arguments = Cmd {local_env; name; arguments}

  let seq cmd_1 cmd_2 = Seq (cmd_1, cmd_2)

  let redirect_stdout shell file = Redirect_stdout (shell, file)

  let redirect_stderr shell file = Redirect_stderr (shell, file)

  let or_echo_false shell = Or_echo_false shell
end

let wrap_with_ssh runner shell =
  let cmd = Shell.to_string shell in
  let ssh_args =
    (match runner.ssh_alias with
    | None -> [runner.address]
    | Some alias -> [alias])
    @ (match runner.ssh_user with None -> [] | Some user -> ["-l"; user])
    @ (match runner.ssh_port with
      | None -> []
      | Some port -> ["-p"; string_of_int port])
    @ match runner.ssh_id with None -> [] | Some id -> ["-i"; id]
  in
  ("ssh", ssh_args @ [cmd])

let wrap_with_ssh_pid runner shell =
  let open Shell in
  wrap_with_ssh
    runner
    (seq Echo_pid (cmd shell.local_env "exec" (shell.name :: shell.arguments)))

module Sys = struct
  type error = {
    address : string option;
    command : string;
    exit_code : Unix.process_status;
    stderr : string;
  }

  exception Remote_error of error

  let show_error {address; command; exit_code; stderr} =
    let address_msg =
      match address with
      | None -> "on localhost"
      | Some address -> "on " ^ address
    in
    let exit_msg =
      match exit_code with
      | Unix.WEXITED code -> "exited with code " ^ string_of_int code
      | Unix.WSIGNALED signal -> "was killed by signal " ^ string_of_int signal
      | Unix.WSTOPPED signal -> "was stopped by signal " ^ string_of_int signal
    in
    let stderr_msg = if stderr = "" then "" else " (" ^ stderr ^ ")" in
    command ^ " " ^ exit_msg ^ stderr_msg ^ " " ^ address_msg ^ "."

  let () =
    Printexc.register_printer @@ function
    | Remote_error error -> Some (show_error error)
    | _ -> None

  (* WARNING: synchronous method so it can block. *)
  let run_unix_with_ssh runner shell =
    let ssh, ssh_args = wrap_with_ssh runner shell in
    let unix_cmd = String.concat " " (ssh :: ssh_args) in
    let ssh_env = ssh_env () in
    Unix.open_process_full unix_cmd ssh_env

  let get_stderr (_, _, stderr) = try input_line stderr with End_of_file -> ""

  let get_stdout (stdout, _, _) = try input_line stdout with End_of_file -> ""

  let file_exists ?runner file =
    match runner with
    | None -> Sys.file_exists file
    | Some runner -> (
        let command = "test" in
        let arguments = ["-e"; file] in
        let shell = Shell.or_echo_false (Shell.cmd [] command arguments) in
        let process = run_unix_with_ssh runner shell in
        let stdout = get_stdout process in
        let stderr = get_stderr process in
        let status = Unix.close_process_full process in
        match status with
        | Unix.WEXITED 0 -> stdout <> "false"
        | error ->
            let error =
              {
                command;
                address = Some runner.address;
                exit_code = error;
                stderr;
              }
            in
            raise (Remote_error error))

  let is_directory ?runner path =
    match runner with
    | None -> Sys.is_directory path
    | Some runner -> (
        let command = "test" in
        let arguments = ["-d"; path] in
        let shell = Shell.or_echo_false (Shell.cmd [] command arguments) in
        let process = run_unix_with_ssh runner shell in
        let stdout = get_stdout process in
        let stderr = get_stderr process in
        let status = Unix.close_process_full process in
        match status with
        | Unix.WEXITED 0 -> stdout <> "false"
        | error ->
            let error =
              {
                command;
                address = Some runner.address;
                exit_code = error;
                stderr;
              }
            in
            raise (Remote_error error))

  let mkdir ?runner ?(perms = 0o755) dir =
    match runner with
    | None -> Unix.mkdir dir perms
    | Some runner -> (
        let command = "mkdir" in
        let arguments = ["-m"; Format.sprintf "%o" perms; dir] in
        let shell = Shell.cmd [] command arguments in
        let process = run_unix_with_ssh runner shell in
        let stderr = get_stderr process in
        let status = Unix.close_process_full process in
        match status with
        | Unix.WEXITED 0 -> ()
        | error ->
            let error =
              {
                command;
                address = Some runner.address;
                exit_code = error;
                stderr;
              }
            in
            raise (Remote_error error))

  let readdir ?runner dir =
    match runner with
    | None -> Sys.readdir dir
    | Some runner -> (
        let command = "ls" in
        let arguments = ["-1"; dir] in
        let shell = Shell.cmd [] command arguments in
        let ((stdout, _, _) as process) = run_unix_with_ssh runner shell in
        let rec read_until_eof input acc =
          try
            let line = input_line input in
            read_until_eof input (line :: acc)
          with End_of_file -> Array.of_list acc
        in
        let files = read_until_eof stdout [] in
        let stderr = get_stderr process in
        let status = Unix.close_process_full process in
        match status with
        | Unix.WEXITED 0 -> files
        | error ->
            let error =
              {
                command;
                address = Some runner.address;
                exit_code = error;
                stderr;
              }
            in
            raise (Remote_error error))

  let remove ?runner file =
    match runner with
    | None -> Sys.remove file
    | Some runner -> (
        let command = "rm" in
        let arguments = [file] in
        let shell = Shell.cmd [] command arguments in
        let process = run_unix_with_ssh runner shell in
        let stderr = get_stderr process in
        let status = Unix.close_process_full process in
        match status with
        | Unix.WEXITED 0 -> ()
        | error ->
            let error =
              {
                command;
                address = Some runner.address;
                exit_code = error;
                stderr;
              }
            in
            raise (Remote_error error))

  let rm_rf runner file =
    let command = "rm" in
    let arguments = ["-rf"; file] in
    let shell = Shell.cmd [] command arguments in
    let process = run_unix_with_ssh runner shell in
    let stderr = get_stderr process in
    let status = Unix.close_process_full process in
    match status with
    | Unix.WEXITED 0 -> ()
    | error ->
        let error =
          {command; address = Some runner.address; exit_code = error; stderr}
        in
        raise (Remote_error error)

  let rmdir ?runner dir =
    match runner with
    | None -> Unix.rmdir dir
    | Some runner -> (
        let command = "rmdir" in
        let arguments = [dir] in
        let shell = Shell.cmd [] command arguments in
        let process = run_unix_with_ssh runner shell in
        let stderr = get_stderr process in
        let status = Unix.close_process_full process in
        match status with
        | Unix.WEXITED 0 -> ()
        | error ->
            let error =
              {
                command;
                address = Some runner.address;
                exit_code = error;
                stderr;
              }
            in
            raise (Remote_error error))

  let mkfifo ?runner ?(perms = 755) pipe =
    match runner with
    | None -> Unix.mkfifo pipe perms
    | Some runner -> (
        let command = "mkfifo" in
        let arguments = ["-m"; Format.sprintf "%o" perms; pipe] in
        let shell = Shell.cmd [] command arguments in
        let process = run_unix_with_ssh runner shell in
        let stderr = get_stderr process in
        let status = Unix.close_process_full process in
        match status with
        | Unix.WEXITED 0 -> ()
        | error ->
            let error =
              {
                command;
                address = Some runner.address;
                exit_code = error;
                stderr;
              }
            in
            raise (Remote_error error))
end
