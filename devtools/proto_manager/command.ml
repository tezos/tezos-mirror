(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

type t = {describe : State.t -> State.t; execute : State.t -> State.t}

module FIFO = struct
  type elt = t

  type t = elt list

  let empty = []

  let register c l = c :: l

  let describe l state =
    List.fold_right (fun c state -> c.describe state) l state

  let execute l state = List.fold_right (fun c state -> c.execute state) l state

  let to_command ?desc t =
    let describe =
      match desc with Some describe -> describe | None -> describe t
    in
    {describe; execute = execute t}
end

module Shell = struct
  let create ~__LOC__ ?desc ?error_msg shell =
    let describe =
      match desc with
      | Some desc -> desc
      | None ->
          fun state ->
            Log.printfln "Shell command: %s" (shell state) ;
            state
    in
    let execute state =
      let shell = shell state in
      let status = Sys.command shell in
      if status <> 0 then (
        (match error_msg with
        | Some error_msg -> Log.error "%s" error_msg
        | None -> Log.error "%s failed with %d" shell status) ;
        State.exit ~__LOC__ state) ;
      state
    in
    {describe; execute}
end

module File = struct
  let check_exists ?error_msg get_filename =
    {
      describe =
        (fun state ->
          Log.printfln "Check that %s exists." (get_filename state) ;
          state);
      execute =
        (fun state ->
          if not (Sys.file_exists (get_filename state)) then (
            (match error_msg with
            | None -> Log.error "%s does not exist." (get_filename state)
            | Some error_msg -> Log.error "%s" (error_msg state)) ;
            Stdlib.exit 1) ;
          state);
    }

  let check_not_exist ?error_msg get_filename =
    {
      describe =
        (fun state ->
          Log.printfln "Check that %s does not exist." (get_filename state) ;
          state);
      execute =
        (fun state ->
          if Sys.file_exists (get_filename state) then (
            (match error_msg with
            | None ->
                Log.error
                  "%s already exists, you should remove it."
                  (get_filename state)
            | Some error_msg -> Log.error "%s" (error_msg state)) ;
            Stdlib.exit 1) ;
          state);
    }
end

module Git = struct
  let git ({git_dir; _} : State.t) cmd = "git -C " ^ git_dir ^ " " ^ cmd

  let check_no_uncommitted_changes =
    Shell.create
      ~__LOC__
      ~error_msg:"Git tree is not clean, please commit or stash your changes"
      ~desc:(fun state ->
        Log.printfln "Check that the git repository has no uncommitted changes." ;
        state)
      (fun state ->
        Format.asprintf
          {|if [ $(%s | wc -l) -gt 0 ]; then
               exit 1
             fi|}
          (git state "status --porcelain"))

  let commit_no_hooks message =
    let open FIFO in
    let commands =
      FIFO.empty
      |> register (Shell.create ~__LOC__ (fun state -> git state {|add ."|}))
      |> register
           (Shell.create ~__LOC__ (fun state ->
                (* if pre-commit hooks are enabled, do not run them *)
                git
                  state
                  (Format.asprintf
                     {|commit -m "%s/%s" --no-verify|}
                     state.capitalize_label
                     message)))
      |> register
           {
             describe =
               (fun state ->
                 Log.printfln "Log created commit" ;
                 state);
             execute =
               (fun state ->
                 Log.printfln
                   "@{<blue>Created commit:@{<cyan> %s/%s@}@}"
                   state.capitalize_label
                   message ;
                 state);
           }
      |> register
           {
             describe =
               (fun state ->
                 Log.printfln "increment commit counter" ;
                 {state with commits = state.commits + 1});
             execute = (fun state -> {state with commits = state.commits + 1});
           }
    in
    to_command
      ~desc:(fun state ->
        Log.printfln "commit no hooks with message %s" message ;
        state)
      commands
end
