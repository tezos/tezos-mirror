(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

type t = {describe : State.t -> State.t; execute : State.t -> State.t}

let create ~desc execute = {describe = desc; execute}

let execute c state = c.execute state

let describe c state = c.describe state

module FIFO = struct
  type elt = t

  type t = elt list

  let empty = []

  let describe l state =
    List.fold_right (fun c state -> c.describe state) l state

  let execute l state = List.fold_right (fun c state -> c.execute state) l state
end

let register c l = c :: l

let of_fifo ?desc t =
  let describe =
    match desc with Some describe -> describe | None -> FIFO.describe t
  in
  {describe; execute = FIFO.execute t}

module Shell = struct
  let create ~__LOC__ ?desc ?error_msg shell =
    let desc =
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
    create ~desc execute
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

  let ocamlformat ~__LOC__ files =
    let desc state =
      Log.printfln
        "Format %a"
        Format.(
          pp_print_list
            ~pp_sep:(fun fmt () -> pp_print_string fmt ", ")
            pp_print_string)
        (files state) ;
      state
    in
    let execute state =
      Utils.File.Content.ocamlformat
        ~error:(fun () -> State.exit ~__LOC__ state)
        (files state) ;
      state
    in
    create ~desc execute
end

module Git = struct
  let git state cmd = "git -C " ^ State.get_git_dir state ^ " " ^ cmd

  let check_no_uncommitted_changes ~__LOC__ =
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

  let commit ~__LOC__ ?no_verify get_msg =
    let commands =
      FIFO.empty
      |> register (Shell.create ~__LOC__ (fun state -> git state {|add .|}))
      |> register
           (Shell.create ~__LOC__ (fun state ->
                let no_verify =
                  match no_verify with
                  | Some () ->
                      (* if pre-commit hooks are enabled, do not run them *)
                      " --no-verify"
                  | None -> ""
                in
                git
                  state
                  (Format.asprintf
                     {|commit -m "%s/%s"%s|}
                     (State.Target.get_constructor state)
                     (get_msg state)
                     no_verify)))
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
                   (State.Target.get_constructor state)
                   (get_msg state) ;
                 state);
           }
      |> register
           {
             describe =
               (fun state ->
                 Log.printfln "increment commit counter" ;
                 State.incr_commit_count state);
             execute = (fun state -> State.incr_commit_count state);
           }
    in
    of_fifo
      ~desc:(fun state ->
        Log.printfln "commit no hooks with message %s" (get_msg state) ;
        state)
      commands

  let cp ~__LOC__ get_src get_dest =
    Shell.create
      ~__LOC__
      ~desc:(fun state ->
        Log.printfln "copy versioned files of %s" (get_src state) ;
        state)
      (fun state ->
        git state (Format.asprintf {|archive HEAD "%s"|} (get_src state))
        ^ Format.asprintf {| | tar -x -C "%s"|} (get_dest state))
end

module Log = struct
  let printfln get_msg =
    let desc state =
      Log.printfln "Log: %s" (get_msg state) ;
      state
    in
    create ~desc (fun state ->
        Log.printfln "%s" (get_msg state) ;
        state)

  let cyan get_msg =
    let desc state =
      Log.printfln "Log: %s" (get_msg state) ;
      state
    in
    create ~desc (fun state ->
        Log.cyan "%s" (get_msg state) ;
        state)

  let yellow get_msg =
    let desc state =
      Log.printfln "Log: %s" (get_msg state) ;
      state
    in
    create ~desc (fun state ->
        Log.yellow "%s" (get_msg state) ;
        state)

  let blue get_msg =
    let desc state =
      Log.printfln "Log: %s" (get_msg state) ;
      state
    in
    create ~desc (fun state ->
        Log.blue "%s" (get_msg state) ;
        state)
end
