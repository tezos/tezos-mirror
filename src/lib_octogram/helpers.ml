(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2023 Nomadic Labs <contact@nomadic-labs.com>                *)
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

type case_maker = {
  mk_case :
    'a 'b.
    string ->
    'a Data_encoding.t ->
    ('b -> 'a option) ->
    ('a -> 'b) ->
    'b Data_encoding.case;
}

let make_mk_case () =
  let tag = ref 0 in
  let next_tag () =
    let res = !tag in
    tag := !tag + 1 ;
    res
  in
  {
    mk_case =
      (fun title encoding f g ->
        Data_encoding.(
          case
            ~title
            (Tag (next_tag ()))
            (conv (fun x -> x) (fun x -> x) encoding)
            f
            g));
  }

let of_json_string enc str =
  Data_encoding.Json.(destruct enc (Result.get_ok @@ from_string str))

let of_json_string_opt enc str =
  match Data_encoding.Json.from_string str with
  | Ok json -> (
      try Some (Data_encoding.Json.destruct enc json) with _ -> None)
  | Error _ -> None

let to_json_string enc value =
  Data_encoding.Json.(
    to_string ~newline:false ~minify:true (construct enc value))

let json_string_of_json json = Ezjsonm.value_to_string ~minify:true json

(** [download url filename] downloads the file at [url],
    stores it in a temporary file named [filename], and returns the complete
    path to the downloaded file.

    The [dir] optional argument can be specified to the directory inside of
    which to save the file. If omitted, the directory is chosen by Tezt.

    The [runner] optional argument can be specified to execute the command on a
    remote runner. If omitted, the command is executed locally. *)
let download ?dir ?runner url filename =
  Log.debug "Download %s" url ;
  let path =
    match dir with
    | Some dir -> dir // filename
    | None -> Temp.file ?runner filename
  in
  let*! _ =
    Curl.get_raw
      ?runner
      ~args:["--fail"; "--keepalive-time"; "2"; "--output"; path]
      url
  in
  Log.debug "%s downloaded" url ;
  Lwt.return path

let exec ?(can_fail = false) ?runner cmd args =
  let process = Process.spawn ?runner cmd args in
  Runnable.run
  @@ Runnable.
       {
         value = process;
         run =
           (fun process ->
             if can_fail then
               let* _ = Process.wait process in
               unit
             else Process.check process);
       }

let mkdir_runnable ?runner ?(p = false) path =
  let process =
    Process.spawn ?runner "mkdir" @@ (if p then ["-p"] else []) @ [path]
  in
  Runnable.{value = process; run = (fun process -> Process.check process)}

let mkdir ?runner ?p path = Runnable.run @@ mkdir_runnable ?runner ?p path

let deploy_runnable ~(runner : Runner.t) ?(r = false) local_file dst =
  let recursive = if r then ["-r"] else [] in
  let identity =
    Option.fold ~none:[] ~some:(fun i -> ["-i"; i]) runner.ssh_id
  in
  let port =
    Option.fold
      ~none:[]
      ~some:(fun p -> ["-P"; Format.sprintf "%d" p])
      runner.ssh_port
  in
  let dst =
    Format.(
      sprintf
        "%s%s:%s"
        (Option.fold ~none:"" ~some:(fun u -> sprintf "%s@" u) runner.ssh_user)
        runner.address
        dst)
  in
  let process =
    Process.spawn
      "scp"
      (*Use -O for original transfer protocol *)
      (["-O"] @ identity @ recursive @ port @ [local_file] @ [dst])
  in
  Runnable.{value = process; run = (fun process -> Process.check process)}

let deploy ~for_runner ?r local_file dst =
  Runnable.run @@ deploy_runnable ~runner:for_runner ?r local_file dst

let when_ b p = if b then p else unit

let change_tmp_directory_if_necessary candidate =
  match candidate with
  | Some name ->
      Filename.set_temp_dir_name
        (Sys.getenv "HOME" // ".octogram" // name
        // Format.asprintf
             "%a"
             (Ptime.pp_rfc3339 ())
             (Tezos_base.Time.System.now ()))
  | None -> ()
