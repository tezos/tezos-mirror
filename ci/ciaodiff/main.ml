(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs. <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

module String_map = Map.Make (String)

let default x = function None -> x | Some x -> x

(* Copied from Tezt.Base *)
let with_open_out file write_f =
  let chan = open_out file in
  try
    write_f chan ;
    close_out chan
  with x ->
    close_out chan ;
    raise x

(* Copied from Tezt.Base *)
let with_open_in file read_f =
  let chan = open_in file in
  try
    let value = read_f chan in
    close_in chan ;
    value
  with x ->
    close_in chan ;
    raise x

(* Copied from Tezt.Base *)
let write_file filename ~contents =
  with_open_out filename @@ fun ch -> output_string ch contents

(* Copied from Tezt.Base *)
let read_file filename =
  with_open_in filename @@ fun ch ->
  let buffer = Buffer.create 512 in
  let bytes = Bytes.create 512 in
  let rec loop () =
    let len = input ch bytes 0 512 in
    if len > 0 then (
      Buffer.add_subbytes buffer bytes 0 len ;
      loop ())
  in
  loop () ;
  Buffer.contents buffer

let parse_yaml_file path =
  match Yaml.yaml_of_string (read_file path) with
  | Ok yaml -> yaml
  | Error (`Msg msg) -> failwith ("failed to parse " ^ path ^ ": " ^ msg)

let output_yaml_file path yaml =
  match Yaml.yaml_to_string yaml with
  | Ok contents -> write_file path ~contents
  | Error (`Msg msg) -> failwith ("failed to output " ^ path ^ ": " ^ msg)

(* [equalities] is a list of keys to be considered equal when sorting.
   It is a map where each binding [x = y] denotes that key [x] is equal to key [y].
   It just makes them take the same spot in the order defined by this [compare_keys]
   function, which in turns makes [git diff] understand that they are to be diff'd
   with each other. If both [x] and [y] are present they will still both be present;
   no information is lost. This is useful when e.g. renaming a job and moving it to
   another place in the file. *)
let compare_keys equalities a b =
  let a = String_map.find_opt a equalities |> default a in
  let b = String_map.find_opt b equalities |> default b in
  String.compare a b

let rec reorder equalities : Yaml.yaml -> Yaml.yaml = function
  | (`Scalar _ | `Alias _ | `A _) as x -> x
  | `O mapping ->
      let m_members =
        mapping.m_members
        |> List.map (fun (key, value) ->
               match key with
               | `Scalar s -> (s, value)
               | _ -> failwith "expected a scalar")
        |> List.sort (fun ((k1 : Yaml.scalar), _) (k2, _) ->
               compare_keys equalities k1.value k2.value)
        |> List.map (fun (key, value) ->
               (`Scalar key, reorder equalities value))
      in
      `O {mapping with m_members}

let with_temp_file prefix f =
  let filename = Filename.temp_file prefix ".yml" in
  Fun.protect ~finally:(fun () ->
      if Sys.file_exists filename then Sys.remove filename)
  @@ fun () -> f filename

let echo x = Printf.ksprintf print_endline x

let command ?stdout name args =
  let cmd = Filename.quote_command ?stdout name args in
  echo "Run: %s" cmd ;
  Sys.command cmd

let get_file_from_file_or_pipeline file_or_pipeline =
  if Filename.check_suffix file_or_pipeline ".yml" then file_or_pipeline
  else ".gitlab/ci/pipelines/" ^ file_or_pipeline ^ ".yml"

let () =
  Clap.description
    "Run 'git diff' on a YAML file, but after tweaking the YAML file to make \
     the diff easier to read. Only the order of entries in objects is \
     modified. This tool assumes the files are pipeline files from \
     '.gitlab/ci/pipelines'." ;
  let was_file_or_pipeline =
    Clap.optional_string
      ~long:"was"
      ~description:
        "Name that FILE_OR_PIPELINE had in --origin, if it was renamed since. \
         Defaults to FILE_OR_PIPELINE."
      ()
  in
  let origin =
    Clap.default_string
      ~long:"origin"
      ~description:
        "Git reference to the old commit to compare FILE_OR_PIPELINE to."
      "master"
  in
  let file_or_pipeline =
    Clap.mandatory_string
      ~placeholder:"FILE_OR_PIPELINE"
      ~description:
        "Path to the YAML file to diff. If it doesn't end with '.yml', the \
         script assumes this is a pipeline name instead, and the actual path \
         that it uses is '.gitlab/ci/pipelines/FILE_OR_PIPELINE.yml'."
      ()
  in
  let equalities =
    let typ =
      Clap.typ
        ~name:"equality"
        ~dummy:("", "")
        ~parse:(fun s ->
          match String.split_on_char '=' s with
          | [a; b] -> Some (a, b)
          | _ -> None)
        ~show:(fun (a, b) -> a ^ "=" ^ b)
    in
    Clap.list
      typ
      ~placeholder:"KEY1=KEY2"
      ~description:
        "Pairs of keys to be considered to be equal. Typically used for job \
         names when jobs have been renamed. This makes the two keys equal \
         during sorting and can thus improve the readability of the diff."
      ()
    |> String_map.of_list
  in
  Clap.close () ;

  let was_file_or_pipeline = was_file_or_pipeline |> default file_or_pipeline in
  let file = get_file_from_file_or_pipeline file_or_pipeline in
  let was = get_file_from_file_or_pipeline was_file_or_pipeline in

  let before =
    with_temp_file "before" @@ fun filename ->
    (match command ~stdout:filename "git" ["show"; origin ^ ":" ^ was] with
    | 0 -> ()
    | code -> echo "git show returned exit code %d" code) ;
    echo "Parse: before" ;
    parse_yaml_file filename
  in
  echo "Parse: after" ;
  let after = parse_yaml_file file in
  echo "Done parsing." ;

  with_temp_file "before" @@ fun before_filename ->
  with_temp_file "after" @@ fun after_filename ->
  echo "Write: before" ;
  output_yaml_file before_filename (reorder equalities before) ;
  echo "Write: after" ;
  output_yaml_file after_filename (reorder equalities after) ;
  echo "If the next command hangs, make sure you are not using dune 3.19.0." ;
  match
    command "git" ["diff"; "--no-index"; "--"; before_filename; after_filename]
  with
  | 0 -> echo "Empty diff."
  | 1 -> ()
  | code -> echo "git diff returned exit code %d" code
