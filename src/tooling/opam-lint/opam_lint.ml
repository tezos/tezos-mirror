(* This tool is used to normalized opam files that are modified by
   opam using opam admin commands.

   Different version of opam can generate different output. We try to
   minimize the noise by formating the files independently of the opam
   version used.

   This is important when ugprading the opam-repository because we
   check that the content of the repo is the one we expect. The
   difference would result in a test failure in the CI *)

let lint file orig =
  let s =
    try
      let t = OpamParser.FullPos.file file in
      OpamPrinter.FullPos.Preserved.opamfile ~format_from:orig t
    with e ->
      Format.eprintf "Failed to format %s@.%s@." file (Printexc.to_string e) ;
      exit 1
  in
  let oc = open_out file in
  output_string oc s ;
  close_out oc

let usage () =
  Printf.eprintf
    "Usage: %s [OPAM_FILE] [OPAM_FILE_ORIG]\n\n\
     Format [OPAM_FILE] (inplace) using the formatting/layout used in \
     [OPAM_FILE_ORIG].\n"
    Sys.executable_name ;
  exit 1

let is_opam f = Filename.basename f = "opam" || Filename.check_suffix f ".opam"

let () =
  match Array.to_list Sys.argv with
  | [_; file; orig] when is_opam file && is_opam orig -> lint file orig
  | _ -> usage ()
