let name = "wasm"

let version = "2.0"

let configure () =
  let lift lookup name = Lwt.return (lookup name) in
  Import.register ~module_name:"spectest" @@ lift Spectest.lookup ;
  Import.register ~module_name:"env" @@ lift Env.lookup ;
  Spectest.register_host_funcs Run.host_funcs_registry ;
  Env.register_host_funcs Run.host_funcs_registry

let banner () = print_endline (name ^ " " ^ version ^ " reference interpreter")

let usage = "Usage: " ^ name ^ " [option] [file ...]"

let args = ref []

let add_arg source = args := !args @ [source]

let quote s = "\"" ^ String.escaped s ^ "\""

let argspec =
  Arg.align
    [
      ("-e", Arg.String add_arg, " evaluate string");
      ( "-i",
        Arg.String (fun file -> add_arg ("(input " ^ quote file ^ ")")),
        " read script from file" );
      ( "-o",
        Arg.String (fun file -> add_arg ("(output " ^ quote file ^ ")")),
        " write module to file" );
      ( "-w",
        Arg.Int (fun n -> Flags.width := n),
        " configure output width (default is 80)" );
      ("-s", Arg.Set Flags.print_sig, " show module signatures");
      ("-u", Arg.Set Flags.unchecked, " unchecked, do not perform validation");
      ("-h", Arg.Clear Flags.harness, " exclude harness for JS conversion");
      ("-d", Arg.Set Flags.dry, " dry, do not run program");
      ("-t", Arg.Set Flags.trace, " trace execution");
      ("-v", Arg.Unit banner, " show version");
    ]

let run () =
  let open Lwt.Syntax in
  Lwt.catch
    (fun () ->
      configure () ;
      Arg.parse
        argspec
        (fun file -> add_arg ("(input " ^ quote file ^ ")"))
        usage ;
      Lwt_list.iter_s
        (fun arg ->
          let+ res = Run.run_string arg in
          if not res then exit 1)
        !args)
    (fun exn ->
      flush_all () ;
      prerr_endline
        (Sys.argv.(0) ^ ": uncaught exception " ^ Printexc.to_string exn) ;
      Printexc.print_backtrace stderr ;
      exit 2)

let _ =
  Lwt.Exception_filter.(set handle_all_except_runtime) ;
  Lwt_main.run (run ())
