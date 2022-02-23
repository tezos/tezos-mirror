type lib = Hacl | Secp256k1

let files = ref []

let libs = ref []

let args = ref []

let () =
  Arg.parse
    [
      ("--hacl", Unit (fun () -> libs := Hacl :: !libs), "Load hacl-wasm");
      ( "--secp256k1",
        Unit (fun () -> libs := Secp256k1 :: !libs),
        "Load @nomadic-labs/secp256k1wasm" );
      ( "--",
        Rest_all (fun l -> args := List.rev_append l !args),
        "args to pass to the scripts" );
    ]
    (fun a ->
      match Filename.extension a with
      | ".js" -> files := a :: !files
      | _ -> args := a :: !args)
    (Printf.sprintf
       "%s [FLAGS] FILE.js -- args"
       (Filename.basename Sys.executable_name))

let load_js = function
  | Hacl ->
      {|
function load_hacl() {
  /* We have to cheat to avoid the noise from hacl-wasm */
  var old_log = console.log;
  console.log = function () {};
  var loader = require('hacl-wasm');
  console.log = old_log;
  return loader.getInitializedHaclModule().then(function(loaded){
    console.log('hacl loaded');
    global._HACL = loaded})
}
|}
  | Secp256k1 ->
      {|
function load_secp256k1() {
  var loader = require('@nomadic-labs/secp256k1wasm');
  return loader().then(function(loaded) {
    console.log('secp256k1 loaded');
    global._SECP256K1 = loaded})
}
|}

let run i file args =
  let argv = "node" :: file :: args in
  let file =
    if Filename.is_implicit file then Filename.concat "." file else file
  in
  let argv = String.concat ", " (List.map (Printf.sprintf "%S") argv) in
  Printf.sprintf
    {|
function run_%d () {
  console.log('Ready to run %s with argv = [ %s ]');
  var old_argv = process.argv; 
  process.argv = [%s];
  require('%s');
  process.argv = old_argv;
}
|}
    i
    file
    argv
    argv
    file

let () =
  let libs = List.rev !libs in
  let files = List.rev !files in
  let args = List.rev !args in
  let b = Buffer.create 1024 in
  Buffer.add_string
    b
    {|process.on('uncaughtException', function (error) {
   console.log(error.stack);
});|} ;
  List.iter (fun lib -> Buffer.add_string b (load_js lib)) libs ;
  List.iteri (fun i file -> Buffer.add_string b (run i file args)) files ;
  let promises =
    List.map
      (function Hacl -> "load_hacl" | Secp256k1 -> "load_secp256k1")
      libs
  in
  Buffer.add_string b "Promise.resolve('Loading')" ;
  List.iter
    (fun p -> Buffer.add_string b (Printf.sprintf ".then(%s)" p))
    promises ;
  List.iteri
    (fun i _file -> Buffer.add_string b (Printf.sprintf ".then(run_%d)" i))
    files ;
  Buffer.add_string
    b
    ".catch(function (e) { console.log(e); process.exit(1) })\n" ;
  print_newline () ;
  let oc = Unix.open_process_out "node" in
  Buffer.output_buffer oc b ;
  flush_all () ;
  match Unix.close_process_out oc with
  | WEXITED x -> exit x
  | WSIGNALED _ -> exit 1
  | WSTOPPED _ -> exit 1
