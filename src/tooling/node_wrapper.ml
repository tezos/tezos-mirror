let code_for_lib = function
  | "hacl-wasm" ->
      {|
(function () {
  /* We have to cheat to avoid the noise from hacl-wasm */
  var old_log = console.log;
  console.log = function () {};
  var loader = require('hacl-wasm');
  console.log = old_log;
  return loader.getInitializedHaclModule().then(function(loaded){
    console.log('hacl loaded');
    global._HACL = loaded})
})
|}
  | "secp256k1-wasm" ->
      {|
(function () {
  var loader = require('@nomadic-labs/secp256k1-wasm');
  return loader().then(function(loaded) {
    console.log('secp256k1 loaded');
    global._SECP256K1 = loaded})
})
|}
  | "ocaml-bls12-381" ->
      {|
(function () {
  var loader = require('@nomadic-labs/ocaml-bls12-381');
  return loader().then(function(loaded) {
    console.log('bls12-381 loaded');
    global._BLS12381 = loaded})
})
|}
  | _ -> raise Not_found

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

let files = ref []

let codes = ref []

let args = ref []

let () =
  let node_preload_var = "NODE_PRELOAD" in
  let preload =
    try String.split_on_char ',' (Unix.getenv node_preload_var)
    with Not_found -> []
  in
  List.iter
    (fun name ->
      let code =
        try code_for_lib name
        with Not_found ->
          failwith
            (Printf.sprintf
               "Unknown preloading library %S in %s"
               name
               node_preload_var)
      in
      codes := code :: !codes)
    preload ;
  List.iter
    (fun x ->
      match Filename.extension x with
      | ".js" -> files := x :: !files
      | _ -> args := x :: !args)
    (List.tl (Array.to_list Sys.argv))

let () =
  let codes = List.rev !codes in
  let files = List.rev !files in
  let args = List.rev !args in
  let b = Buffer.create 1024 in
  Buffer.add_string
    b
    {|
process.on('uncaughtException', function (error) {
   console.error(error.stack);
});

var major = process.version.match(/^v?([0-9]+)\./)[1];
var minimum_major = 16
if(parseInt(major) < minimum_major){
    console.error("Error: nodejs v" + minimum_major + " or greater is needed. Current version is " + process.version);
    process.exit(1);
}
|} ;
  List.iteri
    (fun i init ->
      Buffer.add_string b (Printf.sprintf "var init_lib_%d = %s" i init))
    codes ;
  List.iteri (fun i file -> Buffer.add_string b (run i file args)) files ;
  Buffer.add_string b "Promise.resolve('Loading')" ;
  List.iteri
    (fun i _ -> Buffer.add_string b (Printf.sprintf ".then(init_lib_%d)" i))
    codes ;
  List.iteri
    (fun i _file -> Buffer.add_string b (Printf.sprintf ".then(run_%d)" i))
    files ;
  Buffer.add_string
    b
    ".catch(function (e) { console.log(e); process.exit(1) })\n" ;
  print_newline () ;
  let () =
    match Unix.getenv "ORIGINAL_PATH" with
    | exception Not_found -> ()
    | path -> Unix.putenv "PATH" path
  in
  let oc = Unix.open_process_out "node" in
  Buffer.output_buffer oc b ;
  flush_all () ;
  match Unix.close_process_out oc with
  | WEXITED x -> exit x
  | WSIGNALED _ -> exit 1
  | WSTOPPED _ -> exit 1
