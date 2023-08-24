module TzStdLib = Tezos_lwt_result_stdlib.Lwtreslib.Bare
open Source
open Ast

module Unknown = Error.Make ()

exception Unknown = Unknown.Error (* indicates unknown import name *)

module Registry = Map.Make (struct
  type t = Ast.name

  let compare = String.compare
end)

let registry = ref Registry.empty

let register ~module_name lookup =
  registry := Registry.add module_name lookup !registry

let lookup (im : import) : Instance.extern Lwt.t =
  let {module_name; item_name; _} = im.it in
  Lwt.catch
    (fun () -> Registry.find module_name !registry item_name)
    (function
      | Not_found ->
          Unknown.error
            im.at
            ("unknown import \"" ^ module_name ^ "\".\"" ^ item_name ^ "\"")
      | exn -> Lwt.reraise exn)

let link m =
  let open Lwt.Syntax in
  let* imports = Lazy_vector.Int32Vector.to_list m.it.imports in
  TzStdLib.List.map_s lookup imports
