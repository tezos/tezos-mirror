module TzStdLib = Tezos_lwt_result_stdlib.Lwtreslib.Bare
open Source
open Ast

module Unknown = Error.Make ()

exception Unknown = Unknown.Error (* indicates unknown import name *)

(* TODO: change this to probably use a better representation of module names,
   like hashes for example. *)
module Registry = Map.Make (struct
  type t = Ast.name_list

  let compare = compare
end)

let registry = ref Registry.empty

let from_ast_name name = Lazy_vector.LwtInt32Vector.to_list name

let register name lookup =
  let open Lwt.Syntax in
  let lookup name = lookup (Lazy_vector.LwtInt32Vector.of_list name) in
  let* name = from_ast_name name in
  registry := Registry.add name lookup !registry ;
  Lwt.return_unit

let link_one (m : module_) (im : import) : Instance.extern Lwt.t =
  let open Lwt.Syntax in
  let {module_name; item_name; idesc} = im.it in
  let* module_name_l = from_ast_name module_name in
  let* item_name_l = from_ast_name item_name in
  let* t = import_type m im in
  Lwt.catch
    (fun () -> Registry.find module_name_l !registry item_name_l t)
    (function
      | Not_found ->
          Unknown.error
            im.at
            ("unknown import \"" ^ string_of_name module_name ^ "\".\""
           ^ string_of_name item_name ^ "\"")
      | exn -> raise exn)

let link m =
  let open Lwt.Syntax in
  let* imports = Lazy_vector.LwtInt32Vector.to_list m.it.imports in
  TzStdLib.List.map_s (link_one m) imports

let lookup (module_name : Ast.name) (item_name : Ast.name)
    (t : Types.extern_type) : Instance.extern Lwt.t =
  let open Lwt.Syntax in
  let* module_name_l = from_ast_name module_name in
  let* item_name_l = from_ast_name item_name in
  Registry.find module_name_l !registry item_name_l t
