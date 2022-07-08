module TzStdLib = Tezos_lwt_result_stdlib.Lwtreslib.Bare
open Source
open Ast

module Unknown = Error.Make ()

exception Unknown = Unknown.Error (* indicates unknown import name *)

module Registry = Map.Make (struct
  type t = Ast.name

  let compare = compare
end)

let registry = ref Registry.empty

let register name lookup = registry := Registry.add name lookup !registry

let lookup (m : module_) (im : import) : Instance.extern Lwt.t =
  let open Lwt.Syntax in
  let {module_name; item_name; idesc} = im.it in
  let* t = import_type m im in
  Lwt.catch
    (fun () -> Registry.find module_name !registry item_name t)
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
  TzStdLib.List.map_s (lookup m) imports
