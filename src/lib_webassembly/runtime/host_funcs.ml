type host_func =
  Input_buffer.t ->
  Instance.module_inst ref ->
  Values.value list ->
  Values.value list Lwt.t

module Registry = Map.Make (struct
  type t = string * string

  let compare = compare
end)

let registry : host_func Registry.t ref = ref Registry.empty

let register ~module_name ~func_name implem =
  registry := Registry.add (module_name, func_name) implem !registry

let lookup ~module_name ~func_name =
  Registry.find (module_name, func_name) !registry
