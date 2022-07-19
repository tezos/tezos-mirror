type host_func =
  Input_buffer.t ->
  Instance.module_inst ref ->
  Values.value list ->
  Values.value list Lwt.t

module Registry = Map.Make (String)

let registry : host_func Registry.t ref = ref Registry.empty

let register ~global_name implem =
  registry := Registry.add global_name implem !registry

let lookup ~global_name = Registry.find global_name !registry
