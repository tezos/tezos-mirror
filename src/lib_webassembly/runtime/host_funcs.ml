type host_func =
  Input_buffer.t ->
  Instance.module_inst ref ->
  Values.value list ->
  Values.value list Lwt.t

module Registry = Map.Make (String)

type registry = host_func Registry.t ref

let empty () = ref Registry.empty

let register ~global_name implem registry =
  registry := Registry.add global_name implem !registry

let lookup ~global_name registry = Registry.find global_name !registry
