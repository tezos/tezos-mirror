open Ctypes
module Types = Api_types

module Functions (S : FOREIGN) = struct
  open S

  let add =
    foreign "octez_risc_v_add" (uintptr_t @-> uintptr_t @-> returning uintptr_t)
end
