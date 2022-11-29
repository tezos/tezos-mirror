include Stdlib

module List = struct
  include Stdlib.List

  let map = List_override.map

  let mapi = List_override.mapi

  let append = List_override.append
end
