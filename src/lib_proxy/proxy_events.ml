include Internal_event.Simple

let section = ["proxy_rpc_ctxt"]

let level = Internal_event.Debug

let delegate_to_http =
  declare_3
    ~section
    ~level
    ~name:"delegate_to_http"
    ~msg:"delegating to http: {method} {name} {path}"
    ("method", Data_encoding.string)
    ("name", Data_encoding.string)
    ("path", Data_encoding.string)

let done_locally =
  declare_3
    ~section
    ~level
    ~name:"done_locally"
    ~msg:"locally done: {method} {name} {path}"
    ("method", Data_encoding.string)
    ("name", Data_encoding.string)
    ("path", Data_encoding.string)

let delegate_json_call_to_http =
  declare_2
    ~section
    ~level
    ~name:"delegate_json_call_to_http"
    ~msg:"delegating to http generic json call: {method} {uri}"
    ("method", Data_encoding.string)
    ("uri", Data_encoding.string)

let done_json_call_locally =
  declare_2
    ~section
    ~level
    ~name:"done_json_call_locally"
    ~msg:"locally done generic json call: {method} {uri}"
    ("method", Data_encoding.string)
    ("uri", Data_encoding.string)

let delegate_media_type_call_to_http =
  declare_2
    ~section
    ~level
    ~name:"delegate_media_type_call_to_http"
    ~msg:"delegating to http generic media type call: {method} {uri}"
    ("method", Data_encoding.string)
    ("uri", Data_encoding.string)

let done_media_type_call_locally =
  declare_2
    ~section
    ~level
    ~name:"done_media_type_call_locally"
    ~msg:"locally done generic media type call: {method} {uri}"
    ("method", Data_encoding.string)
    ("uri", Data_encoding.string)
