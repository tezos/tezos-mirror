# Opentelemetry tracing for Cohttp_lwt servers

Wrap your server callback with `Opentelemetry_cohttp_lwt.Server.trace`:

```ocaml
let my_server callback =
  let callback =
    Opentelemetry_cohttp_lwt.Server.trace ~service_name:"my-service" callback in
  Cohttp_lwt_unix.Server.create ~mode:(`TCP (`Port 8080))
    (Server.make () ~callback)
```
