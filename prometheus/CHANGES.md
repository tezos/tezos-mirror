## Next

- Add a `clear` function to clean all instance of a metric (@rodibozman).
- Add a `clear_specific` function to clean a specific metric (@ArnaudBihan).

## v1.2 (2022-06-16)

- Add lwt collectors and pre-collectors (@killian-delarue, #43).
  Note that this is a temporary feature while we wait for OCaml 5 to be released,
  when this can be replaced by the use of effects.

- Fix deprecations in Fmt 0.8.10 (@MisterDA, #36).

- General build updates, upstream deprecations, etc (@talex5, #33 #34 #35 #40 #42).

## v1.1 (2021-06-08)

- Allow using a custom formatter for log output (@MisterDA #31).
  Windows services crash if they try to use stderr.

## v1.0 (2020-12-22)

- Add logging configuration (#29, @talex5).  
  To configure a server to report counts for log messages:
  ```ocaml
  let () = Prometheus_unix.Logging.init ()
  ```
  This installs a reporter that reports the number of messages logged by each log source and at each level.
  The reporter also displays the timestamp and log source with each message, which is a more suitable configuration for servers.

- Add bounds on cohttp to prepare for cohttp 3 release (#28, @talex5).

## v0.7 (2020-03-03)

- switch float representation to OCaml's default `"%f"` (#22, @toots)
- use `Gc.quick_stat` for faster stats (#25, @talex5)

## v0.6 (2019-11-23)

- upgrade build to dune (@talex5)
- upgrade to opam2 format (@talex5)

## v0.5 (2017-12-20)

- prometheus-app: update to cohttp.1.0.0 API (#15, @djs55)
- add support for histograms (#14, @stijn-devriendt and @talex5)
- add `Sample_set module` to clean up the API a bit (#13, @talex5)
- fix gettimeofday parameter not used in favor of Unix.gettimeofdaya (#12, @stijn-devriendt)

## v0.4 (2017-08-02)

- unix: update to cohttp >= 0.99.0. Note this means the unix package
  requires OCaml 4.03+. The main library still only requires OCaml 4.01+

## v0.3 (2017-07-03)

- Build tweaks to support topkg versioning (@avsm)

## v0.2 (2017-05-18)

- add example program and update README
- switch to jbuilder
- throw a clearer error on registering a duplicate metric
- use `Re` rather than `Str`

## v0.1

- Initial release.
