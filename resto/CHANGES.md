dev

* Added `Server.normalize_request_target`, which rewrites absolute-form
  request targets (`http://host/path`) into their origin-form equivalent,
  and made `launch` and `resto_callback` use it: absolute-form targets are
  now handled exactly as their origin-form equivalent. A custom callback
  given to `launch` now receives the normalized request. As required by
  RFC 9112 §3.2.2, the `Host` header of such a request is replaced by the
  host and port of its target.
* Made `Cors.check_host` compare hosts case-insensitively (RFC 3986
  §3.2.2), so that its decision does not depend on the request target form.
* Fixed a file descriptor leak in the client when following redirects: the
  body of each intermediate redirect response is now drained so that its
  connection is released.
* Enabled TCP keepalive on connections accepted by servers, so that
  connections whose peer disappeared without closing are eventually
  closed instead of being retained until the process restarts.
* Fixed a file descriptor leak in the client when calling streamed RPCs.

v1.2

* Fixed streaming RPCs in servers to be closed as soon as the clients
  leave (instead of waiting for the next flush)
* Added the possibility to specify a conn_closed hook function at the
  creation of servers

v1.1

* fix: The client do not wait for a first element of a stream to arrive.

v1.0

* refactoring: Invert control over resto server
* feature: client follow redirects

v0.10:

feature: Merge for dynamic and dynamic tails directories

v0.9:

* feature: Added a strategy to pick on the left or on the right on a directory merging collision.
* feature: Added printers for `Conflict` exception.

0.8:

* Improve ocaml version coverage in CI
* Added helpers for pretty-printing Paths
* Added service-lookup feature to resolve parametric URI from concrete URI

v0.7:

* Remove Ezresto (and other Ez libraries)
* Packaging, build, tests and CI improvements
* Add the `middleware` feature (contributed by Swann Moreau)

v0.6/v0.6.1:

* Split path before percentage-decoding chunks to allow slash's encoding to
  appear in chunks
* Added ACL module to allow/deny access to entry points based on path matching
* The boolean parameter now accepts an empty string, in which case the value is
  considered `true`
* Added support for self-serving client
* Added support for chunking answers
* Allow to specify agent-string
* More tests and better documentation
* More logging with better level settings

v0.5:

* Documentation
* Updated dependencies (notably Lwt)

v0.4:

* client does not depend on Unix anymore. Users must pass `gettimeofday`
  function directly.
* Added `Gone response code

v0.3:

* Schemas are now lazy (to speed up initialisation and because schemas are not
  always used) (by Romain)
- depend on json-data-encoding (new) fork of ocplib-json-typed (deprecated)

v0.2:

- fork from ocplib-resto: new home, new CI, etc.
