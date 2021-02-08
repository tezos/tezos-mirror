
v0.2:

- fork from ocplib-resto: new home, new CI, etc.

v0.3:

* Schemas are now lazy (to speed up initialisation and because schemas are not
  always used) (by Romain)
- depend on json-data-encoding (new) fork of ocplib-json-typed (deprecated)

v0.4:

* client does not depend on Unix anymore. Users must pass `gettimeofday`
  function directly.
* Added `Gone response code

v0.5:

* Documentation
* Updated dependencies (notably Lwt)

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
