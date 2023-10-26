v0.1: Initial Release

v0.1.1:  
- customizable initial size for internal buffer
- remove single lwt related function and lwt dependency

v0.2:  
- CI tests
- error management improvements (use result, allow exn and option)
- do not print 0-sized fields in binary descriptions

v0.3:  
- Adapt to json-data-encoding.0.9.1 and provide json-lexeme seq to string seq
- Improved performance
- `maximum_length` to determine static size bounds (when possible)
- provide `to_`/`of_string` alongside `to_`/`of_bytes`
- Improved documentation
- Increase test coverage
- Fix JSON encoding of Result

v0.4:  
- catch exceptions in user-provided functions
- provide `conv_guard` to help enforce invariants
- remove unused `read_error` constructor: `Invalid_size`
- check that `int31` is actually within `int31` bounds
- add "slicing" feature to analyse binary encoded values

v0.5:  
- add compact encoding combinators
- add fixed-length list and fixed-length array combinators
- fix bug wherein `mu (… option …)` caused an infinite recursion
- fix roundtrip property of BSON
- fix error management of lazy encodings (consistent with non-lazy encodings)
- add a default argument to `Json.construct`: `?include_default_fields`,
  which is simply passed on to `Json_encoding.construct`
- fix missing exported type

v0.5.1:  
- fix bug wherein infinite recursion protection prevented some legitimate uses

v0.5.2:  
- make compact encoding's JSON encoding less opinionated

v0.5.3:  
- fix bug wherein the binary description of zeroable encodings was invalid RST
- make compact encodings JSON output identical to that of vanilla encodings

v0.6:  
- OCaml 4.14 support (including CI tests)
- new tutorial
- fix pathological performance issues when generating some binary descriptions for `mu`+`union` encodings

v0.6.1:  
- improve error management when converting lazy-bytes into JSON

v0.7:  
- add module with safer encoding combinators
- extend Registration API to allow introspection
- improve binary schema generation
- allow dynamic-width ints for ints (with proper overflow checks)
- allow dynamic-width ints (à la `n`) as size headers
- allow to use length-of-collection as header information instead of size
- export type equalities between `Encoding.t` and `encoding` and so on
- improve documentation
- add new string/bytes combinator with parameters to control the JSON representation
- fix bug in check-size combinator

v0.7.1:  
- fixed binary-schema reference duplication issue

v1.0.0:  
- merge in the json-data-encoding repository
- allow bigstring encodings
- allow to chose endianness of integers
- allow to ignore extra object fields when destructing JSON
- improve performance of JSON construction/destruction
- improve self documentation of compact encodings
- improve (further) error management when converting lazy-bytes into JSON

v1.0.1:  
- fixed a bug where negative lengths were accepted by the binary reader
