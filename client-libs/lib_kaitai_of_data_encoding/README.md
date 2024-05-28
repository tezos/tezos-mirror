# Context

Tezos `data-encoding` is a combinator library for defining octez encodings.
Kaitai struct language is a formal description language for describing existing
binary encoded data. Given a valid kaitai struct description file (`.ksy`),
we can autogenerate parsers in many mainstream languages.

# `kaitai-of-data-encoding` library

To generate (`.ksy`) for a valid octez encoding, we have come up with
`kaitai-of-data-encoding` library, that translates `data-encoding` AST
to a valid Kaitai AST (`kaitai` library).
