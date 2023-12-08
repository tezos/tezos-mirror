# Tests on macro

All tests on macro verify solely how macro expand, the exact expansion is
checked via packing a lambda and comparing it against the right bytes.

Note that macro expansion may produce additional levels of nesting. For
instance, `ASSERT` macro expands to

```
{ { IF {} { { UNIT ; FAILWITH } } } }
```

and preserving this in its exact form is important as it affects behaviour of
`PACK` and `UNPACK` instructions.

You can check the exact expansion via the following command

```sh
octez-client expand macros in "$MACRO"
```
