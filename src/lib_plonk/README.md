make sure to remove and unpin polynomial, polynomial-sig and plonk, then opam should install all the necessary dependencies.
```
opam install .
```

Otherwise you can manually pin:
```
opam pin add polynomial-sig.dev git+https://gitlab.com/dannywillems/ocaml-polynomial.git#61639af77c2737c117c837bd77d3a54f20627bd6
opam pin add polynomial.dev git+https://gitlab.com/dannywillems/ocaml-polynomial.git#61639af77c2737c117c837bd77d3a54f20627bd6
```
