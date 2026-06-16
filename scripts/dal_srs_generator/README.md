# dal_srs_generator

## Purpose of the script

When trying to produce commitments using non-conventional DAL "cryptobox"
parameters, one can meet an error like:
```bash
octez-dal-node: Error:
                  Unable to initialise the cryptobox parameters. Reason: The
                  verifier SRS on G2 should contain points for indices
                  shard_length = 256, page_length_domain = 128 &
                  offset_monomial_degree = 2080768. Hint: you can add new
                  points to the SRS (to do that, use the function
                  Srs.Internal_for_tests.Print.print_verifier_srs_from_file).

```

This script wraps the function mentioned in the error message to simply
generate the required hash to be added in the `zcash_srs.ml` file.

## How to use

1. In `src/lib_crypto_dal/srs.ml`, modify the `print_verifier_srs_from_file`
   function to have the desired parameters in `default_params`.
2. If you do not have them yet, create the `srs_zcash_g1` and `srs_zcash_g2`
   files by running the `./scripts/install_dal_trusted_setup.sh` script.
   (use `--legacy`, otherwise, you will get the `srsu` files)
3. Run this script with:
   ```
   dune exec ./scripts/dal_srs_generator/dal_srs_generator.exe -- \
     "./_opam/share/dal-trusted-setup/srs_zcash_g1" \
     "./_opam/share/dal-trusted-setup/srs_zcash_g2" \
     verifier_srs_generated.ml
   ```
   This will generate a file `verifier_srs_generated.ml` containing the new values of
   `srs_g1` and `srs_g2`.
4. You need to copy the values of `srs_g1` and `srs_g2` of `verified_srs_generated.ml`
   into the `src/lib_crypto_dal/zcash_srs.ml` file.
5. If you rebuild your DAL node, the error should have disappeared.
