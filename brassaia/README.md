# Brassaia Libraries

## Overview

Brassaia (which name comes from the 'Brassaia Actinophyllia' tree, also called
the "octopus tree") is a library based on Irmin (see
[irmin/README.md](../irmin/README.md)) that is used in the Octez suite.

Brassaia is a simplification and an optimization of Irmin for the specific needs
of the Octez suite. It is not intended to be a general-purpose library. It is
currently a work in progress and should not be used in production. Octez will
continue to use Irmin until Brassaia is ready. To use Brassaia, one can set the
environment variable `TEZOS_CONTEXT_BACKEND` to `Brassaia`.

## License

Irmin is licensed under the ISC license. Modifications to the Irmin library
provided in Brassaia are under the MIT license. Per a contractual agreement with
Nomadic Labs, sublicensing under the MIT License is implicitly authorized. It is
noted that the ISC and MIT licenses are legally compatible, allowing for such
sublicensing arrangements without conflict of rights. Brassaia is thus under the
double license ISC AND MIT. Brassaia also includes a fork of the [Index](https://github.com/mirage/index) library
which is under the MIT license.

License files can be found in the `LICENSE` directory at the root of the repository.