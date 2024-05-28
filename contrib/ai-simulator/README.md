Adaptive Issuance Simulator
===========================

This simulator aims to provide an approximation of the behavior of the new
adaptive issuance and an estimation of average baking rewards according to a
given delegate configuration.

# Simulation Business Code Architecture

The `Simulator` class is responsible for computing the issuance rate evolution
cycle by cycle for a given evolution of the total stake and total supply at each
cycle end.

Given a `Simulator` object and a delegate configuration, the `Delegate` class
provides a simulation of the expected per-cycle rewards of the delegate.

Detailed API of the classes can be found in the generated documentation.

# Demo Visualization
The simulator comes with a visualization page meant to demonstrate its usage (see `site/index.html`). It
loads the Simulator with the stake and total supply data extracted from an
iteration of weekly-net.


# Build

```
npm install
make css
make build
make server
```

Open `http://127.0.0.1:8080/` in your favorite web-browser.

# Build documentation

```
jsdoc src -d ./docs
```
