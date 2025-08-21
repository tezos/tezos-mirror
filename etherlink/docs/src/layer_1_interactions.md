# Interacting with Etherlink from Tezos

The type of an Etherlink rollup from the perspective of the Layer 1 exposes
three entrypoints:

```
(or
  (or
    # deposit of native tokens wrapped in FA2.1 tickets
    (pair bytes (ticket (pair nat (option bytes))))
    # injection of Ethereum transactions
    bytes)
  # upgrade payloads
  bytes)
```

## Depositing on Etherlink

## Posting delayed transactions with a delayed bridge

## Upgrading Etherlink

## Upgrading Etherlink sequencer
