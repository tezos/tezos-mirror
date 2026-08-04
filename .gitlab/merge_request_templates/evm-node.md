<!-- EVM node merge request template. -->

# Checklist

- [ ] Provide automatic testing.
- [ ] Declare the
      [changelog](https://gitlab.com/tezos/tezos/-/blob/master/etherlink/CHANGES_NODE.md)
      entry of this MR as a fragment
      `etherlink/.changes/node/<section>/<this MR number>.md` — or an empty
      `etherlink/.changes/node/no_changelog/<this MR number>.md` if no entry is
      needed (see
      [`.changes/README.md`](https://gitlab.com/tezos/tezos/-/blob/master/etherlink/.changes/README.md)).
      Do not edit `CHANGES_NODE.md` itself.
- [ ] Add a page in the
      [doc](https://gitlab.com/tezos/tezos/-/tree/master/etherlink/docs)

# What

<!-- Explain what your MR does without going into details. -->

# Why

<!-- Explain the motivation for your work. -->

# How

<!-- Explain how your MR achieves what it says it does and why it is a good way. -->
<!-- Discuss possible side-effects and other solutions you have considered. -->

# Manually testing the MR

<!-- Explain how to test you MR, preferably manually or by running a test. -->

/assign me

/labels ~evm::node

/milestone %"Etherlink: backlog"
