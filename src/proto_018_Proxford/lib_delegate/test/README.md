# Testing Tenderbake via mockup-based simulations

This test suite contains tests that check the baker. A notable feature that
distinguishes these tests from simple unit tests is that the baker is
examined as a whole with all its components working together. We do not run
a node, instead, we run a mockup node that allows us to create an illusion
for the baker that it talks to a real node. Thus, we have full control of
how the mockup node behaves, how the proposals and operations propagate, and
what the baker sees when it calls RPCs.

Pros:

* Integrates naturally with the existing testing setup and CI. No external
  binaries or setup needed.
* Fast. The round time is currently constant and equal to 3 seconds. 2
  second was also tried, but that resulted in deviations from expected
  behavior in about 10% of cases. Upon closer inspection it was found out
  that round timeouts happen before a key event in the scenario. Supposedly,
  this depends on the time the test is started as all other parameters are
  set and deterministic. Switching 3 seconds solved the issue.
* Uses the same code as the baker, so people who are familiar with the
  existing Tezos will benefit from their knowledge.
* Various assertions and checks can be expressed to ensure that the scenario
  in question progresses exactly as it supposed to.
* Many details of how the baker sees the world can be tightly controlled.

Cons:

* Hard to see the logic of the scenario because it has to be written as a
  collection of hooks.

## Running the tests

The tests can be run like this from the `src/proto_alpha/lib_delegate/test`:

```
$ dune exec ./main.exe -- -v
```

## Writing a test

See the examples in `test_scenario.ml` for inspiration. Start writing a
scenario by deciding how many bakers you need and how many delegates each of
them will have (see the docs for `Mockup_simulator.run`):

```ocaml
  let open Mockup_simulator in
  run [(3, (module Default_hooks)); (2, (module Default_hooks))]
```

* Set `debug` to `true` in `Mockup_simulator.default_config` and pass it to
  `Mockup_simulator.run`. When `debug` is enabled baker logs will be printed.
  This is the main instrument for observing what happens in the scenario.
* Consider setting `timeout` to an appropriate value. By default it is 10
  seconds, which should be fine for short scenarios, but may be insufficient
  for longer ones. Timeout is a safety mechanism that prevents scenarios
  from hanging and non-termination.
* It is also possible to control round durations, but it recommended to
  use at least 3 seconds (the default).
* Finally, proposal slots can be controlled with the `delegate_selection`
  field. The nested lists specify slot owners per level and
  round. Note that if not provided, a seed nonce will be bruteforced
  to obtain the desired delegate selection.


```ocaml
  let open Mockup_simulator in
  let config =
    {
      default_config with
      debug = true;
      delegate_selection =
        [
          ( 1l,
            [
              (0l, bootstrap3);
              (1l, bootstrap4);
              (2l, bootstrap2);
              (3l, bootstrap1);
            ] );
          ( 2l,
            [
              (0l, bootstrap1);
              (1l, bootstrap2);
              (2l, bootstrap3);
              (3l, bootstrap4);
            ] );
        ];
      timeout = 15;
    }
  in
  run ~config [(3, (module Default_hooks)); (2, (module Default_hooks))]
```

Note that delegate selection affects both (pre-)attesting and voting power.
Delegates that do not have proposer slots will not be able to (pre-)attest.
Voting power of delegates who have proposer slots will be proportional to
the number of slots they have.

Next step is writing hook modules per baker that control its mockup mode and
execute assertions. In most cases there is no need to implement all hooks,
so the `Default_hooks` module can be reused, e.g.:

```ocaml
  let module Hooks : Mockup_simulator.Hooks = struct
    include Mockup_simulator.Default_hooks

    let stop_on_event = function
      | Baking_state.New_proposal {block; _} ->
          (* Stop the node as soon as we receive a proposal with a level
             higher than 5. *)
          block.shell.level > 5l
      | _ -> false
  end in
```

Other hooks can be used to implement assertions using `failwith` and to set
mutable variable to track progress of a scenario.

### Termination

A scenario runs till all bakers terminate or till the scenario times out. A
baker can terminate successfully or unsuccessfully. Successful termination
happens when `stop_on_event` returns `true`. Unsuccessful termination occurs
when any of the hooks executes `failwith`. If at least one baker fails its
error message propagates and is displayed by the testing framework
(Alcotest).
