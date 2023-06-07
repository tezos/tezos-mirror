# simdal: simulating message propagation on random networks

# Generating data

- Simulation parameters are currently hardcoded, partially in the launch script `launch.sh` and partially in the scenario encoded in `bin/main.ml`

- `launch.sh` requires GNU parallel to be installed (the `parallel` command); the more CPUs are available the faster the results will be obtained. The following command will launch the simulation with all analyses activated:
  `./script.sh --analyse bandwidth --analyse shards --analyse consumer --analyse confirmation`

- Data will be stored in a directory named `results_$(date)`

- After all computations are performed, one can use the `bin/concat.exe` program to assemble the .dat file produced by each simulation process for each analysis, e.g. for slot confirmation:

```
dune exec ./bin/concat.exe -- slot_confirmation.dat results_.../slot_confirmation_*dat
```

# Repo structure

- `lib/` contains the network sampler and (eager) message propagation model
- `bin/` contains the main scenario for the DAL and the various analyses:
  - attester failure probability
  - consumer failure probability
  - bandwidth requirements

The entrypoint of the simulator is `bin/sim.ml`

# TODOs

- [ ] sim: cleaner way to parameterize the simulation (e.g. json-based)
- [ ] sim: strategy for --min-relay is perhaps too aggressive for small attestors
- [ ] sim: lazy/mixed message propagation
- [ ] sim: propagation time analysis
- [ ] stake: more realistic committee computation
