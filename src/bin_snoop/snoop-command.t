Missing config file prints
  $ ./main_snoop.exe benchmark N_IBlake2b_alpha and save to output.json -c __nosuchdir --bench-num 1 2>&1 | sed s'/stats over all benchmarks:.*/stats <hidden>/'
  Model N_IOpt_map__alpha already registered for code generation! (overloaded instruction?) Ignoring.
  Model N_ILambda__alpha already registered for code generation! (overloaded instruction?) Ignoring.
  Model N_ISapling_verify_update__alpha already registered for code generation! (overloaded instruction?) Ignoring.
  Model N_ISapling_verify_update__alpha already registered for code generation! (overloaded instruction?) Ignoring.
  Model N_ISapling_verify_update__alpha already registered for code generation! (overloaded instruction?) Ignoring.
  Model N_KIter__alpha already registered for code generation! (overloaded instruction?) Ignoring.
  Model N_KList_enter_body__alpha already registered for code generation! (overloaded instruction?) Ignoring.
  Model N_KMap_enter_body__alpha already registered for code generation! (overloaded instruction?) Ignoring.
  Benchmarking with the following options:
  { options = { seed=self-init;
                bench #=1;
                nsamples/bench=3000;
                minor_heap_size=262144 words;
                config directory=__nosuchdir };
     save_file = output.json;
     storage = Mem }
  Failed loading json __nosuchdir/N_IBlake2b_alpha.json: No such file or directory (Ignoring)
  Using default configuration for benchmark N_IBlake2b_alpha
  Loaded configuration from __nosuchdir/N_IBlake2b_alpha.json for benchmark N_IBlake2b_alpha
  { "sampler":
      { "int_size": { "min": 8, "max": 100000 },
        "string_size": { "min": 1024, "max": 131072 },
        "bytes_size": { "min": 1024, "max": 131072 },
        "list_size": { "min": 10, "max": 1000 },
        "set_size": { "min": 10, "max": 1000 },
        "map_size": { "min": 10, "max": 1000 } },
    "sapling": { "sapling_txs_file": "/no/such/file", "seed": null },
    "comb": { "max_depth": 1000 },
    "compare": { "type_size": { "min": 1, "max": 15 } } }
  benchmarking 1/1
  stats <hidden>

benchmarking 1/1
