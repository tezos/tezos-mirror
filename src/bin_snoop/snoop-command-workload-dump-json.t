Run benchmark to dump json data later
  $ ./main_snoop.exe benchmark interpreter/N_IBlake2b and save to data.workload --bench-num 2 --nsamples 3 > /dev/null 2> /dev/null

Dump workload json to file
  $ ./main_snoop.exe workload dump data.workload -o data.json 2>&1 |grep -v "already registered for code"
  Measure.load: loaded data.workload
  Measure.packed_measurement_save_json: saved to data.json

Echo data.json file.
  $ cat data.json | jq  '(.measurement_data.date |= "DATE") | (.measurement_data.workload_data |= map((.measures |= map("TIME")) | (.workload[0][1][0][1] |= "SIZE")))'
  {
    "benchmark_namespace": [
      "interpreter",
      "N_IBlake2b"
    ],
    "measurement_data": {
      "benchmark_options": {
        "seed": null,
        "samples_per_bench": "3",
        "bench_number": "2",
        "minor_heap_size": "262144",
        "config_file": null
      },
      "workload_data": [
        {
          "workload": [
            [
              "N_IBlake2b",
              [
                [
                  "bytes",
                  "SIZE"
                ]
              ]
            ],
            [
              "N_IHalt",
              []
            ]
          ],
          "measures": [
            "TIME",
            "TIME",
            "TIME"
          ]
        },
        {
          "workload": [
            [
              "N_IBlake2b",
              [
                [
                  "bytes",
                  "SIZE"
                ]
              ]
            ],
            [
              "N_IHalt",
              []
            ]
          ],
          "measures": [
            "TIME",
            "TIME",
            "TIME"
          ]
        }
      ],
      "date": "DATE"
    }
  }
