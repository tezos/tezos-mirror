Missing config file prints
  $ ./main_snoop.exe benchmark proto/alpha/interpreter/N_IBlake2b and save to output.json -c __nosuchdir --bench-num 1 2>&1 | sed s'/stats over all benchmarks:.*/stats <hidden>/'
  Benchmarking with the following options:
  { options = { seed=self-init;
                bench #=1;
                nsamples/bench=3000;
                minor_heap_size=262144 words;
                config directory=__nosuchdir };
     save_file = output.json;
     storage = Mem }
  Failed loading json __nosuchdir: No such file or directory (Ignoring)
  Using default configuration for benchmark proto/alpha/interpreter/N_IBlake2b
  Loaded configuration from __nosuchdir for benchmark proto/alpha/interpreter/N_IBlake2b
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

Generate empty config
  $ ./main_snoop.exe config generate empty in empty.json; jq . empty.json
  {
    "namespace": ".",
    "config": null,
    "children": []
  }

Generate default config
  $ ./main_snoop.exe config generate default in dft.json for proto/alpha/interpreter/N_IBlake2b; jq . dft.json
  {
    "namespace": ".",
    "config": null,
    "children": [
      {
        "namespace": "proto",
        "config": null,
        "children": [
          {
            "namespace": "alpha",
            "config": null,
            "children": [
              {
                "namespace": "interpreter",
                "config": null,
                "children": [
                  {
                    "namespace": "N_IBlake2b",
                    "config": {
                      "sampler": {
                        "int_size": {
                          "min": 8,
                          "max": 100000
                        },
                        "string_size": {
                          "min": 1024,
                          "max": 131072
                        },
                        "bytes_size": {
                          "min": 1024,
                          "max": 131072
                        },
                        "list_size": {
                          "min": 10,
                          "max": 1000
                        },
                        "set_size": {
                          "min": 10,
                          "max": 1000
                        },
                        "map_size": {
                          "min": 10,
                          "max": 1000
                        }
                      },
                      "sapling": {
                        "sapling_txs_file": "/no/such/file",
                        "seed": null
                      },
                      "comb": {
                        "max_depth": 1000
                      },
                      "compare": {
                        "type_size": {
                          "min": 1,
                          "max": 15
                        }
                      }
                    },
                    "children": [
                      {
                        "namespace": "intercept",
                        "config": {
                          "sampler": {
                            "int_size": {
                              "min": 8,
                              "max": 100000
                            },
                            "string_size": {
                              "min": 1024,
                              "max": 131072
                            },
                            "bytes_size": {
                              "min": 1024,
                              "max": 131072
                            },
                            "list_size": {
                              "min": 10,
                              "max": 1000
                            },
                            "set_size": {
                              "min": 10,
                              "max": 1000
                            },
                            "map_size": {
                              "min": 10,
                              "max": 1000
                            }
                          },
                          "sapling": {
                            "sapling_txs_file": "/no/such/file",
                            "seed": null
                          },
                          "comb": {
                            "max_depth": 1000
                          },
                          "compare": {
                            "type_size": {
                              "min": 1,
                              "max": 15
                            }
                          }
                        },
                        "children": []
                      }
                    ]
                  }
                ]
              }
            ]
          }
        ]
      }
    ]
  }

Modify empty config
  $ ./main_snoop.exe config edit dft.json for proto/alpha/interpreter -j '{"a":123}'; jq . dft.json
  {
    "namespace": ".",
    "config": null,
    "children": [
      {
        "namespace": "proto",
        "config": null,
        "children": [
          {
            "namespace": "alpha",
            "config": null,
            "children": [
              {
                "namespace": "interpreter",
                "config": {
                  "a": 123
                },
                "children": [
                  {
                    "namespace": "N_IBlake2b",
                    "config": {
                      "sampler": {
                        "int_size": {
                          "min": 8,
                          "max": 100000
                        },
                        "string_size": {
                          "min": 1024,
                          "max": 131072
                        },
                        "bytes_size": {
                          "min": 1024,
                          "max": 131072
                        },
                        "list_size": {
                          "min": 10,
                          "max": 1000
                        },
                        "set_size": {
                          "min": 10,
                          "max": 1000
                        },
                        "map_size": {
                          "min": 10,
                          "max": 1000
                        }
                      },
                      "sapling": {
                        "sapling_txs_file": "/no/such/file",
                        "seed": null
                      },
                      "comb": {
                        "max_depth": 1000
                      },
                      "compare": {
                        "type_size": {
                          "min": 1,
                          "max": 15
                        }
                      }
                    },
                    "children": [
                      {
                        "namespace": "intercept",
                        "config": {
                          "sampler": {
                            "int_size": {
                              "min": 8,
                              "max": 100000
                            },
                            "string_size": {
                              "min": 1024,
                              "max": 131072
                            },
                            "bytes_size": {
                              "min": 1024,
                              "max": 131072
                            },
                            "list_size": {
                              "min": 10,
                              "max": 1000
                            },
                            "set_size": {
                              "min": 10,
                              "max": 1000
                            },
                            "map_size": {
                              "min": 10,
                              "max": 1000
                            }
                          },
                          "sapling": {
                            "sapling_txs_file": "/no/such/file",
                            "seed": null
                          },
                          "comb": {
                            "max_depth": 1000
                          },
                          "compare": {
                            "type_size": {
                              "min": 1,
                              "max": 15
                            }
                          }
                        },
                        "children": []
                      }
                    ]
                  }
                ]
              }
            ]
          }
        ]
      }
    ]
  }

Check config edition
  $ ./main_snoop.exe config check dft.json for proto/alpha/interpreter/N_IBlake2b
  Looking for custom configuration for benchmark proto/alpha/interpreter/N_IBlake2b
  Config file successfully parsed
  Loaded configuration from dft.json for benchmark proto/alpha/interpreter/N_IBlake2b
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

Test merge
  $ ./main_snoop.exe config merge dft.json in empty.json; jq . empty.json
  {
    "namespace": ".",
    "config": null,
    "children": [
      {
        "namespace": "proto",
        "config": null,
        "children": [
          {
            "namespace": "alpha",
            "config": null,
            "children": [
              {
                "namespace": "interpreter",
                "config": {
                  "a": 123
                },
                "children": [
                  {
                    "namespace": "N_IBlake2b",
                    "config": {
                      "sampler": {
                        "int_size": {
                          "min": 8,
                          "max": 100000
                        },
                        "string_size": {
                          "min": 1024,
                          "max": 131072
                        },
                        "bytes_size": {
                          "min": 1024,
                          "max": 131072
                        },
                        "list_size": {
                          "min": 10,
                          "max": 1000
                        },
                        "set_size": {
                          "min": 10,
                          "max": 1000
                        },
                        "map_size": {
                          "min": 10,
                          "max": 1000
                        }
                      },
                      "sapling": {
                        "sapling_txs_file": "/no/such/file",
                        "seed": null
                      },
                      "comb": {
                        "max_depth": 1000
                      },
                      "compare": {
                        "type_size": {
                          "min": 1,
                          "max": 15
                        }
                      }
                    },
                    "children": [
                      {
                        "namespace": "intercept",
                        "config": {
                          "sampler": {
                            "int_size": {
                              "min": 8,
                              "max": 100000
                            },
                            "string_size": {
                              "min": 1024,
                              "max": 131072
                            },
                            "bytes_size": {
                              "min": 1024,
                              "max": 131072
                            },
                            "list_size": {
                              "min": 10,
                              "max": 1000
                            },
                            "set_size": {
                              "min": 10,
                              "max": 1000
                            },
                            "map_size": {
                              "min": 10,
                              "max": 1000
                            }
                          },
                          "sapling": {
                            "sapling_txs_file": "/no/such/file",
                            "seed": null
                          },
                          "comb": {
                            "max_depth": 1000
                          },
                          "compare": {
                            "type_size": {
                              "min": 1,
                              "max": 15
                            }
                          }
                        },
                        "children": []
                      }
                    ]
                  }
                ]
              }
            ]
          }
        ]
      }
    ]
  }
