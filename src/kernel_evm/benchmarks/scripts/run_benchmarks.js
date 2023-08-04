// SPDX-FileCopyrightText: 2023 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

// This script runs the benchmarks for the EVM kernel and writes the result to benchmark_result.csv

// Before running this script, run the following commands to build the debugger and the benchmark kernel
// $ make 
// $ make -C src/kernel_evm/

// Then run this script using the following command
// $ node src/kernel_evm/kernel_benchmark/scripts/run_benchmarks.js

// Each row of the output file represents the processing of one message in the kernel
// Each value represents a cost. "gas_cost" is the cost in gas in the EVM, and the other values are costs in ticks in the PVM

var fs = require('fs');
var readline = require('readline');
const { spawn } = require('child_process');
const { execSync } = require('child_process');

const RUN_DEBUGGER_COMMAND = './octez-smart-rollup-wasm-debugger';
const BENCHMARK_KERNEL_PATH = 'src/kernel_evm/target/wasm32-unknown-unknown/release/kernel_benchmark.wasm'

// Get the gas cost of the transactions from the outbox by running the debugger
async function get_gas_cost(path) {
    transaction_gas_costs = new Promise((resolve, _) => {

        var gas_used = [];

        const args = [BENCHMARK_KERNEL_PATH, "--inputs", path];

        const childProcess = spawn(RUN_DEBUGGER_COMMAND, args, {});

        childProcess.stdin.write("load inputs\n");

        childProcess.stdin.write("step inbox\n");

        childProcess.stdin.end();

        childProcess.stdout.on('data', (data) => {
            const output = data.toString();
            if (output.includes("gas_used")) {
                const regex = /gas_used:\s(\d+)/;
                const match = output.match(regex);
                const gas = match ? match[1] : "Nan";
                gas_used.push(gas);
            }
        });

        childProcess.on('close', _ => {
            resolve(gas_used);
        });
        }
    )
    return transaction_gas_costs;
}

// Run the profiler in the debugger and returns the path of the profiler output file
function run_profiler(path) {

    profiler_output_path = new Promise ((resolve, _) => {
        const args = [BENCHMARK_KERNEL_PATH, "--inputs", path];

        const childProcess = spawn(RUN_DEBUGGER_COMMAND, args, {});

        childProcess.stdin.write("load inputs\n");

        childProcess.stdin.write("profile\n");

        childProcess.stdin.end();

        childProcess.stdout.on('data', (data) => {
            const output = data.toString();
            const regex = /Profiling result can be found in (.+)/;
            const match = output.match(regex);
            const result = match ? match[1] : null;
            if (result !== null) {
                resolve(result);
            }
        });
    })
    return profiler_output_path;
}

// Helper function to count the number of ticks of given function call
async function get_ticks(path, function_call_keyword) {
    const fileStream = fs.createReadStream(path);
    var ticks_count_for_transactions = [];
    var previous_row_is_given_function_call = false;

    const rl = readline.createInterface({
        input: fileStream,
        crlfDelay: Infinity
      });

      for await (const l of rl) {
        if (l !== "") {
            tokens = l.split(" ");
            calls = tokens[0];
            ticks = tokens[1];
            if (calls.includes(function_call_keyword)) {
                if (previous_row_is_given_function_call) {
                    ticks_count_for_transactions[ticks_count_for_transactions.length - 1] += parseInt(ticks);
                } else {
                    ticks_count_for_transactions.push(parseInt(ticks));
                    previous_row_is_given_function_call = true
                }
            } else {
                previous_row_is_given_function_call = false;
            }
        }
      }

    return ticks_count_for_transactions;
}

// Parse the profiler output file and get the tick counts of the differerent function calls
async function analyze_profiler_output(path) {

    kernel_run_ticks = await get_ticks(path, "kernel_run");
    run_transaction_ticks = await get_ticks(path, "run_transaction");
    signature_verification_ticks = await get_ticks(path, "tezos_ethereum10signatures");
    interpreter_init_ticks = await get_ticks(path, "interpreter(init)");
    interpreter_decode_ticks = await get_ticks(path, "interpreter(decode)");
    return [kernel_run_ticks, run_transaction_ticks, signature_verification_ticks, interpreter_init_ticks, interpreter_decode_ticks];
}

// Run given benchmark 
async function run_benchmark(path) {
    gas_costs = await get_gas_cost(path);
    profiler_output_path = await run_profiler(path);
    profiler_output_analysis_result = await analyze_profiler_output(profiler_output_path);
    kernel_run_ticks = profiler_output_analysis_result[0];
    run_transaction_ticks = profiler_output_analysis_result[1];
    signature_verification_ticks = profiler_output_analysis_result[2];
    interpreter_init_ticks = profiler_output_analysis_result[3];
    interpreter_decode_ticks = profiler_output_analysis_result[4];
    return [gas_costs, kernel_run_ticks, run_transaction_ticks, signature_verification_ticks, interpreter_init_ticks, interpreter_decode_ticks];
}

// Run the benchmark suite and write the result to benchmark_result.csv
async function run_all_benchmarks(benchmark_scripts) {
    console.log(`Running benchmarks on: [${benchmark_scripts.join('\n  ')}]`);
    var rows = [["benchmark_name", "gas_cost", "kernel_run_ticks", "run_transaction_ticks", "signature_verification_ticks", "interpreter_init_ticks", "interpreter_decode_ticks"]]
    for (var i = 0; i < benchmark_scripts.length; i++) {
        var benchmark_script = benchmark_scripts[i];
        var parts = benchmark_script.split("/");
        var benchmark_name = parts[parts.length - 1].split(".")[0];
        console.log(`Benchmarking ${benchmark_script}`);
        try {
            execSync(`node ${benchmark_script} > transactions.json`);
        } catch (error) {
            console.log(`Error running script ${benchmark_script}. Please fixed the error in the script before running this benchmark script`)
            console.error(error);
        }
        run_benchmark_result = await run_benchmark("transactions.json");
        gas_costs = run_benchmark_result[0];
        kernel_run_ticks = run_benchmark_result[1];
        run_transaction_ticks=run_benchmark_result[2];
        run_signature_verification_ticks=run_benchmark_result[3];
        interpreter_init_ticks = run_benchmark_result[4];
        interpreter_decode_ticks = run_benchmark_result[5];

        // SOL
        rows.push([benchmark_name, "", kernel_run_ticks[0], "", "", interpreter_init_ticks[0], interpreter_decode_ticks[0]]);

        // Metadata
        rows.push([benchmark_name, "", kernel_run_ticks[1], "", "", interpreter_init_ticks[1], interpreter_decode_ticks[1]]);

        for (var j = 0; j < gas_costs.length; j++) {
            rows.push([benchmark_name, gas_costs[j], kernel_run_ticks[j+2], run_transaction_ticks[j], run_signature_verification_ticks[j], interpreter_init_ticks[j+2], interpreter_decode_ticks[j+2]]);
        }

        // EOL
        rows.push([benchmark_name, "", kernel_run_ticks[kernel_run_ticks.length - 2], "", "",interpreter_init_ticks[interpreter_init_ticks.length - 2], interpreter_decode_ticks[interpreter_decode_ticks.length - 2]]);

        // End loop
        rows.push([benchmark_name, "", kernel_run_ticks[kernel_run_ticks.length - 1], "", "",interpreter_init_ticks[interpreter_init_ticks.length - 1], interpreter_decode_ticks[interpreter_decode_ticks.length - 1]]);
    }

    console.log("Writing result to benchmark_result.csv");

    var content = rows.map(row => row.join(",")).join("\n");

    fs.writeFile('benchmark_result.csv', content, err => {
        if (err) {
          console.error(err);
        }
      });

    execSync("rm transactions.json");
}

benchmark_scripts = [
    "src/kernel_evm/kernel_benchmark/scripts/benchmarks/bench_storage_1.js",
    "src/kernel_evm/kernel_benchmark/scripts/benchmarks/bench_storage_2.js",
    "src/kernel_evm/kernel_benchmark/scripts/benchmarks/bench_transfers_1.js",
    "src/kernel_evm/kernel_benchmark/scripts/benchmarks/bench_transfers_2.js",
    "src/kernel_evm/kernel_benchmark/scripts/benchmarks/bench_transfers_3.js",
    "src/kernel_evm/kernel_benchmark/scripts/benchmarks/bench_keccak.js",
    "src/kernel_evm/kernel_benchmark/scripts/benchmarks/bench_verifySignature.js",
    "src/kernel_evm/kernel_benchmark/scripts/benchmarks/bench_loop.js",
]

run_all_benchmarks(benchmark_scripts);

