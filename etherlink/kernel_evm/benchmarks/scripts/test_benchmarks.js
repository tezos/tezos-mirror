// SPDX-FileCopyrightText: 2023 TriliTech <contact@trili.tech>
// SPDX-FileCopyrightText: 2023 Marigold <contact@marigold.dev>
//
// SPDX-License-Identifier: MIT

// This script runs the benchmarks for the EVM kernel and writes the result to benchmark_result.csv

// Before running this script, run the following commands to build the debugger and the benchmark kernel
// $ make
// $ make -f kernels.mk evm_benchmark_installer.wasm

// Then run this script using the following command
// $ node src/kernel_evm/kernel_benchmark/scripts/test_benchmarks.js

// There are two output files, one for the transactions and one for the kernel
// runs.

var fs = require('fs');
const { spawn } = require('child_process');
const { execSync } = require('child_process');
const external = require("./lib/external")
const path = require('node:path')
const { timestamp } = require("./lib/timestamp")
const csv = require('csv-stringify/sync');

const RUN_DEBUGGER_COMMAND = external.bin('./octez-smart-rollup-wasm-debugger');
const EVM_INSTALLER_KERNEL_PATH = external.resource('evm_benchmark_installer.wasm');
const PREIMAGE_DIR = external.ressource_dir('_evm_unstripped_installer_preimages');
const OUTPUT_DIRECTORY = external.output()

function push_match(output, array, regexp) {
    var match;
    while ((match = regexp.exec(output))) {
        array.push(match[1]);
    }
}

function run_profiler(path) {

    profiler_result = new Promise((resolve, _) => {

        let tx_status = [];
        let estimated_ticks = [];
        let estimated_ticks_per_tx = [];

        const args = ["--kernel", EVM_INSTALLER_KERNEL_PATH, "--inputs", path, "--preimage-dir", PREIMAGE_DIR];

        const childProcess = spawn(RUN_DEBUGGER_COMMAND, args, {});

        childProcess.stdin.write("step inbox\n");

        childProcess.stdin.end();

        childProcess.stdout.on('data', (data) => {
            const output = data.toString();
            push_match(output, tx_status, /Transaction status: (OK_[a-zA-Z09]+|ERROR_[A-Z_]+)\b/g)
            push_match(output, estimated_ticks, /\bEstimated ticks:\s*(\d+)/g)
            push_match(output, estimated_ticks_per_tx, /\bEstimated ticks after tx:\s*(\d+)/g)

        });
        childProcess.on('close', _ => {
            if (tx_status.length == 0) {
                console.log(new Error("Status data not found"));
            }
            resolve({
                tx_status,
                estimated_ticks,
                estimated_ticks_per_tx,
            });
        });
    })
    return profiler_result;
}


// Run given benchmark
async function run_benchmark(path) {
    var inbox_size = fs.statSync(path).size
    run_profiler_result = await run_profiler(path);
    return {
        inbox_size,
        ...run_profiler_result
    }
}

function build_benchmark_scenario(benchmark_script) {
    try {
        let bench_path = path.format({ dir: __dirname, base: benchmark_script })
        execSync(`node ${bench_path} > transactions.json`);
    } catch (error) {
        console.log(`Error running script ${benchmark_script}. Please fix the error in the script before running this benchmark script`)
        console.error(error);
    }
}

function log_benchmark_result(benchmark_name, run_benchmark_result) {
    let rows_txs = [];
    let rows_runs = [];
    let tx_status = run_benchmark_result.tx_status;
    let estimated_ticks = run_benchmark_result.estimated_ticks;
    let nb_runs = estimated_ticks.length;
    let error = 0
    console.log(`Number of transactions: ${tx_status.length}`)

    for (var j = 0; j < tx_status.length; j++) {
        let basic_info_row = {
            benchmark_name,
            tx_idx: j + 1,
            status: tx_status[j],
            estimated_ticks: run_benchmark_result.estimated_ticks_per_tx[j],
        }
        rows_txs.push(basic_info_row);
        if (!tx_status[j].includes("OK_true")) error++
    }

    console.log(`Number of errors: ${error}`)

    //reboots
    for (var j = 0; j < nb_runs; j++) {
        rows_runs.push({
            benchmark_name: benchmark_name + `(reboot ${j})`,
            estimated_ticks: estimated_ticks[j],
        });
    }

    // row concerning all runs
    rows_runs.push({
        benchmark_name: benchmark_name + "(all)",
        inbox_size: run_benchmark_result.inbox_size,
        nb_tx: tx_status.length,
        nb_runs
    });
    return { rows_txs, rows_runs };
}


function output_filename_txs(time) {
    return path.format({ dir: OUTPUT_DIRECTORY, base: `test_result_tx_${time}.csv` })
}

function output_filename_runs(time) {
    return path.format({ dir: OUTPUT_DIRECTORY, base: `test_result_runs_${time}.csv` })
}

// Run the benchmark suite and write the result to benchmark_result_${TIMESTAMP}.csv
async function run_all_benchmarks(benchmark_scripts) {
    console.log(`Running benchmarks on: [${benchmark_scripts.join('\n  ')}]`);
    let fields_runs = [
        "benchmark_name",
        "inbox_size",
        "nb_tx",
        "nb_runs",
        "estimated_ticks",
    ]
    let fields_txs = [
        "benchmark_name",
        "tx_idx",
        "status",
        "estimated_ticks",
    ]

    let time = timestamp();
    let output_txs = output_filename_txs(time);
    let output_runs = output_filename_runs(time);
    console.log(`Output txs in ${output_txs}`);
    console.log(`Output runs in ${output_runs}`);

    fs.writeFileSync(output_txs, csv.stringify([], { header: true, columns: fields_txs }));
    fs.writeFileSync(output_runs, csv.stringify([], { header: true, columns: fields_runs }));

    for (var i = 0; i < benchmark_scripts.length; i++) {
        var benchmark_script = benchmark_scripts[i];
        var parts = benchmark_script.split("/");
        var benchmark_name = parts[parts.length - 1].split(".")[0];
        console.log(`Testing ${benchmark_script}`);
        build_benchmark_scenario(benchmark_script);
        let run_benchmark_result = await run_benchmark("transactions.json");
        let results = log_benchmark_result(benchmark_name, run_benchmark_result);
        fs.appendFileSync(output_txs, csv.stringify(results.rows_txs, { columns: fields_txs }));
        fs.appendFileSync(output_runs, csv.stringify(results.rows_runs, { columns: fields_runs }));
    }
    console.log("Test complete");
    fs.rmSync(
        "transactions.json", { recursive: true, force: true })
}

const benchmark_scripts = require("./benchmarks_list.json")

run_all_benchmarks(benchmark_scripts);
