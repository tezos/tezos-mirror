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
const external = require("./lib/external")
const path = require('node:path')
const { timestamp } = require("./lib/timestamp")
const csv = require('csv-stringify/sync');

const RUN_DEBUGGER_COMMAND = external.bin('./octez-smart-rollup-wasm-debugger');
const EVM_INSTALLER_KERNEL_PATH = external.resource('evm_unstripped_installer.wasm');
const PREIMAGE_DIR = external.ressource_dir('_evm_unstripped_installer_preimages');
const OUTPUT_DIRECTORY = external.output()


function sumArray(arr) {
    return arr.reduce((acc, curr) => acc + curr, 0);
}

function push_match(output, array, regexp) {
    var match;
    while ((match = regexp.exec(output))) {
        array.push(match[1]);
    }
}

function run_profiler(path) {

    profiler_result = new Promise((resolve, _) => {

        var gas_used = [];

        var tx_status = [];
        var estimated_ticks = [];
        var estimated_ticks_per_tx = [];
        var tx_size = [];
        var bip_store = [];
        var bip_read = [];
        var receipt_size = [];

        var profiler_output_path = "";

        const args = ["--kernel", EVM_INSTALLER_KERNEL_PATH, "--inputs", path, "--preimage-dir", PREIMAGE_DIR];

        const childProcess = spawn(RUN_DEBUGGER_COMMAND, args, {});

        childProcess.stdin.write("load inputs\n");

        childProcess.stdin.write("step kernel_run\n");

        childProcess.stdin.write("profile\n");

        childProcess.stdin.end();

        childProcess.stdout.on('data', (data) => {
            const output = data.toString();
            const profiler_output_path_regex = /Profiling result can be found in (.+)/;
            const profiler_output_path_match = output.match(profiler_output_path_regex);
            const profiler_output_path_result = profiler_output_path_match
                ? profiler_output_path_match[1]
                : null;
            if (profiler_output_path_result !== null) {
                profiler_output_path = profiler_output_path_result;
                console.log(`Flamechart: ${profiler_output_path}`)
            }
            push_match(output, gas_used, /\bgas_used:\s*(\d+)/g)
            push_match(output, tx_status, /Transaction status: (OK_[a-zA-Z09]+|ERROR_[A-Z_]+)\b/g)
            push_match(output, estimated_ticks, /\bEstimated ticks:\s*(\d+)/g)
            push_match(output, estimated_ticks_per_tx, /\bEstimated ticks after tx:\s*(\d+)/g)
            push_match(output, tx_size, /\bStoring transaction object of size\s*(\d+)/g)
            push_match(output, bip_store, /\bStoring Block in Progress of size\s*(\d+)/g)
            push_match(output, bip_read, /\bReading Block in Progress of size\s*(\d+)/g)
            push_match(output, receipt_size, /\bStoring receipt of size \s*(\d+)/g)

        });
        childProcess.on('close', _ => {
            if (profiler_output_path == "") {
                console.log(new Error("Profiler output path not found"));
            }
            if (gas_used == []) {
                console.log(new Error("Gas usage data not found"));
            }
            if (tx_status.length == 0) {
                console.log(new Error("Status data not found"));
            }
            if (tx_status.length != estimated_ticks_per_tx.length) {
                console.log(new Error("Tx status array length (" + tx_status.length + ") != estimated ticks per tx array length (" + estimated_ticks_per_tx.length + ")"));
            }
            if (tx_status.length != tx_size.length) {
                console.log(new Error("Missing transaction size data (expected: " + tx_status.length + ", actual: " + tx_size.length + ")"));
            }
            if (tx_status.length != receipt_size.length) {
                console.log(new Error("Missing receipt size value (expected: " + tx_status.length + ", actual: " + receipt_size.length + ")"));
            }
            resolve({
                profiler_output_path,
                gas_costs: gas_used,
                tx_status,
                estimated_ticks,
                estimated_ticks_per_tx,
                tx_size,
                bip_store,
                bip_read,
                receipt_size
            });
        });
    })
    return profiler_result;
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
    signature_verification_ticks = await get_ticks(path, "25EthereumTransactionCommon6caller");
    sputnik_runtime_ticks = await get_ticks(path, "11evm_runtime7Runtime3run");
    store_transaction_object_ticks = await get_ticks(path, "storage24store_transaction_object");
    store_receipt_ticks = await get_ticks(path, "store_transaction_receipt");
    interpreter_init_ticks = await get_ticks(path, "interpreter(init)");
    interpreter_decode_ticks = await get_ticks(path, "interpreter(decode)");
    fetch_blueprint_ticks = await get_ticks(path, "blueprint5fetch");
    block_finalize = await get_ticks(path, "store_current_block");
    return {
        kernel_run_ticks: kernel_run_ticks,
        run_transaction_ticks: run_transaction_ticks,
        signature_verification_ticks: signature_verification_ticks,
        store_transaction_object_ticks: store_transaction_object_ticks,
        interpreter_init_ticks: interpreter_init_ticks,
        interpreter_decode_ticks: interpreter_decode_ticks,
        fetch_blueprint_ticks: fetch_blueprint_ticks,
        sputnik_runtime_ticks: sputnik_runtime_ticks,
        store_receipt_ticks,
        block_finalize
    };
}

// Run given benchmark
async function run_benchmark(path) {
    var inbox_size = fs.statSync(path).size
    run_profiler_result = await run_profiler(path);
    profiler_output_analysis_result = await analyze_profiler_output(run_profiler_result.profiler_output_path);
    return {
        inbox_size,
        ...profiler_output_analysis_result,
        ...run_profiler_result
    }
}

function build_benchmark_scenario(benchmark_script) {
    try {
        let bench_path = path.format({ dir: __dirname, base: benchmark_script })
        execSync(`node ${bench_path} > transactions.json`);
    } catch (error) {
        console.log(`Error running script ${benchmark_script}. Please fixed the error in the script before running this benchmark script`)
        console.error(error);
    }
}

function log_benchmark_result(benchmark_name, run_benchmark_result) {
    rows = [];
    gas_costs = run_benchmark_result.gas_costs;
    kernel_run_ticks = run_benchmark_result.kernel_run_ticks;
    run_transaction_ticks = run_benchmark_result.run_transaction_ticks;
    sputnik_runtime_ticks = run_benchmark_result.sputnik_runtime_ticks
    signature_verification_ticks = run_benchmark_result.signature_verification_ticks;
    store_transaction_object_ticks = run_benchmark_result.store_transaction_object_ticks;
    interpreter_init_ticks = run_benchmark_result.interpreter_init_ticks;
    interpreter_decode_ticks = run_benchmark_result.interpreter_decode_ticks;
    fetch_blueprint_ticks = run_benchmark_result.fetch_blueprint_ticks;
    tx_status = run_benchmark_result.tx_status;
    estimated_ticks = run_benchmark_result.estimated_ticks;
    estimated_ticks_per_tx = run_benchmark_result.estimated_ticks_per_tx;
    tx_size = run_benchmark_result.tx_size;
    bip_read = run_benchmark_result.bip_read;
    bip_store = run_benchmark_result.bip_store;

    console.log(`Number of transactions: ${tx_status.length}`)
    run_time_index = 0;
    gas_cost_index = 0;
    for (var j = 0; j < tx_status.length; j++) {
        let basic_info_row = {
            benchmark_name,
            signature_verification_ticks: signature_verification_ticks[j],
            status: tx_status[j],
            estimated_ticks: estimated_ticks_per_tx[j],
        }
        if (tx_status[j].includes("OK_UNKNOWN")) {
            // no outcome should mean never invoking sputnik
            rows.push(
                {
                    gas_cost: 21000,
                    run_transaction_ticks: run_transaction_ticks[j],
                    sputnik_runtime_ticks: 0,
                    store_transaction_object_ticks: store_transaction_object_ticks[j],
                    ...basic_info_row
                });

        }
        else if (tx_status[j].includes("OK")) {
            // sputnik runtime called only if not a transfer
            sputnik_runtime_tick = (gas_costs[gas_cost_index] > 21000) ? sputnik_runtime_ticks[run_time_index++] : 0

            rows.push(
                {
                    gas_cost: gas_costs[gas_cost_index],
                    run_transaction_ticks: run_transaction_ticks[j],
                    sputnik_runtime_ticks: sputnik_runtime_tick,
                    store_transaction_object_ticks: store_transaction_object_ticks[j],
                    store_receipt_ticks: run_benchmark_result.store_receipt_ticks[j],
                    receipt_size: run_benchmark_result.receipt_size[j],
                    tx_size: tx_size[j],
                    ...basic_info_row
                });
            gas_cost_index += 1;
        } else {
            // we can expect no gas cost, no storage of the tx object, and no run transaction, but there will be signature verification
            // invalide transaction detected: ERROR_NONCE, ERROR_PRE_PAY and ERROR_SIGNATURE, in all cases `caller` is called.
            rows.push(basic_info_row);

        }
    }

    if (run_time_index !== sputnik_runtime_ticks.length) {
        console.log("Warning: runtime not matched with a transaction in: " + benchmark_name);
    }

    // first kernel run
    // the nb of tx correspond to the full inbox, not just those done in first run
    rows.push({
        benchmark_name: benchmark_name + "(all)",
        interpreter_init_ticks: interpreter_init_ticks[0],
        interpreter_decode_ticks: interpreter_decode_ticks[0],
        fetch_blueprint_ticks: fetch_blueprint_ticks[0],
        kernel_run_ticks: kernel_run_ticks[0],
        estimated_ticks: estimated_ticks[0],
        inbox_size: run_benchmark_result.inbox_size,
        nb_tx: tx_status.length,
        bip_store: bip_store[0] ? bip_store[0] : 0
    });

    //reboots
    for (var j = 1; j < kernel_run_ticks.length; j++) {
        rows.push({
            benchmark_name: benchmark_name + "(all)",
            interpreter_init_ticks: interpreter_init_ticks[j],
            interpreter_decode_ticks: interpreter_decode_ticks[j],
            fetch_blueprint_ticks: fetch_blueprint_ticks[j],
            kernel_run_ticks: kernel_run_ticks[j],
            estimated_ticks: estimated_ticks[j],
            bip_store: bip_store[j] ? bip_store[j] : 0,
            bip_read: bip_read[j - 1] // the first read correspond to second run
        });
    }

    // ticks that are not covered by identified area of interest
    finalize_ticks = sumArray(run_benchmark_result.block_finalize)
    unaccounted_ticks =
        sumArray(kernel_run_ticks)
        - sumArray(fetch_blueprint_ticks)
        - sumArray(run_transaction_ticks)
        - sumArray(signature_verification_ticks)
        - sumArray(store_transaction_object_ticks)
        - sumArray(run_benchmark_result.store_receipt_ticks)
        - finalize_ticks

    // row concerning all runs
    rows.push({
        benchmark_name: benchmark_name + "(all)",
        unaccounted_ticks,
        block_finalize: finalize_ticks
    });
    return rows;
}


function output_filename() {
    return path.format({ dir: OUTPUT_DIRECTORY, base: `benchmark_result_${timestamp()}.csv` })
}

// Run the benchmark suite and write the result to benchmark_result_${TIMESTAMP}.csv
async function run_all_benchmarks(benchmark_scripts) {
    console.log(`Running benchmarks on: [${benchmark_scripts.join('\n  ')}]`);
    var fields = [
        "benchmark_name",
        "status",
        "gas_cost",
        "signature_verification_ticks",
        "sputnik_runtime_ticks",
        "run_transaction_ticks",
        "tx_size",
        "store_transaction_object_ticks",
        "receipt_size",
        "store_receipt_ticks",
        "estimated_ticks",
        "interpreter_decode_ticks",
        "interpreter_init_ticks",
        "nb_tx",
        "inbox_size",
        "fetch_blueprint_ticks",
        "bip_read",
        "bip_store",
        "block_finalize",
        "kernel_run_ticks",
        "unaccounted_ticks",
    ];
    let output = output_filename();
    console.log(`Output in ${output}`);
    const csv_config = { columns: fields };
    fs.writeFileSync(output, csv.stringify([], { header: true, ...csv_config }));
    for (var i = 0; i < benchmark_scripts.length; i++) {
        var benchmark_script = benchmark_scripts[i];
        var parts = benchmark_script.split("/");
        var benchmark_name = parts[parts.length - 1].split(".")[0];
        console.log(`Benchmarking ${benchmark_script}`);
        build_benchmark_scenario(benchmark_script);
        run_benchmark_result = await run_benchmark("transactions.json");
        benchmark_log = log_benchmark_result(benchmark_name, run_benchmark_result);
        fs.appendFileSync(output, csv.stringify(benchmark_log, csv_config));
    }
    console.log("Benchmarking complete");
    execSync("rm transactions.json");
}

benchmark_scripts = [
    "benchmarks/bench_storage_1.js",
    "benchmarks/bench_storage_2.js",
    "benchmarks/bench_transfers_1.js",
    "benchmarks/bench_transfers_2.js",
    "benchmarks/bench_transfers_3.js",
    "benchmarks/bench_keccak.js",
    "benchmarks/bench_verifySignature.js",
    "benchmarks/bench_erc20tok.js",
    "benchmarks/bench_read_info.js",

    "benchmarks/scenarios/solidity_by_example/bench_abi_decode.js",
    "benchmarks/scenarios/solidity_by_example/bench_abi_encode.js",
    "benchmarks/scenarios/solidity_by_example/bench_array.js",
    "benchmarks/scenarios/solidity_by_example/bench_assembly_error.js",
    "benchmarks/scenarios/solidity_by_example/bench_assembly_loop.js",
    "benchmarks/scenarios/solidity_by_example/bench_assembly_variable.js",
    "benchmarks/scenarios/solidity_by_example/bench_bitwise_op.js",
    "benchmarks/scenarios/solidity_by_example/bench_counter.js",
    "benchmarks/scenarios/solidity_by_example/bench_create_contract.js",
    "benchmarks/scenarios/solidity_by_example/bench_delegatecall.js",
    "benchmarks/scenarios/solidity_by_example/bench_enum.js",
    "benchmarks/scenarios/solidity_by_example/bench_event.js",
    "benchmarks/scenarios/solidity_by_example/bench_function_modifier.js",
    "benchmarks/scenarios/solidity_by_example/bench_function_selector.js",
    "benchmarks/scenarios/solidity_by_example/bench_immutable.js",
    "benchmarks/scenarios/solidity_by_example/bench_mapping.js",
    "benchmarks/scenarios/solidity_by_example/bench_payable.js",
    "benchmarks/scenarios/solidity_by_example/bench_send_ether.js",
    "benchmarks/scenarios/solidity_by_example/bench_struct.js",
    "benchmarks/scenarios/solidity_by_example/bench_ether_wallet.js",
    "benchmarks/scenarios/solidity_by_example/bench_multi_sig_wallet.js",
    "benchmarks/scenarios/solidity_by_example/bench_merkle_tree.js",
    "benchmarks/scenarios/solidity_by_example/bench_iterable_map.js",
    "benchmarks/scenarios/solidity_by_example/bench_erc721.js",
    "benchmarks/scenarios/solidity_by_example/bench_bytecode_contract.js",
    "benchmarks/scenarios/solidity_by_example/bench_create2.js",
    "benchmarks/scenarios/solidity_by_example/bench_minimal_proxy.js",
    "benchmarks/scenarios/solidity_by_example/bench_upgradeable_proxy.js",
    "benchmarks/scenarios/solidity_by_example/bench_binary_exponentiation.js",
    "benchmarks/bench_erc1155.js",
    "benchmarks/bench_selfdestruct.js",

    "benchmarks/bench_creates_erc20.js",
    "benchmarks/bench_creates_erc1155.js",

    "benchmarks/bench_linear_transfers.js 0",
    "benchmarks/bench_linear_transfers.js 5",
    "benchmarks/bench_linear_transfers.js 10",
    "benchmarks/bench_linear_transfers.js 15",
    "benchmarks/bench_linear_transfers.js 20",
    "benchmarks/bench_linear_transfers.js 25",
    "benchmarks/bench_linear_transfers.js 30",

    "benchmarks/bench_linear_erc20.js 0",
    "benchmarks/bench_linear_erc20.js 5",
    "benchmarks/bench_linear_erc20.js 10",
    "benchmarks/bench_linear_erc20.js 15",
    "benchmarks/bench_linear_erc20.js 20",
    "benchmarks/bench_linear_erc20.js 25",
    "benchmarks/bench_linear_erc20.js 30",

    "benchmarks/bench_loop_progressive.js",
    "benchmarks/bench_loop_expensive.js",

    "benchmarks/bench_loop_calldataload.js"
]

run_all_benchmarks(benchmark_scripts);
