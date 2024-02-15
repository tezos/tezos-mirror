// SPDX-FileCopyrightText: 2023 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

// This script runs the benchmarks for the EVM kernel and writes the result to benchmark_result.csv

// Before running this script, run the following commands to build the debugger and the benchmark kernel
// $ make
// $ make -C etherlink/kernel_evm/

// Then run this script using the following command
// $ node etherlink/kernel_evm/kernel_benchmark/scripts/run_benchmarks.js

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
const commander = require('commander');
const { mkdirSync } = require('node:fs')

commander
    .usage('[OPTIONS]')
    .option('-i, --include <regex>', 'Only consider benchmark scripts matching <regex>')
    .option('-e, --exclude <regex>', 'Exclude benchmark scripts matching <regex>')
    .option('-o, --output-dir <path>', "Output directory")
    .option('--fast-mode', "Launch kernel in fast mode, but less information is collected", false)
    .option('--keep-temp', "Keep temporary files", false)
    .parse(process.argv);

let INCLUDE_REGEX = commander.opts().include
let EXCLUDE_REGEX = commander.opts().exclude
function filter_name(name) {
    return (INCLUDE_REGEX === undefined
        || name.match(INCLUDE_REGEX))
        && (EXCLUDE_REGEX === undefined
            || !name.match(EXCLUDE_REGEX))
}

let KEEP_TEMP = commander.opts().keepTemp;
let FAST_MODE = commander.opts().fastMode;
const RUN_DEBUGGER_COMMAND = external.bin('./octez-smart-rollup-wasm-debugger');
const EVM_INSTALLER_KERNEL_PATH = external.resource('evm_benchmark_installer.wasm');
const PREIMAGE_DIR = external.ressource_dir('_evm_unstripped_installer_preimages');
const OUTPUT_DIRECTORY = commander.opts().outputDir ? commander.opts().outputDir : external.output()
mkdirSync(OUTPUT_DIRECTORY, { recursive: true })
console.log(`Output directory ${OUTPUT_DIRECTORY}`)

function sumArray(arr) {
    return arr?.reduce((acc, curr) => acc + curr, 0) ?? 0;
}

function push_match(output, array, regexp) {
    var match;
    while ((match = regexp.exec(output))) {
        array.push(match[1]);
    }
}

/// Parses the data from the `Section` output. See
/// `evm_execution/src/handler.rs` for the format.
function parse_data(opcode, gas_and_result) {
    if (gas_and_result.length != 18) { return undefined };

    let gas_data = gas_and_result.substring(0, 16);
    let step_result = parseInt('0x' + gas_and_result.substring(16));
    // parse little endian integers
    let gas = parseInt('0x' + gas_data.match(/../g).reverse().join(''));
    return { opcode, gas, step_result }
}

/// Parses the section and push the sample into the set of opcodes.
function push_profiler_sections(output, opcodes, precompiles) {
    const section_regex = /\__wasm_debugger__::Section{ticks:(\d+);data:\((0x[0-9a-fA-F]*),0x([0-9a-fA-F]*)\)}/g;
    let precompiled_address_set = new Set([1, 2, 3, 4, 9, 32]);

    for (const match of output.matchAll(section_regex)) {
        let is_opcode_data = match[2].length == 4;
        if (is_opcode_data) {
            let { opcode, gas, step_result } = parse_data(match[2], match[3]);
            let ticks = parseInt(match[1]);
            let result = { ticks, gas, step_result };
            if (opcodes[opcode] == undefined) {
                opcodes[opcode] = [result]
            } else {
                opcodes[opcode].push(result)
            }
        } else {
            let ticks = parseInt(match[1]);
            let address = parseInt(match[2].substring(0, 42));
            let data_size = parseInt("0x" + match[2].substring(42));
            if (precompiled_address_set.has(address)) {
                precompiles.push({ "address": address, "data_size": data_size, "ticks": ticks })
            }
        }
    }
    return opcodes;
}

function run_profiler(path, logs) {

    profiler_result = new Promise((resolve, _) => {

        var gas_used = [];

        var tx_status = [];
        var estimated_ticks = [];
        var estimated_ticks_per_tx = [];
        var tx_size = [];
        var block_in_progress_store = [];
        var block_in_progress_read = [];
        var receipt_size = [];
        let bloom_size = [];
        let nb_reboots = 0;

        var profiler_output_path = "";

        const args = ["--kernel", EVM_INSTALLER_KERNEL_PATH, "--inputs", path, "--preimage-dir", PREIMAGE_DIR];

        const childProcess = spawn(RUN_DEBUGGER_COMMAND, args, {});

        let opcodes = {};

        let precompiles = [];

        childProcess.stdin.write("load inputs\n");

        childProcess.stdin.write("step kernel_run\n");

        if (FAST_MODE)
            childProcess.stdin.write("step inbox\n");
        else
            childProcess.stdin.write("profile\n");

        childProcess.stdin.end();

        childProcess.stdout.on('data', (data) => {
            const output = data.toString();
            if (!output.includes("__wasm_debugger__")) fs.appendFileSync(logs, output)
            const profiler_output_path_regex = /Profiling result can be found in (.+)/;
            const profiler_output_path_match = output.match(profiler_output_path_regex);
            const profiler_output_path_result = profiler_output_path_match
                ? profiler_output_path_match[1]
                : null;
            if (profiler_output_path_result !== null) {
                profiler_output_path = profiler_output_path_result;
                if (KEEP_TEMP) console.log(`Flamechart: ${profiler_output_path}`)
            }
            push_match(output, gas_used, /\bgas_used:\s*(\d+)/g)
            push_match(output, tx_status, /Transaction status: (OK_[a-zA-Z09]+|ERROR_[A-Z_]+)\b/g)
            push_match(output, estimated_ticks, /\bEstimated ticks:\s*(\d+)/g)
            push_match(output, estimated_ticks_per_tx, /\bEstimated ticks after tx:\s*(\d+)/g)
            push_match(output, tx_size, /\bStoring transaction object of size\s*(\d+)/g)
            push_match(output, block_in_progress_store, /\bStoring Block In Progress of size\s*(\d+)/g)
            push_match(output, block_in_progress_read, /\bReading Block In Progress of size\s*(\d+)/g)
            push_match(output, receipt_size, /\bStoring receipt of size \s*(\d+)/g)
            push_match(output, bloom_size, /\[Benchmarking\] bloom size:\s*(\d+)/g)
            push_profiler_sections(output, opcodes, precompiles);
            if (output.includes("Kernel was rebooted.")) nb_reboots++;
        });
        childProcess.on('close', _ => {
            if (!FAST_MODE && profiler_output_path == "") {
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
            if (tx_status.length != bloom_size.length) {
                console.log(new Error("Missing bloom size value (expected: " + tx_status.length + ", actual: " + bloom_size.length + ")"));
            }
            if (block_in_progress_store.length != nb_reboots) {
                console.log(new Error("Missing stored block in progress size value (expected: " + nb_reboots + ", actual: " + block_in_progress_store.length + ")"));
            }
            if (block_in_progress_read.length != nb_reboots) {
                console.log(new Error("Missing read block in progress size value (expected: " + nb_reboots + ", actual: " + block_in_progress_read.length + ")"));
            }
            resolve({
                profiler_output_path,
                gas_costs: gas_used,
                tx_status,
                estimated_ticks,
                estimated_ticks_per_tx,
                tx_size,
                block_in_progress_store,
                block_in_progress_read,
                receipt_size,
                opcodes,
                bloom_size,
                precompiles,
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
    sputnik_runtime_ticks = await get_ticks(path, "EvmHandler$LT$Host$GT$7execute");
    store_transaction_object_ticks = await get_ticks(path, "storage24store_transaction_object");
    store_receipt_ticks = await get_ticks(path, "store_transaction_receipt");
    interpreter_init_ticks = await get_ticks(path, "interpreter(init)");
    interpreter_decode_ticks = await get_ticks(path, "interpreter(decode)");
    fetch_blueprint_ticks = await get_ticks(path, "blueprint5fetch");
    block_finalize = await get_ticks(path, "store_current_block");
    logs_to_bloom = await get_ticks(path, "logs_to_bloom");
    block_in_progress_store_ticks = await get_ticks(path, "store_block_in_progress");
    block_in_progress_read_ticks = await get_ticks(path, "read_block_in_progress");
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
        block_finalize,
        logs_to_bloom,
        block_in_progress_store_ticks,
        block_in_progress_read_ticks
    };
}

// Run given benchmark
async function run_benchmark(path, logs) {
    var inbox_size = fs.statSync(path).size
    run_profiler_result = await run_profiler(path, logs);
    profiler_output_analysis_result = FAST_MODE ? {} : await analyze_profiler_output(run_profiler_result.profiler_output_path);
    if (!KEEP_TEMP && !FAST_MODE) {
        fs.unlink(run_profiler_result.profiler_output_path, (err) => {
            if (err) {
                console.error(err);
            } else {
                console.log(`Profiler output ${run_profiler_result.profiler_output_path} removed.`);
            }
        });
    }
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
    block_in_progress_read = run_benchmark_result.block_in_progress_read;
    block_in_progress_store = run_benchmark_result.block_in_progress_store;
    block_in_progress_store_ticks = run_benchmark_result.block_in_progress_store_ticks;
    block_in_progress_read_ticks = run_benchmark_result.block_in_progress_read_ticks;

    console.log(`Number of transactions: ${tx_status.length}`)
    run_time_index = 0;
    gas_cost_index = 0;
    for (var j = 0; j < tx_status.length; j++) {
        let basic_info_row = {
            benchmark_name,
            signature_verification_ticks: signature_verification_ticks?.[j],
            status: tx_status[j],
            estimated_ticks: estimated_ticks_per_tx[j],
        }
        if (tx_status[j].includes("OK_UNKNOWN")) {
            // no outcome should mean never invoking sputnik
            rows.push(
                {
                    gas_cost: 21000,
                    run_transaction_ticks: run_transaction_ticks?.[j],
                    sputnik_runtime_ticks: 0,
                    store_transaction_object_ticks: store_transaction_object_ticks?.[j],
                    ...basic_info_row
                });

        }
        else if (tx_status[j].includes("OK")) {
            // sputnik runtime called only if not a transfer
            // FIXME: won't work with fees ? + run_time_index unreliable in fast mode
            sputnik_runtime_tick = (gas_costs[gas_cost_index] > 21000) ? sputnik_runtime_ticks?.[run_time_index++] : 0

            rows.push(
                {
                    gas_cost: gas_costs[gas_cost_index],
                    run_transaction_ticks: run_transaction_ticks?.[j],
                    sputnik_runtime_ticks: sputnik_runtime_tick ?? 0,
                    store_transaction_object_ticks: store_transaction_object_ticks?.[j],
                    store_receipt_ticks: run_benchmark_result.store_receipt_ticks?.[j],
                    receipt_size: run_benchmark_result.receipt_size[j],
                    tx_size: tx_size[j],
                    logs_to_bloom: run_benchmark_result.logs_to_bloom?.[j],
                    bloom_size: run_benchmark_result.bloom_size[j],
                    ...basic_info_row
                });
            gas_cost_index += 1;
        } else {
            // we can expect no gas cost, no storage of the tx object, and no run transaction, but there will be signature verification
            // invalide transaction detected: ERROR_NONCE, ERROR_PRE_PAY and ERROR_SIGNATURE, in all cases `caller` is called.
            rows.push(basic_info_row);

        }
    }

    if (!FAST_MODE && run_time_index !== sputnik_runtime_ticks.length) {
        console.log("Warning: runtime not matched with a transaction in: " + benchmark_name);
    }

    // first kernel run
    // the nb of tx correspond to the full inbox, not just those done in first run
    // FIXME: bip_idx unreliable in FAST_MODE
    let bip_idx = 0
    rows.push({
        benchmark_name: benchmark_name + "(all)",
        interpreter_init_ticks: interpreter_init_ticks?.[0],
        interpreter_decode_ticks: interpreter_decode_ticks?.[0],
        fetch_blueprint_ticks: fetch_blueprint_ticks?.[0],
        kernel_run_ticks: kernel_run_ticks?.[0],
        estimated_ticks: estimated_ticks?.[0],
        inbox_size: run_benchmark_result.inbox_size,
        nb_tx: tx_status.length,
        block_in_progress_store: block_in_progress_store[0] ? block_in_progress_store[0] : '',
        block_in_progress_store_ticks: block_in_progress_store[0] ? block_in_progress_store_ticks?.[bip_idx++] : ''
    });

    //reboots
    for (var j = 1; j < kernel_run_ticks?.length; j++) {
        rows.push({
            benchmark_name: benchmark_name + "(all)",
            interpreter_init_ticks: interpreter_init_ticks?.[j],
            interpreter_decode_ticks: interpreter_decode_ticks?.[j],
            fetch_blueprint_ticks: fetch_blueprint_ticks?.[j],
            kernel_run_ticks: kernel_run_ticks?.[j],
            estimated_ticks: estimated_ticks?.[j],
            block_in_progress_store: block_in_progress_store[j] ? block_in_progress_store[j] : '',
            block_in_progress_store_ticks: block_in_progress_store[j] ? block_in_progress_store_ticks?.[bip_idx] : '',
            block_in_progress_read: block_in_progress_read[j - 1], // the first read correspond to second run
            block_in_progress_read_ticks: block_in_progress_read[j - 1] ? block_in_progress_read_ticks?.[bip_idx - 1] : ''
        });
        if (block_in_progress_read[j - 1]) bip_idx++
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

function logs_filename(time) {
    return path.format({ dir: OUTPUT_DIRECTORY, base: `logs_${time}.log` })
}

function output_filename(time) {
    return path.format({ dir: OUTPUT_DIRECTORY, base: `benchmark_result_${time}.csv` })
}

function opcodes_dump_filename(time) {
    return path.format({ dir: OUTPUT_DIRECTORY, base: `dump_opcodes_${time}.json` })
}

function precompiles_filename(time) {
    return path.format({ dir: OUTPUT_DIRECTORY, base: `precompiles_${time}.csv` })
}

function dump_bench_opcode(filename, benchmark_name, opcodes, is_first) {
    if (!is_first)
        fs.appendFileSync(filename, ",")
    fs.appendFileSync(filename, `"${benchmark_name}":${JSON.stringify(opcodes)}`);
}

const PROFILER_OUTPUT_DIRECTORY = OUTPUT_DIRECTORY + "/profiling"
mkdirSync(PROFILER_OUTPUT_DIRECTORY, { recursive: true })



function move_profiler_output(src, bench_name, time) {
    if (!KEEP_TEMP || FAST_MODE) return;
    let dest = path.format({ dir: PROFILER_OUTPUT_DIRECTORY, base: `${bench_name.replaceAll('/', '_')}_${time}.out` })
    fs.rename(path.resolve(src), dest, (err) => {
        if (err && err.code === 'EXDEV') {
            console.log(`WARNING: couldn't move profiler output with rename. Won't try more to avoid taking too long. File remains at ${src}`)
            console.log(err)
            return;
        } else if (err) {
            console.log(`WARNING: error while trying to move profiler output. Benchmarking while continue. File remains at ${src}`)
            console.log(err)
            return;
        }
        console.log(`Finished moving profiling output to ${dest}`)
        return;
    })
}

// Run the benchmark suite and write the result to benchmark_result_${TIMESTAMP}.csv
async function run_all_benchmarks(benchmark_scripts) {
    console.log(`Running benchmarks on: \n[ ${benchmark_scripts.join(',\n  ')}]`);
    var benchmark_fields = [
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
        "logs_to_bloom",
        "bloom_size",
        "estimated_ticks",
        "interpreter_decode_ticks",
        "interpreter_init_ticks",
        "nb_tx",
        "inbox_size",
        "fetch_blueprint_ticks",
        "block_in_progress_read",
        "block_in_progress_read_ticks",
        "block_in_progress_store",
        "block_in_progress_store_ticks",
        "block_finalize",
        "kernel_run_ticks",
        "unaccounted_ticks",
    ];
    var precompiles_field = [
        "address",
        "data_size",
        "ticks",
    ]
    let time = timestamp();
    let output = output_filename(time);
    let opcodes_dump = opcodes_dump_filename(time);
    let precompiles_output = precompiles_filename(time);
    let logs = logs_filename(time)
    console.log(`Output in ${output}`);
    console.log(`Dumped opcodes in ${opcodes_dump}`);
    console.log(`Precompiles in ${precompiles_output}`);
    const benchmark_csv_config = { columns: benchmark_fields };
    const precompile_csv_config = { columns: precompiles_field };
    fs.writeFileSync(output, csv.stringify([], { header: true, ...benchmark_csv_config }));
    fs.writeFileSync(precompiles_output, csv.stringify([], { header: true, ...precompile_csv_config }));
    fs.writeFileSync(logs, "Logging debugger\n")
    fs.writeFileSync(opcodes_dump, "{");
    console.log(`Full logs in ${logs}`)
    for (var i = 0; i < benchmark_scripts.length; i++) {
        var benchmark_script = benchmark_scripts[i];
        var parts = benchmark_script.split("/");
        var benchmark_name = parts[parts.length - 1].split(".")[0];
        console.log(`Benchmarking ${benchmark_script}`);
        fs.appendFileSync(logs, `=================================================\nBenchmarking ${benchmark_script}\n`)
        build_benchmark_scenario(benchmark_script);
        run_benchmark_result = await run_benchmark("transactions.json", logs);
        benchmark_log = log_benchmark_result(benchmark_name, run_benchmark_result);
        fs.appendFileSync(output, csv.stringify(benchmark_log, benchmark_csv_config));
        fs.appendFileSync(precompiles_output, csv.stringify(run_benchmark_result.precompiles, precompile_csv_config))
        dump_bench_opcode(opcodes_dump, benchmark_name, run_benchmark_result.opcodes, i == 0);
        move_profiler_output(run_benchmark_result.profiler_output_path, benchmark_script, time)
    }
    fs.appendFileSync(opcodes_dump, "}");
    console.log("Benchmarking complete");
    fs.appendFileSync(logs, "=================================================\nBenchmarking complete.\n")
    execSync("rm transactions.json");
}

benchmark_scripts = require("./benchmarks_list.json")
run_all_benchmarks(benchmark_scripts.filter(filter_name));
