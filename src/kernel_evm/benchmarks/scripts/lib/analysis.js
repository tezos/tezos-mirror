// SPDX-FileCopyrightText: 2023 Marigold <contact@marigold.dev>
//
// SPDX-License-Identifier: MIT

const number_formatter_compact = Intl.NumberFormat('en', { notation: 'compact', compactDisplay: 'long' });
const number_formatter = Intl.NumberFormat('en', {});
const BASE_GAS = 21000
const RUN_TRANSACTION_OVERHEAD = 560_000
const CREATE_STORAGE_CUTOFF = 600_000

module.exports = { init_analysis, check_result, processRecord }

function init_analysis() {
    let empty = {
        // total amount of gas consumed
        total_gas: 0,
        // total amount of ticks used in run_transaction_ticks
        total_ticks_tx: 0,
        tick_per_gas: [],
        run_transaction_overhead: [],
        init: 0,
        decode: 0,
        signatures: [],
        nb_kernel_run: 0,
        nb_call: 0,
        nb_create: 0,
        nb_transfer: 0,
        kernel_runs: []

    };
    return empty
}

function print_analysis(infos) {
    const tickPerGas = infos.total_ticks_tx / infos.total_gas
    console.info(`Kernels infos`)
    console.info(`Overall tick per gas: ~${tickPerGas.toFixed()}`)
    console.info(`Tick per gas: ${pp_avg_max(infos.tick_per_gas)}`)
    console.info(`Signature verification: ${pp_avg_max(infos.signatures)}`)
    console.info(`Decoding: ${pp(infos.decode)} ticks`)
    console.info(`Initialisation: ${pp(infos.init)} ticks`)
    console.info(`transfer overhead: ${pp_avg_max(infos.run_transaction_overhead)} `)
    console.info(`-------------------------------------------------------`)
    console.info(`Benchmark run infos`)
    console.info(`Number of tx: ${infos.signatures.length}`)
    console.info(`Number of kernel run: ${infos.nb_kernel_run}`)
}

function processRecord(record, acc) {
    if (is_transaction(record)) process_transaction_record(record, acc)
    else process_bench_record(record, acc)
}

function process_transaction_record(record, acc) {
    acc.signatures.push(record.signature_verification_ticks)
    if (is_transfer(record)) process_transfer(record, acc)
    else if (is_create(record)) process_create(record, acc)
    else process_call(record, acc)
}

function process_transfer(record, acc) {
    acc.run_transaction_overhead.push(record.run_transaction_ticks)
}

function process_create(record, acc) {

}

function process_call(record, acc) {
    let gas = record.gas_cost - BASE_GAS
    let ticks = record.run_transaction_ticks - RUN_TRANSACTION_OVERHEAD
    acc.total_gas += gas
    acc.total_ticks_tx += ticks
    acc.tick_per_gas.push(ticks / gas)
}

function process_bench_record(record, acc) {
    if (!isNaN(record.interpreter_decode_ticks)) acc.init = record.interpreter_decode_ticks
    if (!isNaN(record.interpreter_init_ticks)) acc.decode = record.interpreter_init_ticks
    if (!isNaN(record.interpreter_decode_ticks)) acc.nb_kernel_run += 1
    if (!isNaN(record.kernel_run_ticks)) acc.kernel_runs.push(record.kernel_run_ticks)
}

function is_transfer(record) {
    return record.gas_cost == 21000
}
function is_create(record) {
    return record.store_transaction_object_ticks > CREATE_STORAGE_CUTOFF
}

function is_transaction(record) {
    return !(isNaN(record.gas_cost)
        || isNaN(record.run_transaction_ticks)
        || isNaN(record.signature_verification_ticks))
}

function check_result(infos) {
    const tickPerGas = infos.total_ticks_tx / infos.total_gas
    print_analysis(infos)
    const is_error = tickPerGas > 2000
    if (is_error) {
        console.error(`Tick per gas too high!`)
        return 1
    }
    return 0
}

function pp(number) {
    return number_formatter_compact.format(number)
}
function pp_full(number) {
    return number_formatter.format(number)

}

function average(arr) {
    let avg = arr.reduce((p, c) => p + c, 0) / arr.length
    return pp(avg.toFixed())
}

function maximum(arr) {
    return pp_full(arr.reduce((p, c) => Math.max(p, c), 0).toFixed())
}

function pp_avg_max(arr) {
    return `~${average(arr)} (max: ${maximum(arr)})`
}