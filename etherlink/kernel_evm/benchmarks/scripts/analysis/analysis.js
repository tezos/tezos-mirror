// SPDX-FileCopyrightText: 2023 Marigold <contact@marigold.dev>
//
// SPDX-License-Identifier: MIT

const { is_transfer, is_create, is_transaction, BASE_GAS } = require('./utils')
const fs = require('fs')
const fetch = require('./fetch')
const block_finalization = require('./block_finalization')
const tx_register = require('./tx_register')
const tx_overhead = require('./tx_overhead')
const queue = require('./queue')

const number_formatter_compact = Intl.NumberFormat('en', { notation: 'compact', compactDisplay: 'long' });
const number_formatter = Intl.NumberFormat('en', {});

module.exports = { init_analysis, check_result, process_record }

function init_analysis() {
    let empty = {
        // total amount of gas consumed
        total_gas: 0,
        // total amount of ticks used in run_transaction_ticks
        sputnik_ticks: 0,
        pure_transfers_ticks: [],
        init: 0,
        decode: 0,
        signatures: [],
        nb_kernel_run: 0,
        nb_call: 0,
        nb_transfer: 0,
        kernel_runs: [],
        fetch_data: [],
        block_finalization: [],
        tx_register: [],
        tx_overhead: [],
        runs_infos: []

    };
    return empty
}

function print_analysis(infos) {
    console.info(`-------------------------------------------------------`)
    console.info(`Fetch Analysis`)
    console.info(`----------------------------------`)
    let error_fetch = fetch.print_fetch_analysis(infos)
    console.info(`-------------------------------------------------------`)
    console.info(`Block Finalization Analysis`)
    console.info(`----------------------------------`)
    let error_block_finalization = block_finalization.print_analysis(infos)
    console.info(`-------------------------------------------------------`)
    console.info(`Queue read and storage analysis`)
    console.info(`----------------------------------`)
    let error_queue = queue.print_analysis(infos)
    console.info(`-------------------------------------------------------`)
    console.info(`Transaction Registering Analysis`)
    console.info(`----------------------------------`)
    let error_register = tx_register.print_analysis(infos)
    console.info(`-------------------------------------------------------`)
    console.info(`Transaction Overhead Analysis`)
    console.info(`----------------------------------`)
    // model is known to fall short as an overapproximation
    let _error_tx_overhead = tx_overhead.print_analysis(infos)
    console.info(`-------------------------------------------------------`)
    console.info(`Kernels infos`)
    console.info(`----------------------------------`)
    console.info(`Decoding: ${pp(infos.decode)} ticks`)
    console.info(`Initialisation: ${pp(infos.init)} ticks`)
    console.info(`Signature verification: ${pp_avg_max(infos.signatures.filter((x) => !!x))}`)
    console.info(`Transfer tick cost: ${pp_avg_max(infos.pure_transfers_ticks.filter((x) => !!x))} `)
    console.info(`-------------------------------------------------------`)
    console.info(`Benchmark run stats`)
    console.info(`----------------------------------`)
    console.info(`Total gas in execution: ${pp(infos.total_gas)}`)
    console.info(`Total ticks in sputnik: ${pp(infos.sputnik_ticks)}`)
    console.info(`Number of tx: ${infos.signatures.length}`)
    console.info(`Number of kernel run: ${infos.nb_kernel_run}`)
    console.info(`Number of transfers: ${infos.nb_transfer}`)
    console.info(`Number of create/call: ${infos.nb_call}`)
    console.info(`Number of kernel run: ${infos.nb_kernel_run}`)
    console.info(`Number of blocks: ${infos.block_finalization.length}`)
    console.info(`-------------------------------------------------------`)
    return error_fetch + error_block_finalization + error_register + error_queue

}

function process_record(record, acc) {
    if (is_transaction(record)) process_transaction_record(record, acc)
    else process_bench_record(record, acc)
}

function process_bench_record(record, acc) {
    acc.runs_infos.push(record)
    if (!isNaN(record.interpreter_decode_ticks)) {
        acc.nb_kernel_run += 1
        acc.decode = Math.max(acc.decode, record.interpreter_decode_ticks)
        acc.init = Math.max(acc.init, record.interpreter_init_ticks)
    }
    if (!isNaN(record.kernel_run_ticks)) acc.kernel_runs.push(record.kernel_run_ticks)

    // Adds info needed for fetch analysis
    if (!isNaN(record.fetch_blueprint_ticks) && !isNaN(record.nb_tx)) {
        acc.fetch_data.push({
            ticks: record.fetch_blueprint_ticks,
            size: record.inbox_size,
            nb_tx: record.nb_tx,
            benchmark_name: record.benchmark_name
        })
    }

    // Adds infos needed for block finalization analysis
    if (!isNaN(record.inbox_size)) {
        acc.block_finalization.push(record)
    }
    if (!isNaN(record.block_finalize)) {
        // add block_finalize info to last record if same benchmark
        let last_record = acc.block_finalization.pop()
        if (last_record.benchmark_name == record.benchmark_name) {
            last_record.block_finalize = record.block_finalize
            acc.block_finalization.push(last_record)
        } else {
            console.error("[Error] couldn't find correct finalize information")
        }
    }
}

function process_transaction_record(record, acc) {
    acc.signatures.push(record.signature_verification_ticks)
    if (is_transfer(record)) process_transfer(record, acc)
    else process_execution(record, acc)

    // Adds infos for tx registration analysis
    if (!isNaN(record.tx_size) && !isNaN(record.store_transaction_object_ticks))
        acc.tx_register.push(record)

    // Adds infos for transaction overhead analysis
    if (!isNaN(record.tx_size) && !isNaN(record.sputnik_runtime_ticks) && !isNaN(record.run_transaction_ticks))
        acc.tx_overhead.push(record)
}

function process_transfer(record, acc) {
    acc.pure_transfers_ticks.push(record.run_transaction_ticks)
    acc.nb_transfer++
}


function process_execution(record, acc) {
    acc.nb_call++
    let gas = record.gas_cost - BASE_GAS
    if (!isNaN(record.gas_cost)) acc.total_gas += gas
    if (!isNaN(record.sputnik_runtime_ticks)) acc.sputnik_ticks += record.sputnik_runtime_ticks
}

function check_result(infos) {
    const tickPerGas = infos.sputnik_ticks / infos.total_gas
    let nb_errors = print_analysis(infos)
    const is_error = nb_errors > 0
    if (is_error) {
        console.info(`-------------------------------------------------------`)
        console.error(`WARNING: too many model underestimation (${nb_errors})`)
        console.info(`-------------------------------------------------------`)
        return 1
    } else {
        console.log(`Global tpg: ${tickPerGas}`)
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