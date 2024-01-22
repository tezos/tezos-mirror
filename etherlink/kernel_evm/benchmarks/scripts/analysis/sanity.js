// SPDX-FileCopyrightText: 2023 Marigold <contact@marigold.dev>
//
// SPDX-License-Identifier: MIT

const { is_transaction, BASE_GAS } = require('./utils')


module.exports = { init_sanity, check_record, print_summary }

// initialize the accumulator
function init_sanity() { return [] }

// print a summaro of all errors recorded in an accumulator
function print_summary(sanity_acc) {
    console.info(`-------------------------------------------------------`)
    console.log(`Sanity check: ${sanity_acc.length} problems spotted`)
    for (datum of sanity_acc) {
        console.log(`${datum.name}: ${datum.type} record ->  ${datum.errors.join(', ')}`)
    }
    console.info(`-------------------------------------------------------`)
}

// Check that a given record is sane
function check_record(record, sanity_acc) {
    let acc = { name: record.benchmark_name, type: "", errors: [] }
    if (is_transaction(record)) check_transaction_record(record, acc)
    else check_bench_record(record, acc)
    if (acc.errors.length > 0)
        sanity_acc.push(acc)
}

// not a transaction record
function check_bench_record(record, acc) {
    if (isNaN(record.interpreter_init_ticks)) check_bench_all_record(record, acc)
    else check_run_record(record, acc)
}

// record added at the end with information related to whole benchmark
function check_bench_all_record(record, acc) {
    acc.type = "benchmark"
    if (isNaN(record.unaccounted_ticks)) record_error(acc, `missing unaccounted ticks`)
    if (isNaN(record.block_finalize)) record_error(acc, `missing block finalization ticks`)
}

// record linked to a kernel_run
function check_run_record(record, acc) {
    if (isNaN(record.interpreter_init_ticks)) record_error(acc, `missing init ticks`)
    if (isNaN(record.interpreter_decode_ticks)) record_error(acc, `missing decode ticks`)
    if (isNaN(record.kernel_run_ticks)) record_error(acc, `missing kernel run ticks`)
    if (isNaN(record.estimated_ticks)) record_error(acc, `missing estimated ticks`)

    if (!isNaN(record.fetch_blueprint_ticks)) {
        // first run
        acc.type = "first run"
        if (isNaN(record.inbox_size)) record_error(acc, `missing inbox size`)
        if (isNaN(record.nb_tx)) record_error(acc, `missing number of tx`)
        if (isNaN(record.bip_store)) record_error(acc, `missing bip store ticks`)
    } else {
        // not first run
        acc.type = "reboot run"
        if (isNaN(record.bip_read)) record_error(acc, `missing bip read ticks`)
    }
}

// record of a transaction
function check_transaction_record(record, acc) {
    acc.type = "transaction"
    if (record.benchmark_name === '') record_error(acc, `missing benchmark name`)
    if (isNaN(record.signature_verification_ticks)) record_error(acc, `missing signature verification ticks`)
    if (isNaN(record.estimated_ticks)) record_error(acc, `missing estimated ticks`)
    if (record.status.includes(`OK`)) {
        if (isNaN(record.store_transaction_object_ticks)) record_error(acc, `missing tx storing ticks`)
        if (isNaN(record.store_receipt_ticks)) record_error(acc, `missing receipt storing ticks`)

        if (isNaN(record.gas_cost)) record_error(acc, `missing gas`)
        if (isNaN(record.run_transaction_ticks)) record_error(acc, `missing execution ticks`)
        if (isNaN(record.sputnik_runtime_ticks)) record_error(acc, `missing sputnik ticks`)
        if (isNaN(record.tx_size)) record_error(acc, `missing tx size`)
        if (isNaN(record.receipt_size)) record_error(acc, `missing receipt size`)
        if (!record.status.includes(`true`)) record_error(acc, `${record.status}`)
    } else {
        record_error(acc, `${record.status}`)
    }

}

function record_error(acc, msg) {
    acc.errors.push(msg)
}