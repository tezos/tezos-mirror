// SPDX-FileCopyrightText: 2023 Marigold <contact@marigold.dev>
//
// SPDX-License-Identifier: MIT

const { is_transaction, is_scenario, is_run, is_blueprint_reading } = require('./utils')

const fs = require('fs')
const path = require('node:path')

let FILENAME = "sanity.log";
module.exports = { init_sanity, check_record, print_summary }

// initialize the accumulator
function init_sanity() { return [] }

function print(str, filepath) {
    fs.appendFileSync(filepath, `${str} \n`);
}

// print a summaro of all errors recorded in an accumulator
function print_summary(sanity_acc, dirname) {
    let filepath = path.format({ dir: dirname, base: FILENAME })

    fs.writeFileSync(filepath, '')
    print(`-------------------------------------------------------`, filepath)
    console.log(`-------------------------------------------------------`)
    print(`Sanity check: ${sanity_acc.length} problems spotted`, filepath)
    if(sanity_acc.length > 0){
        console.log(`Sanity check: ${sanity_acc.length} problems spotted (see ${filepath})`)
    } else {
        console.log("Sanity check: no problem")
    }
    for (datum of sanity_acc) {
        if (typeof datum === 'string' || datum instanceof String)
            print(datum, filepath);
        else
            print(`${datum?.name}: ${datum?.type} record ->  ${datum?.errors.join(', ')}`, filepath);
    }
    print(`-------------------------------------------------------`, filepath)
}

// Check that a given record is sane
function check_record(record, sanity_acc) {
    let acc = { name: record.benchmark_name, type: "", errors: [] }
    if (is_transaction(record)) check_transaction_record(record, acc)
    else if (is_scenario(record)) check_bench_all_record(record, acc);
    else if (is_run(record)) check_run_record(record, acc);
    else if (is_blueprint_reading(record)) check_bip_record(record, acc);

    if (acc.errors.length > 0)
        sanity_acc.push(acc)
}

function check_bip_record(record, acc) {
    acc.type = "bip"
    if (isNaN(record.next_bip_ticks)) record_error(acc, `missing blueprint ticks`);
    if (isNaN(record.chunks_in_bip)) record_error(acc, `missing nb chunks`);
    if (isNaN(record.txs_in_bip)) record_error(acc, `missing nb of txs`);
    if (isNaN(record.bip_size)) record_error(acc, `missing bip_size`);
}

// record added at the end with information related to whole benchmark
function check_bench_all_record(record, acc) {
    acc.type = "benchmark"
    if (isNaN(record.unaccounted_ticks)) record_error(acc, `missing unaccounted ticks`)
    if (isNaN(record.block_finalize)) record_error(acc, `missing block finalization ticks`)
    if (isNaN(record.inbox_size)) record_error(acc, `missing inbox size`)
    if (isNaN(record.nb_tx)) record_error(acc, `missing nb txs`)
    if (isNaN(record.nb_msg)) record_error(acc, `missing nb messages`)
    if (isNaN(record.blueprint_chunks)) record_error(acc, `missing nb of blueprint chunks`)
    if (isNaN(record.delayed_inputs)) record_error(acc, `missing nb of blueprint chunks`)
    if(parseInt(record.blueprint_chunks) === 0
        && parseInt(record.delayed_inputs) === 0)
        record_error(acc, `no blueprint chunk nor delayed input`)
}

// record linked to a kernel_run
function check_run_record(record, acc) {
    acc.type = "run"
    if (isNaN(record.interpreter_init_ticks)) record_error(acc, `missing init ticks`)
    if (isNaN(record.interpreter_decode_ticks)) record_error(acc, `missing decode ticks`)
    if (isNaN(record.kernel_run_ticks)) record_error(acc, `missing kernel run ticks`)
    if (isNaN(record.estimated_ticks)) record_error(acc, `missing estimated ticks`)
    if (isNaN(record.stage_one_ticks)) record_error(acc, `missing stage one ticks`)
}

// record of a transaction
function check_transaction_record(record, acc) {
    acc.type = "transaction"
    if (record.benchmark_name === '') record_error(acc, `missing benchmark name`)
    if (isNaN(record.signature_verification_ticks)) record_error(acc, `missing signature verification ticks`)
    if (isNaN(record.hashing_ticks)) record_error(acc, `missing hashing ticks`)
    if (isNaN(record.estimated_ticks)) record_error(acc, `missing estimated ticks`)
    if (record.status.includes(`OK`)) {
        if (isNaN(record.store_transaction_object_ticks)) record_error(acc, `missing tx storing ticks`)
        if (isNaN(record.store_receipt_ticks)) record_error(acc, `missing receipt storing ticks`)

        if (isNaN(record.gas_cost)) record_error(acc, `missing gas`)
        if (isNaN(record.run_transaction_ticks)) record_error(acc, `missing execution ticks`)
        if (isNaN(record.sputnik_runtime_ticks)) record_error(acc, `missing sputnik ticks`)
        if (isNaN(record.tx_size)) record_error(acc, `missing tx size`)
        if (isNaN(record.receipt_size)) record_error(acc, `missing receipt size`)
        // only acceptable Ok_false is for reboots (OutOfTicks) or expected
        if (!(record.status === record.expected || record.reason?.includes(`OutOfTicks`))
            && !record.status?.includes("true"))
            record_error(acc, `${record.status} (${record.reason})`)
    } else {
        record_error(acc, `${record.status} : ${record.reason}`)
    }

}

function record_error(acc, msg) {
    acc.errors.push(msg)
}
