// SPDX-FileCopyrightText: 2023 Marigold <contact@marigold.dev>
//
// SPDX-License-Identifier: MIT

const path = require('node:path')
module.exports = { print_analysis }
const utils = require("./utils")
const OUTPUT = 'register_tx_data.csv'
const MODEL_OBJ = { intercept: 200000, coef: 880 }
const MODEL_RECEIPT = { intercept: 200000, coef: 960 }
const MODEL_LOGBLOOM = { intercept: 5300, coef: 85000 }

function print_analysis(infos, dir = "analysis_result") {

    function print_csv(name, columns) {
        utils.print_csv(dir, name + OUTPUT, infos.tx_register, columns)
    }

    // transaction object
    console.log(`[object] Current model: ${utils.print_model(MODEL_OBJ, "size")}`)
    let obj_lr = utils.make_lr(infos.tx_register, (x) => x.tx_size, (y) => y.store_transaction_object_ticks)
    console.log(`[object] Computed LR: ${utils.print_lr(obj_lr)} `)
    let error_object = utils.print_summary_errors(infos.tx_register, datum => { return datum.tx_size - utils.predict_linear_model(MODEL_OBJ, datum.receipt_size) }, "[object]")

    print_csv(
        "object_",
        ["benchmark_name", "tx_size", "store_transaction_object_ticks",]
    )

    // receipt
    console.log(`[receipt] Current model: ${utils.print_model(MODEL_RECEIPT, "size")}`)
    let receipt_lr = utils.make_lr(infos.tx_register, (x) => x.receipt_size, (y) => y.store_receipt_ticks)
    console.log(`[receipt] Computed LR: ${utils.print_lr(receipt_lr)} `)
    let error_receipt = utils.print_summary_errors(infos.tx_register, datum => { return datum.store_receipt_ticks - utils.predict_linear_model(MODEL_RECEIPT, datum.receipt_size) }, "[receipt]")

    print_csv(
        "receipt_",
        ["benchmark_name", "receipt_size", "store_receipt_ticks"]
    )

    // bloom
    console.log(`[bloom] Current model: ${utils.print_model(MODEL_LOGBLOOM, "size")}`)
    let bloom_lr = utils.make_lr(infos.tx_register, (x) => x.bloom_size, (y) => y.logs_to_bloom)
    console.log(`[bloom] Computed LR: ${utils.print_lr(bloom_lr)} `)
    let error_bloom = utils.print_summary_errors(infos.tx_register, datum => { return datum.logs_to_bloom - utils.predict_linear_model(MODEL_LOGBLOOM, datum.bloom_size) }, "[bloom]")

    print_csv(
        "bloom_",
        ["benchmark_name", "bloom_size", "logs_to_bloom"]
    )

    let errors = error_receipt + error_object + error_bloom
    console.log(`Total errors: ${errors}`)
    return errors
}
