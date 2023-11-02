// SPDX-FileCopyrightText: 2023 Marigold <contact@marigold.dev>
//
// SPDX-License-Identifier: MIT

const BASE_GAS = 21000
const CREATE_STORAGE_CUTOFF = 600_000

const MLR = require("ml-regression-multivariate-linear")

module.exports = { is_transfer, is_create, is_transaction, BASE_GAS, make_lr, print_lr, print_summary_errors, print_model, predict_linear_model }

function is_transfer(record) {
    return record.gas_cost == BASE_GAS
}
function is_create(record) {
    return record.store_transaction_object_ticks > CREATE_STORAGE_CUTOFF
}

function is_transaction(record) {
    return !record.benchmark_name.includes("(all)")
}

function make_lr(data, select_x, select_y) {

    var X = []
    var Y = []
    for (datum of data) {
        let x = select_x(datum)
        let y = select_y(datum)
        if (!!x && !!y) {
            X.push([x])
            Y.push([y])
        }
    }
    if (X.length > 0) {
        let mlr = new MLR(X, Y)
        return mlr
    }
}

function print_lr(lr, var_name = "size") {
    if (!!lr) return `Y = ${lr.weights[1][0].toFixed()} + ${lr.weights[0][0].toFixed()} * ${var_name}`
    else return "no linear regression available"
}

function print_summary_errors(data, compute_error, prefix = "") {
    let max_error_current = 0;
    let nb_error = 0
    for (datum of data) {
        let error = compute_error(datum)
        if (error > 0) nb_error += 1
        if (!isNaN(error)) max_error_current = Math.max(max_error_current, error)
    }
    console.log(`${prefix} nb of errors: ${nb_error} ; maximum error: ${max_error_current} ticks`)
    return nb_error
}

function print_model(model, var_name) {
    return `Y = ${model.intercept} + ${model.coef} * ${var_name}`
}

function predict_linear_model(model, x) {
    if (isNaN(x)) return model.intercept
    return model.intercept + model.coef * x
}