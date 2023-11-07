// SPDX-FileCopyrightText: 2023 Marigold <contact@marigold.dev>
//
// SPDX-License-Identifier: MIT

const BASE_GAS = 21000
const CREATE_STORAGE_CUTOFF = 600_000

const MLR = require("ml-regression-multivariate-linear")

module.exports = { is_transfer, is_create, is_transaction, BASE_GAS, make_lr, print_lr }

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