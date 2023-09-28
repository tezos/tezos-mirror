// SPDX-FileCopyrightText: 2023 Marigold <contact@marigold.dev>
//
// SPDX-License-Identifier: MIT

const BASE_GAS = 21000
const CREATE_STORAGE_CUTOFF = 600_000


module.exports = { is_transfer, is_create, is_transaction, BASE_GAS }

function is_transfer(record) {
    return record.gas_cost == BASE_GAS
}
function is_create(record) {
    return record.store_transaction_object_ticks > CREATE_STORAGE_CUTOFF
}

function is_transaction(record) {
    return !record.benchmark_name.includes("(all)")
}