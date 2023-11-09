// SPDX-FileCopyrightText: 2023 Marigold <contact@marigold.dev>
//
// SPDX-License-Identifier: MIT

const path = require('node:path')
module.exports = { print_analysis }
const fs = require('fs');
const csv = require('csv-stringify/sync')
const utils = require("./utils")
const OUTPUT_STORE = 'queue_storing_data.csv'
const OUTPUT_READ = 'queue_reading_data.csv'
const READ_UPPER_BOUND = 500_000_000
const STORE_UPPER_BOUND = 1_100_000_000

function print_analysis(infos, dir = "analysis_result") {
    const read_data = infos.runs_infos.filter((d) => !!d.queue_read);
    let mlr_read = utils.make_lr(read_data, (x) => x.queue_read, (y) => y.queue_read_ticks)
    console.log(`[read] Computed LR: ${utils.print_lr(mlr_read)} `)
    let error_read = utils.print_summary_errors(read_data, datum => { return datum.queue_read_ticks - READ_UPPER_BOUND }, "[read]")
    fs.writeFileSync(path.format({ dir, name: OUTPUT_READ }), csv.stringify(read_data), {
        header: true,
        columns: ["benchmark_name", "queue_read", "queue_read_ticks"]
    })

    const store_data = infos.runs_infos.filter((d) => !!d.queue_store);
    let mlr_store = utils.make_lr(store_data, (x) => x.queue_store, (y) => y.queue_store_ticks)
    console.log(`[store] Computed LR: ${utils.print_lr(mlr_store)} `)
    let error_store = utils.print_summary_errors(store_data, datum => { return datum.queue_store_ticks - STORE_UPPER_BOUND }, "[store]")
    fs.writeFileSync(path.format({ dir, name: OUTPUT_STORE }), csv.stringify(store_data), {
        header: true,
        columns: ["benchmark_name", "queue_store", "queue_store_ticks"]
    })
    return error_read + error_store;
}