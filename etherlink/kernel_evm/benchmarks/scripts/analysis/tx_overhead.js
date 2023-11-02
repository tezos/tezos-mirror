// SPDX-FileCopyrightText: 2023 Marigold <contact@marigold.dev>
//
// SPDX-License-Identifier: MIT

const path = require('node:path')
module.exports = { print_analysis }
const fs = require('fs');
const csv = require('csv-stringify/sync');
const utils = require("./utils")
const OUTPUT = 'tx_overhead_data.csv'
const MODEL = { intercept: 1_150_000, coef: 880 }

function print_analysis(infos, dir = "analysis_result") {
    const data = infos.tx_overhead;
    for (datum of data) {
        datum.tx_overhead = datum.run_transaction_ticks - datum.sputnik_runtime_ticks
    }
    console.log(`Current model: ${utils.print_model(MODEL, "tx_size")}`)
    let lr = utils.make_lr(infos.tx_register, (x) => x.tx_size, (y) => y.tx_overhead)
    console.log(`Computed LR: ${utils.print_lr(lr)} `)
    let error = utils.print_summary_errors(infos.tx_register, datum => { return datum.tx_overhead - utils.predict_linear_model(MODEL, datum.tx_size) })

    const csv_config = {
        header: true,
        columns: ["benchmark_name", "status", "gas_cost", "tx_size", "run_transaction_ticks", "sputnik_runtime_ticks", "tx_overhead"]
    };
    fs.writeFileSync(path.format({ dir, name: OUTPUT }), csv.stringify(data, csv_config))

    return error
}