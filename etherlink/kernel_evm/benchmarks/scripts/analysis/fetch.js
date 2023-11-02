// SPDX-FileCopyrightText: 2023 Marigold <contact@marigold.dev>
//
// SPDX-License-Identifier: MIT

const path = require('node:path')
module.exports = { print_fetch_analysis }
const fs = require('fs');
const csv = require('csv-stringify/sync')
const utils = require("./utils")
const OUTPUT = 'fetch_data.csv'
const UPPER_BOUND = 1_000_000_000

function print_fetch_analysis(infos, dir = "analysis_result") {
    // prepare data
    const csv_config = {
        header: true,
        columns: ["benchmark_name", "size", "nb_tx", "ticks"]
    }
    fs.mkdirSync(dir, { recursive: true })
    fs.writeFileSync(path.format({ dir, name: OUTPUT }), csv.stringify(infos.fetch_data, csv_config))

    // compute errors
    console.log(`current model: Y = ${UPPER_BOUND}`)
    return utils.print_summary_errors(infos.fetch_data, datum => { return datum.ticks - UPPER_BOUND })
}
