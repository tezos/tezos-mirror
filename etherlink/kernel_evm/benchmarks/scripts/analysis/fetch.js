// SPDX-FileCopyrightText: 2023 Marigold <contact@marigold.dev>
//
// SPDX-License-Identifier: MIT

const path = require('node:path')
module.exports = { print_fetch_analysis }
const utils = require("./utils")
const OUTPUT = 'fetch_data.csv'
const UPPER_BOUND = 1_000_000_000

function print_fetch_analysis(infos, dir = "analysis_result") {
    // prepare data

    utils.print_csv(dir, OUTPUT, infos.fetch_data, ["benchmark_name", "size", "nb_tx", "ticks"])

    // compute errors
    console.log(`current model: Y = ${UPPER_BOUND}`)
    return utils.print_summary_errors(infos.fetch_data, datum => { return datum.ticks - UPPER_BOUND })
}
