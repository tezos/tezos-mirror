// SPDX-FileCopyrightText: 2023 Marigold <contact@marigold.dev>
//
// SPDX-License-Identifier: MIT

const path = require('node:path')
module.exports = { print_analysis }
const utils = require("./utils")
const OUTPUT = 'block_finalization_data.csv'
const UPPER_BOUND = 125000000

function print_analysis(infos, dir = "analysis_result") {
    let mlr = utils.make_lr(infos.block_finalization, (x) => x.nb_tx, (y) => y.block_finalize)
    console.log(`Linear Regression: ${utils.print_lr(mlr, "nbtx")} `)

    utils.print_csv(dir, OUTPUT, infos.block_finalization, ["benchmark_name", "inbox_size", "nb_tx", "block_finalize"])

    console.log(`current model: Y = ${UPPER_BOUND} `)
    return utils.print_summary_errors(infos.block_finalization, datum => { return datum.block_finalize - UPPER_BOUND })
}
