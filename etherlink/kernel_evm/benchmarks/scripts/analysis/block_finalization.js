// SPDX-FileCopyrightText: 2023 Marigold <contact@marigold.dev>
//
// SPDX-License-Identifier: MIT

const path = require('node:path')
module.exports = { print_analysis }
const utils = require("./utils")
const pdfUtils = require("./pdf_utils")
const OUTPUT = 'block_finalization_data.csv'
const UPPER_BOUND = 125_000_000
const MODEL = {
    intercept: UPPER_BOUND,
    coef: 0,
}

function print_analysis(infos, dir = "analysis_result", doc) {
    pdfUtils.h1("Block finalization", doc);
    let data = infos.block_finalization
    let errors = utils.print_page(data, {
        fx: (d) => d.nb_tx,
        fy: (d) => d.block_finalize,
        model: MODEL,
        labels: {
            title: "Tick model for finalizing a block",
            x: "Number of transactions in the block",
        },
        context: {
            csv_filepath: OUTPUT,
            csv_columns: ["benchmark_name", "inbox_size", "nb_tx", "block_finalize"],
            dir,
            doc
        }
    });
    return errors;

}
