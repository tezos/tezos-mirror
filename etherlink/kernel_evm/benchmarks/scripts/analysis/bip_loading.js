// SPDX-FileCopyrightText: 2024 Marigold <contact@marigold.dev>
//
// SPDX-License-Identifier: MIT

module.exports = { print_analysis }
const utils = require("./utils")
const pdfUtils = require("./pdf_utils")
const OUTPUT = 'bip_loading.csv'
const MODEL = {
    intercept:3_000_000,
    coef:3700,
}

function print_analysis(infos, dir = "analysis_result", doc) {
    pdfUtils.h1("BlockInProgress from blueprint", doc);
    // prepare data
    let data = infos.bip_loading
    let errors = utils.print_page(data, {
        fx: (d) => d.bip_size,
        fy: (d) => d.next_bip_ticks,
        model: MODEL,
        labels: {
            title: "Tick model for loading a blueprint",
            x: "size of blueprint",
        },
        context: {
            csv_filepath: OUTPUT,
            csv_columns: ["benchmark_name", "next_bip_ticks", "bip_size", "chunks_in_bip", "txs_in_bip"],
            dir,
            doc
        }
    });
    return errors;
}
