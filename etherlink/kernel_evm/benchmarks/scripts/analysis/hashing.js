// SPDX-FileCopyrightText: 2024 Marigold <contact@marigold.dev>
//
// SPDX-License-Identifier: MIT

module.exports = { print_analysis }
const utils = require("./utils")
const pdfUtils = require("./pdf_utils")
const OUTPUT = 'hashing.csv'
const MODEL = {
    intercept: 200_000,
    coef: 1400
}

function print_analysis(infos, dir = "analysis_result", doc) {
    pdfUtils.h1("Hashing", doc);
    // prepare data
    let data = infos.transaction
    let errors = utils.print_page(data, {
        fx: (d) => d.data_size,
        fy: (d) => d.hashing_ticks,
        model: MODEL,
        labels: {
            title: "Tick model for hashing",
            x: "data size",
        },
        context: {
            csv_filepath: OUTPUT,
            csv_columns: ["benchmark_name", "hashing_ticks", "data_size"],
            dir,
            doc
        }
    });
    return errors;
}
