// SPDX-FileCopyrightText: 2023 Marigold <contact@marigold.dev>
//
// SPDX-License-Identifier: MIT

const path = require('node:path')
module.exports = { print_analysis }
const utils = require("./utils")
const pdfUtils = require("./pdf_utils")
const OUTPUT_STORE = 'queue_storing_data.csv'
const OUTPUT_READ = 'block_in_progress_reading_data.csv'
const READ_UPPER_BOUND = 500_000_000
const STORE_UPPER_BOUND = 1_100_000_000
const MODEL_READ = {
    intercept: READ_UPPER_BOUND,
    coef: 0,
}
const MODEL_STORE = {
    intercept: STORE_UPPER_BOUND,
    coef: 0,
}

function print_analysis(infos, dir = "analysis_result", doc) {
    let outline = pdfUtils.h1("Reboot", doc);
    let data = infos.runs_infos;
    let context_init = { doc, dir };

    pdfUtils.h2("Read block in progress", doc, outline);
    let error_read = utils.print_page(data.filter((d) => !!d.block_in_progress_read), {
        fx: (d) => d.block_in_progress_read,
        fy: (d) => d.block_in_progress_read_ticks,
        model: MODEL_READ,
        labels: {
            title: "Tick model for reading block in progress after reboot",
            x: "size of bip",
            prefix: "[read] "
        },
        context: {
            csv_filepath: OUTPUT_READ,
            csv_columns: ["benchmark_name", "block_in_progress_read", "block_in_progress_read_ticks"],
            ...context_init
        }
    });


    doc.addPage();
    pdfUtils.h2("Write block in progress", doc, outline);
    let error_store = utils.print_page(data.filter((d) => !!d.block_in_progress_store), {
        fx: (d) => d.block_in_progress_store,
        fy: (d) => d.block_in_progress_store_ticks,
        model: MODEL_STORE,
        labels: {
            title: "Tick model for storing block in progress before reboot",
            x: "size of bip",
            prefix: "[store] "
        },
        context: {
            csv_filepath: OUTPUT_STORE,
            csv_columns: ["benchmark_name", "block_in_progress_store", "block_in_progress_store_ticks"],
            ...context_init
        }
    });


    return error_read + error_store;
}
