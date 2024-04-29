// SPDX-FileCopyrightText: 2023 Marigold <contact@marigold.dev>
//
// SPDX-License-Identifier: MIT

const utils = require("./utils")
const pdfUtils = require("./pdf_utils")
const OUTPUT = 'register_tx_data.csv'
const MODEL_OBJ = { intercept: 200000, coef: 880 }
const MODEL_RECEIPT = { intercept: 200000, coef: 960 }
const MODEL_LOGBLOOM = { intercept: 5300, coef: 85000 }
module.exports = { print_analysis }


function print_analysis(infos, dir = "analysis_result", doc) {
    let context_init = {
        doc,
        dir
    }
    let data = infos.tx_register
    let outline = pdfUtils.h1("Registering transaction", doc);

    // transaction object
    pdfUtils.h2("Store transaction object", doc, outline);
    let errors_obj = utils.print_page(data, {
        fx: (d) => d.tx_size,
        fy: (d) => d.store_transaction_object_ticks,
        model: MODEL_OBJ,
        labels: {
            title: "Tick model for storing transaction object",
            x: "size of tx",
            prefix: "[object] "
        },
        context: {
            csv_filepath: `object_${OUTPUT}`,
            csv_columns: ["benchmark_name", "tx_size", "store_transaction_object_ticks",],
            ...context_init
        }
    });

    // receipt
    doc.addPage()
    pdfUtils.h2("Store receipt", doc, outline);
    let errors_receipt = utils.print_page(data, {
        fx: (d) => d.receipt_size,
        fy: (d) => d.store_receipt_ticks,
        model: MODEL_RECEIPT,
        labels: {
            title: "Tick model for storing transaction receipt",
            x: "size of receipt",
            prefix: "[receipt] "
        },
        context: {
            csv_filepath: `receipt_${OUTPUT}`,
            csv_columns: ["benchmark_name", "receipt_size", "store_receipt_ticks"],
            ...context_init
        }
    });

    // bloom
    doc.addPage()
    pdfUtils.h2("Log bloom", doc, outline);
    let errors_bloom = utils.print_page(data, {
        fx: (d) => d.bloom_size,
        fy: (d) => d.logs_to_bloom,
        model: MODEL_LOGBLOOM,
        labels: {
            title: "Tick model for log bloom",
            x: "size of bloom",
            prefix: "[bloom] "
        },
        context: {
            csv_filepath: `bloom_${OUTPUT}`,
            csv_columns: ["benchmark_name", "bloom_size", "logs_to_bloom"],
            ...context_init
        }
    });

    return errors_obj + errors_receipt + errors_bloom;
}
