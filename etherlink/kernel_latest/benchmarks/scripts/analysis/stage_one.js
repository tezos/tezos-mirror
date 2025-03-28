// SPDX-FileCopyrightText: 2023-2024 Marigold <contact@marigold.dev>
//
// SPDX-License-Identifier: MIT

module.exports = { print_analysis }
const utils = require("./utils")
const pdfUtils = require("./pdf_utils");
const OUTPUT = 'stage_one.csv'
const MODEL_BP = {
    intercept: 27_000_000,
    coef: 25_000_000
}
const MODEL_DELAYED = {
    intercept: 1_800_000,
    coef: 1_800_000
}

function compile_data_one_bench(bench_data) {

    // we need the amount of ticks it takes to read an empty inbox
    // it's a constant value, and the smallest one
    let empty_stage_one_ticks = Math.min(...(bench_data.run_data))

    // now let's ignore kernel_runs where the inbox is empty
    let inbox_reading_runs = bench_data.run_data.filter((val) => val > empty_stage_one_ticks);

    // the total amount of ticks it took to read the inbox is the sum of the
    // remaing values
    let ticks = inbox_reading_runs.reduce((acc, val) => acc + val, 0)

    return { ticks, ...bench_data }
}

function analyse_blueprints(data, doc) {


    let mlr = utils.make_lr(data, (x) => x.nb_chunks, (y) => y.ticks)
    let dataset = data.map((datum) => { return { x: datum.nb_chunks, y: datum.ticks } })
    pdfUtils.output_msg(`[Blueprint] Linear Regression: ${utils.print_lr(mlr, "nb_chunks")}`, doc)

    let max_for_1 = data.filter((row) => row.nb_chunks === 1).reduce((acc, row) => Math.max(acc, row.ticks), 0)
    let min_for_1 = data.filter((row) => row.nb_chunks === 1).reduce((acc, row) => Math.min(acc, row.ticks), 1000000000)
    pdfUtils.output_msg(`maximum nb of ticks for 1 chunk: ${max_for_1}`)
    pdfUtils.output_msg(`minimum nb of ticks for 1 chunk: ${min_for_1}`)

    let errors = utils.print_summary_errors(data,
        datum => {
            return datum.ticks - utils.predict_linear_model(MODEL_BP, datum.nb_chunks)
        }
        , "[Blueprint]"
        , doc
    );
    pdfUtils.draw(doc,
        { title: "Tick model for blueprint reading", x: "nb of bp chunks" },
        dataset,
        MODEL_BP,
        mlr
    );
    return errors;
}

function analyse_delayed(data, doc) {
    let mlr = utils.make_lr(data, (x) => x.nb_msg, (y) => y.ticks)
    pdfUtils.output_msg(`[Delayed] Linear Regression: ${utils.print_lr(mlr, "nb_msg")}`, doc)

    let errors = utils.print_summary_errors(data,
        datum => {
            return datum.ticks - utils.predict_linear_model(MODEL_DELAYED, datum.nb_msg)
        }
        , "[Delayed]"
        , doc
    );

    let dataset = data.map((datum) => { return { x: datum.nb_msg, y: datum.ticks } })
    pdfUtils.draw(doc,
        { title: "Tick model for reading delayed transactions", x: "nb of L1 messages" },
        dataset,
        MODEL_DELAYED,
        mlr
    );
    return errors;

}

function print_analysis(infos, dir = "analysis_result", doc) {
    //doc.fontSize(25).text("Stage one").fontSize(8);
    //let outline = doc.outline.addItem("Stage one");
    let outline = pdfUtils.h1("Stage one", doc);

    // we need to assemble the data
    let data = infos.stage_one_data.map(compile_data_one_bench);
    // then filter improper data

    utils.print_csv(dir, OUTPUT, data, ["benchmark_name", "nb_chunks", "nb_msg", "delayed_inputs", "ticks"])
    pdfUtils.h2("Blueprints", doc, outline);
    let errors_bp = analyse_blueprints(data.filter(datum => !(isNaN(datum.nb_chunks)) && datum.nb_chunks > 0), doc)
    doc.addPage();
    pdfUtils.h2("Delayed transactions", doc, outline);
    let errors_delayed = analyse_delayed(data.filter(datum => !(isNaN(datum.nb_chunks)) && datum.nb_chunks === 0), doc)

    return errors_bp + errors_delayed
}
