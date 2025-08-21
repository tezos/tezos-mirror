// SPDX-FileCopyrightText: 2023 Marigold <contact@marigold.dev>
//
// SPDX-License-Identifier: MIT

// usage: node check_csv.js data.csv

let fs = require('fs')
let path = require('path')
let analysis = require("./analysis/analysis")
let graphs = require("./analysis/graph")
let sanity = require("./analysis/sanity")
let { parse } = require('csv-parse')



const commander = require('commander');
commander
    .usage('[OPTIONS] <benchmark_data.csv>')
    .option('-o, --output-dir <path>', "Output directory", "analysis_result")
    .parse(process.argv);

let args = commander.args


const OUTPUT_DIRECTORY = commander.opts().outputDir
const cast_value = function(value, context) {
    let n = parseInt(value)
    if (isNaN(n)) return value;
    else return n;
}

function list_files(arg) {
    if (path.extname(arg) === ".csv") {
        return [arg]
    } else {
        return fs.readdirSync(arg)
            .filter((filename) => filename.includes("benchmark_result"))
            .map((filename) => path.join(arg, filename))
    }
}

async function processFile(filename, { analysis_acc, graph_acc, sanity_acc }) {
    const parser = fs
        .createReadStream(`${filename}`)
        .pipe(parse({
            // CSV options if any
            delimiter: ",",
            columns: true,
            cast: cast_value,
            trim: true
        }))
    for await (const record of parser) {
        // Work with each record
        analysis.process_record(record, analysis_acc, sanity_acc)
        graphs.process_record(record, graph_acc)
        sanity.check_record(record, sanity_acc)
    }
    return;
}

(async () => {

    if (args.length == 0) {
        console.error("Usage: node check_csv.js my_file.csv")
        process.exit(1)
    }

    let analysis_acc = analysis.init_analysis();
    let graph_acc = graphs.init_graphs();
    let sanity_acc = sanity.init_sanity();
    let infos = { filename: args[0], report: "benchmark_report.pdf", analysis_acc, graph_acc, sanity_acc };

    let files = list_files(args[0]);
    console.log(`Processing ${files.length} files`)
    console.log(files);

    // process files in ||
    await Promise.all(files.map((filename) => processFile(filename, infos)))

    let exit_status = await analysis.check_result(infos, OUTPUT_DIRECTORY)
    sanity.print_summary(infos.sanity_acc, OUTPUT_DIRECTORY)
    await graphs.draw_tick_per_gas(infos.graph_acc, OUTPUT_DIRECTORY)
    process.exit(exit_status)
})()
