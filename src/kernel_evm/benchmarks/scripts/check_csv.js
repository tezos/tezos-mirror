// SPDX-FileCopyrightText: 2023 Marigold <contact@marigold.dev>
//
// SPDX-License-Identifier: MIT

// usage: node check_csv.js data.csv

let fs = require('fs')
let analysis = require("./analysis/analysis")
let graphs = require("./analysis/graph")
let { parse } = require('csv-parse')

var args = process.argv.slice(2)

if (args.length == 0) {
    console.error("Usage: node check_csv.js my_file.csv")
    process.exit(1)
}



const filename = args[0]
const cast_value = function (value, context) {
    if (context.header) return value
    return parseInt(value)
}
const processFile = async () => {
    const acc = analysis.init_analysis()
    const graph_acc = graphs.init_graphs()
    const parser = fs
        .createReadStream(`${__dirname}/${filename}`)
        .pipe(parse({
            // CSV options if any
            delimiter: ",",
            columns: true,
            cast: cast_value
        }))
    for await (const record of parser) {
        // Work with each record
        analysis.process_record(record, acc)
        graphs.process_record(record, graph_acc)
    }
    return { analysis: acc, graphs: graph_acc }
}

(async () => {
    const infos = await processFile()
    let exit_status = analysis.check_result(infos.analysis)
    await graphs.draw_tick_per_gas(infos.graphs)
    process.exit(exit_status)
})()