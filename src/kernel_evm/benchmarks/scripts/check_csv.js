// SPDX-FileCopyrightText: 2023 Marigold <contact@marigold.dev>
//
// SPDX-License-Identifier: MIT

// usage: node check_csv.js data.csv

let fs = require('fs')
let { init_analysis, check_result, processRecord } = require("./lib/analysis")
let { parse } = require('csv-parse')
const internal = require('stream')
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
    const acc = init_analysis()
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
        processRecord(record, acc)
    }
    return acc
}

(async () => {
    const infos = await processFile()
    let exit_status = check_result(infos)
    process.exit(exit_status)
})()