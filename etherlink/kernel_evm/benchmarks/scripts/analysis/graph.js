// SPDX-FileCopyrightText: 2023 Marigold <contact@marigold.dev>
//
// SPDX-License-Identifier: MIT


const { ChartJSNodeCanvas } = require('chartjs-node-canvas');
const { is_transfer, is_create, is_transaction, BASE_GAS } = require('./utils')
const fs = require('fs');
const RUN_TRANSACTION_OVERHEAD = 560_000
const OUTPUT = 'tick_per_gas.png'

module.exports = { process_record, draw_tick_per_gas, init_graphs }

function init_graphs() {
    let empty = {
        dataset: { create: [], call: [] }
    };
    return empty
}

function process_record(record, acc) {
    if (is_transaction(record)) process_transaction_record(record, acc)

}

function process_transaction_record(record, acc) {
    if (is_transfer(record)) process_transfer(record, acc)
    else if (is_create(record)) process_create(record, acc)
    else process_call(record, acc)
}

function process_transfer(record, acc) {
    // not relevant for tick per gas graph
}

function process_create(record, acc) {
    add_data(record, acc.dataset.create)

}

function process_call(record, acc) {
    add_data(record, acc.dataset.call)
}

function add_data(record, dataset) {
    dataset.push({ x: record.run_transaction_ticks, y: record.gas_cost, r: radius(record) })
}

async function draw_tick_per_gas(infos) {
    const width = 1000; //px
    const height = 1000; //px
    const backgroundColour = 'white'; // Uses https://www.w3schools.com/tags/canvas_fillstyle.asp
    const chartJSNodeCanvas = new ChartJSNodeCanvas({ width, height, backgroundColour });
    const configuration = {
        type: 'bubble',
        data: {
            datasets: [{
                label: 'Create',
                data: infos.dataset.create,
                borderColor: 'Blue',
                // backgroundColor: 'Blue'
            }
                , {
                label: 'call',
                data: infos.dataset.call,
                borderColor: 'Green',
                // backgroundColor: 'Green'
            }
            ]
        },
        options: {
        }
    }
    const buffer = await chartJSNodeCanvas.renderToBuffer(configuration);
    fs.writeFileSync(OUTPUT, buffer, 'base64');
    console.info(`Output graph ${OUTPUT}`)
}

function radius(record) {
    if (record.gas_cost > BASE_GAS) {
        let gas = record.gas_cost - BASE_GAS
        let ticks = record.run_transaction_ticks - RUN_TRANSACTION_OVERHEAD
        let tpg = ticks / gas / 100
        return tpg.toFixed() //px
    } else return 20 //px
}

function add_data(record, dataset) {
    dataset.push({ x: record.run_transaction_ticks, y: record.gas_cost, r: radius(record) })
}