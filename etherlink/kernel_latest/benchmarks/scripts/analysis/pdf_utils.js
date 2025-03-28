// SPDX-FileCopyrightText: 2024 Marigold <contact@marigold.dev>
//
// SPDX-License-Identifier: MIT
const tmp = require("tmp");
const { ChartJSNodeCanvas } = require('chartjs-node-canvas');
const fs = require('fs')

module.exports = { draw, output_msg, h1, h2 }

const default_font_size = 10;
function h1(str, doc){
    doc.fontSize(25).text(str).fontSize(default_font_size);
    return doc.outline.addItem(str);
}


function h2(str, doc, outline){
    doc.fontSize(20).text(str).fontSize(default_font_size);
    return outline.addItem(str);
}
const number_formatter_compact = Intl.NumberFormat('en', { notation: 'compact', compactDisplay: 'short' });
function draw(doc, labels, data, model, regression) {

    const width = 600; //px
    const height = 600; //px
    const backgroundColour = 'white'; // Uses https://www.w3schools.com/tags/canvas_fillstyle.asp
    const chartJSNodeCanvas = new ChartJSNodeCanvas({ type: 'png', width, height, backgroundColour });
    let max = data.reduce((acc, d) => Math.max(acc, d.x), 0);
    let dataset_model = [
        { x: 0, y: model.intercept }, { x: max, y: model.intercept + max * model.coef }
    ]
    let dataset_regression = !regression ? [] : [
        { x: 0, y: regression.weights[1][0] }, { x: max, y: regression.weights[1][0] + max * regression.weights[0][0] }
    ]
    const configuration = {
        data: {
            datasets: [{
                type: 'scatter',
                label: labels.y ?? `benchmarks`,
                data: data,
                backgroundColor: 'rgba(255, 99, 132, 1)', // Color of the points
                borderWidth: 1,
                xAxisID: 'x',
                yAxisID: 'y',
            }, {
                type: 'line',
                label: 'model',
                data: dataset_model,
                backgroundColor: 'rgb(0, 0, 255)',
                borderColor: 'rgb(0, 0, 255)',
                xAxisID: 'x', // Specify to which axes to link
                yAxisID: 'y'
            }, {
                type: 'line',
                label: 'regression',
                data: dataset_regression,
                backgroundColor: 'rgb(0, 255, 0)',
                borderColor: 'rgb(0, 255, 0)',
                xAxisID: 'x', // Specify to which axes to link
                yAxisID: 'y'
            }],
        },
        options: {
            plugins: {
                title: {
                    display: !!labels.title,
                    text: labels.title
                },
                tickFormat: {
                    notation: 'compact'
                }
            },
            scales: {
                x: {
                    type: 'linear',
                    position: 'bottom',
                    title: {
                        display: !!labels.x,
                        text: labels.x ?? ""
                    }
                },
                y: {
                    type: 'linear',
                    position: 'left',
                    beginAtZero: true,
                    title: {
                        display: true,
                        text: labels.y ?? "ticks"
                    },
                    ticks: {
                        callback: (val) => {
                            if (!val) return 0;
                            return number_formatter_compact.format(val);
                        }
                    }
                }
            }
        }
    };

    const buffer = chartJSNodeCanvas.renderToBufferSync(configuration);
    const tmp_img = tmp.fileSync({ postfix: ".png" });
    fs.writeFileSync(tmp_img.name, buffer);
    doc.image(tmp_img.name, { width: 400 });
    fs.unlinkSync(tmp_img.name);
}

function output_msg(str, doc = null){
    console.log(str);
    if(doc) doc.text(str);
}
