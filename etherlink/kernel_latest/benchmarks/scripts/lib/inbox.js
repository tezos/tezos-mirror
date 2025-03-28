// SPDX-FileCopyrightText: 2023 Marigold <contact@marigold.dev>
//
// SPDX-License-Identifier: MIT


const path = require('node:path')
const { mkdirSync } = require('node:fs')
const { execSync } = require('child_process')
const { timestamp } = require("./timestamp")


function inbox_name(inbox_dir, benchmark_script) {
    let name = path.basename(benchmark_script, ".js")
    return path.format({ dir: inbox_dir, name: `${name}_${timestamp()}`, ext: ".json" })
}

function build_benchmark_scenario(inbox, benchmark_script) {
    try {
        let inbox_filename = inbox_name(inbox, benchmark_script)
        execSync(`node ${benchmark_script} > ${inbox_filename}`,
            { env: { ...process.env } })
        return inbox_filename
    } catch (error) {
        console.error(`Error running script ${benchmark_script}. Please fixed the error in the script before running this benchmark script`)
        console.error(error);
    }
}


function build_inbox(inbox_dir, script) {
    mkdirSync(inbox_dir, { recursive: true })
    return build_benchmark_scenario(inbox_dir, script)
}

module.exports = { build_inbox }