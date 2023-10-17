// SPDX-FileCopyrightText: 2023 Marigold <contact@marigold.dev>
//
// SPDX-License-Identifier: MIT

// Requires installation of https://github.com/brendangregg/FlameGraph
const path = require('node:path')
const { mkdirSync } = require('node:fs')
const { execSync } = require('child_process')
const { spawn } = require('child_process')
const { timestamp } = require("./lib/timestamp")
const { build_inbox } = require("./lib/inbox")

const external = require("./lib/external")

const commander = require('commander');

commander
    .version('1.0.0', '-v, --version')
    .usage('[OPTIONS] file')
    .option('-b, --bench', 'Build inbox from [file].')
    .addHelpText('after', '\nRequires installation of https://github.com/brendangregg/FlameGraph')
    .parse(process.argv);

const RUN_DEBUGGER_COMMAND = external.bin('./octez-smart-rollup-wasm-debugger')
const EVM_INSTALLER_KERNEL_PATH = external.resource('evm_unstripped_installer.wasm')
const PREIMAGE_DIR = external.ressource_dir('_evm_unstripped_installer_preimages')

function flamechart_name(flamecharts, inbox) {
    let name = path.basename(inbox, ".json")
    return path.format({ dir: flamecharts, name: `${name}_${timestamp()}`, ext: ".svg" })

}

const FLAMECHART_DIR = path.join(process.cwd(), "flamechart")
mkdirSync(FLAMECHART_DIR, { recursive: true })

const INBOX_DIR = path.join(process.cwd(), "inbox_test")

function run_profiler(path) {

    profiler_result = new Promise((resolve, _) => {

        let profiler_output_path = "";

        const args = ["--kernel", EVM_INSTALLER_KERNEL_PATH, "--inputs", path, "--preimage-dir", PREIMAGE_DIR]

        const childProcess = spawn(RUN_DEBUGGER_COMMAND, args, {})

        childProcess.stdin.write("load inputs\n")

        childProcess.stdin.write("step kernel_run\n")

        childProcess.stdin.write("profile\n")

        childProcess.stdin.end()

        childProcess.stdout.on('data', (data) => {
            const output = data.toString()
            const profiler_output_path_regex = /Profiling result can be found in (.+)/
            const profiler_output_path_match = output.match(profiler_output_path_regex)
            const profiler_output_path_result = profiler_output_path_match
                ? profiler_output_path_match[1]
                : null
            if (profiler_output_path_result !== null) {
                profiler_output_path = profiler_output_path_result
            }


        });
        childProcess.on('close', _ => {
            if (profiler_output_path == "") {
                console.log(new Error("Profiler output path not found"))
            }
            resolve({ profiler_output_path: profiler_output_path })
        });
    })
    return profiler_result;
}

async function flamechart(inbox) {
    let flamechart = flamechart_name(FLAMECHART_DIR, inbox)
    console.log(`flamechart: ${flamechart}`)
    let profiler_result = await run_profiler(inbox)
    execSync(`flamegraph.pl --flamechart ${profiler_result.profiler_output_path} > ${flamechart}`)
}

var inbox = ""
let filename = commander.args[0]
if (commander.opts().bench) {
    console.log(`Building inbox from script ${filename}`)
    inbox = build_inbox(INBOX_DIR, filename)
} else {
    inbox = filename
}
console.log(`inbox: ${inbox}`)
flamechart(inbox)