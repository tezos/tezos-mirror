// SPDX-FileCopyrightText: 2023 Marigold <contact@marigold.dev>
//
// SPDX-License-Identifier: MIT

// Requires installation of https://github.com/brendangregg/FlameGraph
const path = require('node:path')
const fs = require('fs');
const { mkdirSync } = require('node:fs')
const { execSync } = require('child_process')
const { spawn } = require('child_process')
const { timestamp } = require("./lib/timestamp")
const { build_inbox } = require("./lib/inbox")

const external = require("./lib/external")

const commander = require('commander');
const { FixedNumber } = require('ethers');
const { exec } = require('node:child_process');

commander
    .version('1.0.0', '-v, --version')
    .usage('[OPTIONS] file')
    .option('-b, --bench', 'Build inbox from [file].')
    .option('-d, --directory <path>', 'Output directory', path.resolve(process.cwd(), './flamechart'))
    .option('-t, --tezos-dir <path>', 'tezos repo directory')
    .option('-i, --inbox-directory <path>', 'Directory to store the inbox if building from bench script', '/tmp')
    .option('-k, --skip <n>', 'Number of levels to process before profiling', '0')
    .option('--flamegraph-bin <path>', 'Flamegraph utility', 'flamegraph.pl')
    .option('--keep-temp', "Keep temporary files", false)
    .option("--debugger <path>", "debugger binary")
    .option('--kernel-dir <path>', 'directory containing kernel and preimage dir', ".")
    .addHelpText('after', '\nRequires installation of https://github.com/brendangregg/FlameGraph')
    .parse(process.argv);

if (commander.args.length < 1) {
    commander.help()
}

if (commander.opts().tezosDir) {
    process.env['TEZOS_DIR'] = commander.opts().tezosDir // setup env variable, used by subprocess
}

const RUN_DEBUGGER_COMMAND = commander.opts().debugger ? commander.opts().debugger : external.bin('./octez-smart-rollup-wasm-debugger')
console.log(`Debugger: ${RUN_DEBUGGER_COMMAND}`)

const KERNEL_DIR = commander.opts().kernelDir
const EVM_INSTALLER_KERNEL_PATH = external.resource(path.format({ dir: KERNEL_DIR, name: 'evm_unstripped_installer.wasm' }))
const PREIMAGE_DIR = external.resource(path.format({ dir: KERNEL_DIR, name: '_evm_unstripped_installer_preimages' }))

const INBOX_DIR = path.resolve(commander.opts().inboxDirectory)
const BUILD_INBOX = commander.opts().bench
let inbox = ""
let filename = commander.args[0]
if (BUILD_INBOX) {
    console.log(`Building inbox from script: ${filename}`)
    inbox = build_inbox(INBOX_DIR, filename)
} else {
    inbox = filename
}
console.log(`Inbox: ${inbox}`)

const FLAMECHART_DIR = path.resolve(commander.opts().directory)
const FLAMEGRAPH_BIN = commander.opts().flamegraphBin
mkdirSync(FLAMECHART_DIR, { recursive: true })

const LOG_FILE = path.format({ dir: FLAMECHART_DIR, name: `${path.basename(inbox, '.json')}_${timestamp()}`, ext: ".log" })
const FLAMECHART_FILE = path.format({ dir: FLAMECHART_DIR, name: `${path.basename(inbox, ".json")}_${timestamp()}`, ext: ".svg" })

const SKIPS = commander.opts().skip;

const KEEP_TEMP = commander.opts().keepTemp;

function run_profiler(file) {

    profiler_result = new Promise((resolve, _) => {
        fs.writeFileSync(LOG_FILE, `Output of debugger for bench ${path.basename(commander.args[0])}\n`);

        let profiler_output_path = "";

        const args = ["--kernel", EVM_INSTALLER_KERNEL_PATH, "--inputs", file, "--preimage-dir", PREIMAGE_DIR]
        const childProcess = spawn(RUN_DEBUGGER_COMMAND, args)

        childProcess.stdin.write("load inputs\n") // step over upgrade
        childProcess.stdin.write("step kernel_run\n")
        if (SKIPS > 0) console.log(`Skipping profiling of ${SKIPS} level`)
        for (let index = 0; index < SKIPS; index++) childProcess.stdin.write("step inbox\n")
        childProcess.stdin.write("profile\n")
        childProcess.stdin.end()

        childProcess.stdout.on('data', (data) => {
            // note that debugger output (proper) and kernel output can be interlived apparently ?
            const output = data.toString()
            const profiler_output_path_regex = /Profiling result can be found in (.+)/
            const profiler_output_path_match = output.match(profiler_output_path_regex)
            const profiler_output_path_result = profiler_output_path_match
                ? profiler_output_path_match[1]
                : null
            if (profiler_output_path_result !== null) {
                profiler_output_path = profiler_output_path_result
                console.log(`Profiler output: ${profiler_output_path}`)
            }
            // log output
            fs.appendFileSync(LOG_FILE, output)

        });

        // capturing stderr for good measure
        childProcess.stderr.on('data', (data) => {
            // log output
            fs.appendFileSync(LOG_FILE, data.toString())
        })

        childProcess.on('close', _ => {
            if (profiler_output_path == "") {
                console.log(new Error("Profiler output path not found"))
            }
            console.log(`Profiling done.`)
            resolve({ profiler_output_path: profiler_output_path })
        });
    })
    return profiler_result;
}

async function flamechart(inbox) {
    console.log(`Logs in ${LOG_FILE}`)
    let profiler_result = await run_profiler(inbox)
    console.log(`Building flamechart ${FLAMECHART_FILE}`)
    execSync(`${FLAMEGRAPH_BIN} --flamechart ${profiler_result.profiler_output_path} > ${FLAMECHART_FILE}`)

    // cover our tracks
    if (BUILD_INBOX && !KEEP_TEMP) {
        console.log(`Deleting inbox ${inbox}`)
        fs.rmSync(inbox, { recursive: true, force: true })
    }
    if (!KEEP_TEMP) {
        console.log(`Deleting profiling ${profiler_result.profiler_output_path}`)
        fs.rmSync(
            profiler_result.profiler_output_path, { recursive: true, force: true })
    }
}

flamechart(inbox)