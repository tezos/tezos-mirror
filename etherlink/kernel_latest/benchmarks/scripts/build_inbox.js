// SPDX-FileCopyrightText: 2023 Marigold <contact@marigold.dev>
//
// SPDX-License-Identifier: MIT

const path = require('node:path')
const { build_inbox } = require("./lib/inbox")
let benchmark_scripts = require("./benchmarks_list.json")

const INBOX_DIR = path.join(process.cwd(), "inbox_test")

if (process.argv.length == 3) {
    console.log(build_inbox(INBOX_DIR, process.argv[2]))

} else {
    benchmark_scripts.map((x) => build_inbox(INBOX_DIR, path.join(__dirname, x)))
}


