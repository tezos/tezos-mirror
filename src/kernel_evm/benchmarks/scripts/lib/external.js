// SPDX-FileCopyrightText: 2023 Marigold <contact@marigold.dev>
//
// SPDX-License-Identifier: MIT

// utils to use alternative path to resources

const path = require('node:path');

const in_prod = function () {
    return process.env.NODE_ENV == "production"
}

// When in production, binaries are searched in the $PATH
const bin = function (filepath) {
    if (in_prod()) {
        return path.basename(filepath)
    } else {
        return filepath
    }
}

// When in production, the ressources are searched for in directory
// configurable using the env variable $EXTERNAL_RESSOURCES
const resource = function (filepath) {
    if (in_prod() && process.env.EXTERNAL_RESSOURCES) {
        return path.format({
            dir: process.env.EXTERNAL_RESSOURCES,
            base: path.basename(filepath)
        })
    } else {
        return filepath
    }
}

// Returns an output directory configurable using env variable $OUTPUT
const output = function () {
    if (process.env.OUTPUT) {
        return process.env.OUTPUT
    } else {
        return "."
    }

}
module.exports = { bin, resource, output }
