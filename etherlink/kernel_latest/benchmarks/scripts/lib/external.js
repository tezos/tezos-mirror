// SPDX-FileCopyrightText: 2023 Marigold <contact@marigold.dev>
//
// SPDX-License-Identifier: MIT

// utils to use alternative path to resources

const path = require('node:path');

/**
 * Is the script execution in production (as indicated by $NODE_ENV)
 * @returns boolean
 */
const in_prod = function () {
    return process.env.NODE_ENV == "production"
}

/**
 * When in production, binaries are searched in the $PATH,
 * else in TEZOS_DIR,
 * else the argument is used.
 * @param filepath
 * @returns
 */
const bin = function (filepath) {

    if (in_prod()) {
        return path.basename(filepath)
    } else if (process.env.TEZOS_DIR) {
        return path.resolve(process.env.TEZOS_DIR, filepath)
    } else {
        return path.resolve(filepath)
    }
}

/**
 * When in production, the ressources are searched for in directory
 * configurable using the env variable $EXTERNAL_RESSOURCES
 * else in TEZOS_DIR,
 * else the argument is used.
 * @param filepath
 * @returns
 */
const resource = function (filepath) {
    if (in_prod() && process.env.EXTERNAL_RESSOURCES) {
        return path.format({
            dir: process.env.EXTERNAL_RESSOURCES,
            base: path.basename(filepath)
        })
    } else if (process.env.TEZOS_DIR) {
        return path.format({
            dir: process.env.TEZOS_DIR,
            base: path.basename(filepath)
        })
    } else {
        return filepath
    }
}

/**
 * returns a directory name depending on execution environnement.
 * In production: returns the environnement variable $EXTERNAL_RESSOURCES,
 * else: returns filepath.
 * @param filepath
 * @returns
 */
const ressource_dir = function (filepath) {
    if (in_prod() && process.env.EXTERNAL_RESSOURCES) {
        return process.env.EXTERNAL_RESSOURCES
    } else {
        return filepath
    }

}

/**
 * Returns an output directory configurable using env variable $OUTPUT
 * @returns
 */
const output = function () {
    if (process.env.OUTPUT) {
        return process.env.OUTPUT
    } else {
        return "."
    }

}

module.exports = { bin, resource, ressource_dir, output }
