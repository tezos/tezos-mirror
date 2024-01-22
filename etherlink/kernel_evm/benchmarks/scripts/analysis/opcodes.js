// SPDX-FileCopyrightText: 2023 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

// usage: node opcodes.js <opcodes_dump.json>

const fs = require('fs');
const process = require("process");
const csv = require("csv-stringify/sync");

function sumArray(arr) {
    return arr.reduce((acc, curr) => acc + curr, 0);
}

/// See https://www.evm.codes/?fork=shanghai, these corresponds to `STOP`,
/// `RETURN` and `REVERT`
const no_gas_opcodes = [ 0x00, 0xf3, 0xfd ]

/// Subtlety to handle the case where no gas is accounted. In that case, we
/// suppose that the ticks per gas is constant.
function compute_ticks_per_gas({ gas, ticks }) {
    if (gas == 0) { return ticks } else { return ticks / gas }
}

/// A sample is considered inconsistent if it didn't account for gas while the
/// specification expects it to, or in the contrary if it spent gas while it
/// shouldn't.
function is_inconsistent(opcode, res) {
    return no_gas_opcodes.some((elem) => Number(opcode) == Number(elem)) ? res.gas != 0 : res.gas == 0;
}

function apply_comparison(cmpf, array, defaultValue = undefined) {
    var result = undefined;

    for (const value of array) {
        if (result == undefined) {
            result = value;
        } else {
            result = cmpf(result, value);
        }
    };

    if (result == undefined) { return defaultValue } else { return result }
}

function check_no_gas_expected(opcode) {
    return no_gas_opcodes.some((elem) => Number(opcode) == Number(elem))
}

/// compute_results takes a set of result for a given opcode and computes the
/// meaningful data to analyze and produce the tick model.
///
/// The data are the following (t/g = ticks per gas unit):
/// - `average` (t/g)
/// - `min` (t/g)
/// - `max` (t/g)
/// - `median` (t/g)
/// - `standard deviation` (t/g)
/// - `no gas expected` denotes whether the opcode should account for gas
///   (see `no_gas_opcodes` above)
/// - `cardinal` is the number of time the opcode has been used in the benchmark
/// - `total gas` is total gas spent for the whole benchmark for this opcode
/// - `average_gas`
/// - `total_ticks` is the total number of ticks consumed for this opcode
///
/// - `inconsistencies` denotes the number of samples that didn't account for
///   gas while they should have, or in the contrary if they spent gas while they
///   shouldn't.
function compute_results(opcode, opcode_data) {
    /// Prepare the samples, removing the inconsistencies
    let results = opcode_data
        .filter((x) => !is_inconsistent(opcode, x))
        .map(compute_ticks_per_gas)
    // Quoting @Pilou097: `!!x` it will convert `x` to boolean and then negate
    // the boolean. It's equal to `false` when x is `null`, `undefined` or `NaN`
    // and `empty strings`
        .filter((x) => !!x);
    results.sort((a, b) => a - b);

    if (results.length == 0) { return null };

    /// Extract all the meaningful data
    let sum = sumArray(results);
    let average = sum / results.length;
    let min = apply_comparison(Math.min, results);
    let max = apply_comparison(Math.max, results);
    let varianceArray = results.map(ticks => (ticks - average) * (ticks - average));
    let variance = sumArray(varianceArray) / varianceArray.length
    let standard_deviation = Math.sqrt(variance);
    let cardinal = results.length;

    let total_ticks = sumArray(opcode_data.map(({ ticks }) => ticks));
    let total_gas = sumArray(opcode_data.map(({ gas }) => gas));
    let average_gas = total_gas / results.length;

    let mid = Math.floor(results.length / 2);
    let median = results.length % 2 != 0 ? results[mid] : (results[mid - 1] + results[mid]) / 2;
    let no_gas_expected = check_no_gas_expected(opcode);
    let inconsistencies = opcode_data.reduce((count, value) => {
        if (is_inconsistent(opcode, value)) { return count + 1; } else { return count; }
    }, 0);

    return {
        average,
        min,
        max,
        median,
        standard_deviation,
        no_gas_expected,
        cardinal,
        total_gas,
        average_gas,
        total_ticks,
        inconsistencies,
        opcode
    };

}

function compute_opcodes_analysis_results(result) {
    let analysis = [];
    for (const opcode in result) {
        let results = compute_results(opcode, result[opcode]);
        if (results !== null) {
            analysis[opcode] = results;
        }
    }
    return analysis;
}

function merge_all_benchmark_opcodes(results) {
    let opcodes = {};

    for (const [_benchmark_name, benchmark_opcodes] of Object.entries(results)) {
        for (const opcode in benchmark_opcodes) {
            if (opcodes[opcode] == undefined) {
                opcodes[opcode] = benchmark_opcodes[opcode]
            } else {
                opcodes[opcode] = opcodes[opcode].concat(benchmark_opcodes[opcode])
            }
        }
    }
    return opcodes;
}

function opcode_to_string(n) {
    return '0x' + (n < 16 ? "0" : "") + n.toString(16);
}

function produce_opcodes_csv(result, file) {
    let columns = {
        opcode: "opcode",
        no_gas_expected: "no gas expected",
        cardinal: "cardinal",
        average: "average t/g",
        median: "median t/g",
        min: "min t/g",
        max: "max t/g",
        standard_deviation: "standard_deviation",
        total_gas: "total_gas",
        average_gas: "average_gas",
        total_ticks: "total_ticks",
        inconsistencies: "inconsistent gas accounting"
    };
    let rows = [];
    for (const line in result) {
        let no_gas_expected = result[line].no_gas_expected ? 'true' : '';
        rows.push({
            ...result[line],
            no_gas_expected,
        });
    };
    fs.writeFileSync(
        file,
        csv.stringify(rows, { header: true, columns }),
        { flag: "w" }
    )
}

/// This part of the script is dedicated to produce the template for the Rust
/// implementation of the tick model.


const header = `// SPDX-FileCopyrightText: 2023 Marigold <contact@marigold.dev>
// SPDX-FileCopyrightText: 2023 Nomadic Labs <contact@nomadic-labs.com>
// SPDX-FileCopyrightText: 2022-2023 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

//! Ticks per gas per Opcode model for the EVM Kernel

// The values from this file have been autogenerated by a benchmark script. If
// it needs to be updated, please have a look at the script
// \`etherlink/kernel_evm/benchmarks/scripts/analysis/opcodes.js\`.

use evm::Opcode;
`

const default_constant_name = "DEFAULT_TICKS_PER_GAS"

function default_ticks_per_gas(default_value) {
    return `
// Default ticks per gas value
const ${default_constant_name}: u64 = ${default_value};
`;
}

function constant_name(opcode) {
    return "MODEL_" + String.prototype.toUpperCase.call(opcode);
}

/// Translates to `const MODEL_<OPCODE>: u64 = median + deviation
function create_constant(opcode, average, deviation) {
    let model = Math.ceil(average) + 2 * Math.ceil(deviation);
    let comment = `// Average: ${Math.ceil(average)}; Standard deviation: ${Math.ceil(deviation)}`;
    let constant = `const ${constant_name(opcode)}: u64 = ${model};`;
    return comment + "\n" + constant + "\n"
}

function create_constant_without_data(opcode) {
    let comment = "// No data";
    let constant = `const ${constant_name(opcode)}: u64 = ${default_constant_name};`;
    return comment + "\n" + constant + "\n"

}

/// Translates to `Opcode::<Opcode> => <OPCODE_CONSTANT> * gas`
function match_branch(opcode, no_gas) {
    let branch = opcode + " => " + constant_name(opcode);
    if (no_gas) {
        return branch + ", // constant, no gas accounted";
    } else {
        return branch + " * gas,";
    }
}

/// Generate the big pattern matching.
function opcode_fn(branches) {
    let branches2 = branches.join("\n");
    let fn = `pub fn ticks(opcode: &Opcode, gas: u64) -> u64 {
      match opcode.as_u8() {
        ${branches2}
        _ => ${default_constant_name} * gas,
        }
      }`;
    return fn;

}

/// Generates the template, which is:
/// - header
/// - default ticks per gas for unknown opcodes
/// - constants (or function) for the ticks per gas
/// - pattern matching on the opcode, that can be used directly into the ticks accounting of the kernel
function create_model_template(results, opcodes_complete_list, file) {
    let constants = [default_ticks_per_gas(2000)];
    let branches = [];

    for (const opcode of opcodes_complete_list) {
        if (results[opcode] == undefined) {
            constants.push(create_constant_without_data(opcode));
            branches.push(match_branch(opcode, check_no_gas_expected(opcode)));
        } else {
            let { median, standard_deviation, no_gas_expected } = results[opcode];

            constants.push(create_constant(opcode, median, standard_deviation));
            branches.push(match_branch(opcode, no_gas_expected));
        }
    };
    fs.writeFileSync(file, header, { flag : "w" });
    fs.appendFileSync(file, constants.join("\n") + "\n");
    fs.appendFileSync(file, opcode_fn(branches) + "\n");
}

/// Return [start, start + 1, ..., end - 1, end]
function range(start, end) {
    if (end < start) {
        return []
    } else {
        return [...Array(end - start + 1).keys()].map((k) => k + start)
    }
}

/// See https://ethereum.org/fr/developers/docs/evm/opcodes/
const invalid_opcodes = [
    ...range(0x0c, 0x0f),
    ...range(0x1e, 0x1f),
    ...range(0x21, 0x2f),
    ...range(0x49, 0x4f),
    ...range(0x5c, 0x5e),
    ...range(0xa5, 0xef),
    ...range(0xf6, 0xf9),
    ...range(0xfb, 0xfc),
]

/// Generates the list of all opcodes of EVM.
function build_opcodes_list() {
    let opcodes = [];

    // Opcodes are represented by a single byte, hence the list can be built by
    // ranging between 0x00 and 0xff.
    for (let candidate = 0x00; candidate <= 0xff; candidate++) {
        // Some values are not opcodes (see
        // https://ethereum.org/fr/developers/docs/evm/opcodes/)
        if (!invalid_opcodes.includes(candidate)) { opcodes.push(opcode_to_string(candidate)) }
    }

    return opcodes
}

function build_missing_opcodes(opcodes, opcodes_complete_list) {
    let missing_opcodes = [];

    for (const candidate of opcodes_complete_list) {
        let found = false;
        for (const model in opcodes) {
            if (Number(model) == Number(candidate)) {
                found = true;
            }
        }
        if (!found) {
            missing_opcodes.push(candidate)
        }
    }

    return missing_opcodes
}

/// Utility to warn about the opcodes that haven't be covered during the
/// benchmarks.
function warn_for_missing_opcodes(opcodes, opcodes_complete_list) {
    let missing_opcodes = build_missing_opcodes(opcodes, opcodes_complete_list);
    console.log(`These opcodes are missing a model: ${missing_opcodes.map((op) => `${op}`)}.`)
}

function extract_timestamp(filename) {
    const filename_regex = /(\d{4}-\d{2}-\d{2}T\d{2}-\d{2}-\d{2}.\d+Z)/g;
    for (const match of filename.matchAll(filename_regex)) {
        return match[0];
    }

}

function analysis_filename(input) {
    let timestamp = extract_timestamp(input);
    return `opcodes_analysis_${timestamp}.csv`
}

function model_rs_filename(input) {
    let timestamp = extract_timestamp(input);
    return `tick_model_${timestamp}.rs`
}

/// Entrypoint on the script
function generate_model() {
    let filename = undefined;
    if (process.argv.length < 3) {
        console.log("Script expects an argument");
        exit(2);
    } else {
        filename = process.argv[2];
    }

    let analysis_output = analysis_filename(filename);
    let model_rs_output = model_rs_filename(filename);
    let output = fs.readFileSync(filename);
    let all_bench_opcodes = JSON.parse(output);
    let opcodes = merge_all_benchmark_opcodes(all_bench_opcodes);
    let analysis = compute_opcodes_analysis_results(opcodes);

    console.log(`The opcodes analysis is in ${analysis_output}`);
    produce_opcodes_csv(analysis, analysis_output);
    var opcodes_complete_list = build_opcodes_list();
    console.log(`The template for the tick model is in ${model_rs_output}`);
    create_model_template(analysis, opcodes_complete_list, model_rs_output);
    warn_for_missing_opcodes(analysis, opcodes_complete_list);
}

generate_model()
