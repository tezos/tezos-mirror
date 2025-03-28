// SPDX-FileCopyrightText: 2023 Marigold <contact@marigold.dev>
//
// SPDX-License-Identifier: MIT

// A simple timestamp utils, usable in filenames.

function timestamp() {
    const dateObject = new Date();
    // YYYY-MM-DDTHH-MM-SS.SSSZ
    return dateObject.toISOString().replace(/:/g, "-")
}


module.exports = { timestamp }