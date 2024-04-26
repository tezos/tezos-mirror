// SPDX-FileCopyrightText: 2024 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

use comfy_table::{Attribute, Cell};
use numfmt::{Formatter, Numeric};

/// Style the cell with [`Attribute::Bold`] & [`Color::White`]
pub fn highlight(cell: Cell) -> Cell {
    cell.add_attribute(Attribute::Bold)
}

/// Produce a string by formatting the number with the given `separator` and `precision`
pub fn thousand_format<N: Numeric>(content: N, num_decimals: u8) -> String {
    let mut fmt = Formatter::new()
        .separator(',')
        .unwrap()
        .precision(numfmt::Precision::Decimals(num_decimals));
    fmt.fmt2(content).to_string()
}
