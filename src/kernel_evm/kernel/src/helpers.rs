// SPDX-FileCopyrightText: 2023 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

pub fn ensures(cond: bool) -> Option<()> {
    if cond {
        Some(())
    } else {
        None
    }
}
