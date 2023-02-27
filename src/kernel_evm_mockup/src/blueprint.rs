// SPDX-FileCopyrightText: 2022-2023 TriliTech <contact@trili.tech>
// SPDX-FileCopyrightText: 2023 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

#![allow(dead_code)]

use crate::inbox::Messages;

pub struct Queue {
    // In our case, to make it simple and straightforward it will be
    // an array of pendings transactions even though it'll be only a
    // singleton for our needs.
    pub proposal: Vec<Messages>,
}

impl Queue {
    pub fn add(mut queue: Queue, messages: Messages) {
        queue.proposal.push(messages)
    }
}
