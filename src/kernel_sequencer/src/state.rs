// SPDX-FileCopyrightText: 2023 Marigold <contact@marigold.dev>
//
// SPDX-License-Identifier: MIT

use tezos_data_encoding::{enc::BinWriter, nom::NomReader};
use tezos_smart_rollup_encoding::public_key::PublicKey;
use tezos_smart_rollup_host::runtime::{Runtime, RuntimeError};

use crate::{delayed_inbox::UserMessage, queue::Queue, storage::write_state};

/// Represent the state of the delayed inbox
///
/// The delayed inbox has 2 states:
///  - Sequenced(SmartRollupAddress):
///     the delayed inbox accepts messages from the registered sequencer
///  - Fallback:
///     the kernel will process by itself the messages from the delayed inbox,
///     it's also the default mode
#[derive(Debug, PartialEq, Eq, NomReader, BinWriter, Clone)]
pub enum State {
    Sequenced(PublicKey),
    Fallback,
}

impl Default for State {
    fn default() -> Self {
        State::Fallback
    }
}

/// If the head of the inbox is too old, the state switch to the Fallback mode.
pub fn update_state<H: Runtime>(
    host: &mut H,
    delayed_inbox_queue: &Queue,
    current_level: u32,
) -> Result<(), RuntimeError> {
    let head = delayed_inbox_queue.head(host)?;

    match head {
        None => Ok(()),
        Some(UserMessage { timeout_level, .. }) => {
            if timeout_level < current_level {
                write_state(host, State::Fallback)?;
                Ok(())
            } else {
                Ok(())
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use tezos_smart_rollup_encoding::public_key::PublicKey;
    use tezos_smart_rollup_host::path::RefPath;
    use tezos_smart_rollup_mock::MockHost;

    use crate::{
        delayed_inbox::UserMessage,
        queue::Queue,
        state::State,
        storage::{read_state, write_state},
    };

    use super::update_state;

    fn sequenced_state() -> State {
        State::Sequenced(
            PublicKey::from_b58check("edpkuDMUm7Y53wp4gxeLBXuiAhXZrLn8XB1R83ksvvesH8Lp8bmCfK")
                .expect("decoding should work"),
        )
    }

    #[test]
    fn test_switch_to_fallback() {
        let mut mock_host = MockHost::default();
        let mut delayed_inbox_queue = Queue::new(&mock_host, &RefPath::assert_from(b"/queue"))
            .expect("queue should be created");
        let message = UserMessage {
            timeout_level: 32,
            payload: Vec::default(),
        };
        write_state(&mut mock_host, sequenced_state()).expect("Writing a new state should work");

        delayed_inbox_queue
            .add(&mut mock_host, &message)
            .expect("Element should be added to the queue");

        update_state(&mut mock_host, &delayed_inbox_queue, 100)
            .expect("Updating the state should work");

        let state = read_state(&mut mock_host).expect("reading the state should work");

        assert_eq!(state, State::Fallback);
    }

    #[test]
    fn test_same_state() {
        let mut mock_host = MockHost::default();
        let mut delayed_inbox_queue = Queue::new(&mock_host, &RefPath::assert_from(b"/queue"))
            .expect("queue should be created");
        let message = UserMessage {
            timeout_level: 32,
            payload: Vec::default(),
        };

        write_state(&mut mock_host, sequenced_state()).expect("Writing a new state should work");

        delayed_inbox_queue
            .add(&mut mock_host, &message)
            .expect("Element should be added to the queue");

        update_state(&mut mock_host, &delayed_inbox_queue, 16)
            .expect("Updating the state should work");

        let read_state = read_state(&mut mock_host).expect("reading the state should work");

        assert_eq!(read_state, sequenced_state());
    }
}
