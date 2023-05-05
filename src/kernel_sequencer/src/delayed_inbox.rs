// SPDX-FileCopyrightText: 2023 Marigold <contact@marigold.dev>
//
// SPDX-License-Identifier: MIT

use tezos_smart_rollup_host::{
    input::Message,
    runtime::{Runtime, RuntimeError},
};

/// Return a message from the inbox
///
/// This function drives the delayed inbox:
///  - add messages to the delayed inbox
///  - process messages from the sequencer
///  - returns message as "normal" message to the user kernel
pub fn read_input<Host: Runtime>(host: &mut Host) -> Result<Option<Message>, RuntimeError> {
    host.read_input()
}

#[cfg(test)]
mod tests {
    use tezos_smart_rollup_mock::MockHost;

    use super::read_input;

    #[test]
    fn test_read_input() {
        let mut mock_host = MockHost::default();
        mock_host.add_external(16);
        mock_host.add_external(16);
        mock_host.add_external(16);

        let _ = read_input(&mut mock_host).unwrap().unwrap();
        let _ = read_input(&mut mock_host).unwrap().unwrap();
        let _ = read_input(&mut mock_host).unwrap().unwrap();

        let res = read_input(&mut mock_host).unwrap();

        assert!(res.is_none());
    }
}
