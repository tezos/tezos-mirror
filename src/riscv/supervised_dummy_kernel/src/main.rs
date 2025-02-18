// SPDX-FileCopyrightText: 2025 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

use std::ffi::CStr;

unsafe fn test_write(msg: &CStr) {
    let message_endl = msg.to_bytes();
    let written = libc::write(1, message_endl.as_ptr().cast(), message_endl.len());
    assert_eq!(written as usize, message_endl.len());
}

unsafe fn test_writev<const N: usize>(msgs: [&CStr; N]) {
    let mut total_length = 0;
    let mut iovecs = msgs.map(|msg| {
        let bytes = msg.to_bytes();
        total_length += bytes.len();
        libc::iovec {
            iov_base: bytes.as_ptr().cast_mut().cast(),
            iov_len: bytes.len(),
        }
    });

    loop {
        let written = libc::writev(1, iovecs.as_ptr(), iovecs.len() as i32);
        assert!(written > 0);

        let mut remove = written as usize;
        total_length -= remove;

        for iov in iovecs.iter_mut() {
            let to_remove = iov.iov_len.min(remove);
            iov.iov_len -= to_remove;
            iov.iov_base = iov.iov_base.add(to_remove);
            remove -= to_remove;
        }

        if total_length == 0 {
            break;
        }
    }
}

fn main() {
    unsafe {
        test_write(c"Hello World\n");
        test_writev([c"Hello\n", c"World\n"]);
    }
}
