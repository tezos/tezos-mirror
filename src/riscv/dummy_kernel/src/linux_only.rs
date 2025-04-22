// SPDX-FileCopyrightText: 2025 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

use std::ffi::CStr;
use std::time::Instant;
use std::time::SystemTime;

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

fn random(mut buffer: &mut [u8]) {
    while !buffer.is_empty() {
        let len = unsafe { libc::getrandom(buffer.as_mut_ptr().cast(), buffer.len(), 0) };
        assert!(len > 0);
        buffer = &mut buffer[len as usize..];
    }
}

fn fit_ascii(c: u8) -> u8 {
    const A: u8 = b'a';
    const Z: u8 = b'z';

    if !(A..=Z).contains(&c) {
        c % (Z - A + 1) + A
    } else {
        c
    }
}

#[expect(dead_code)]
#[derive(Debug, Default)]
struct Foo {
    a: usize,
    b: u64,
    c: u8,
}

/// Do some dummy stuff to test the kernel
pub fn dummy() {
    unsafe {
        test_write(c"Hello World\n");
        test_writev([c"Hello\n", c"World\n"]);
    }

    let mut chars = [0; 6];
    random(&mut chars[..5]);
    let chars = chars.map(fit_ascii);

    let letters = std::str::from_utf8(&chars).unwrap();
    eprintln!("Random letters: {letters}");

    println!("Hello World");

    unsafe {
        let mut buffer = [0u8; 256];
        libc::getcwd(buffer.as_mut_ptr().cast(), buffer.len());

        let cstr = CStr::from_bytes_with_nul_unchecked(&buffer);

        test_write(c"The current working directory is: ");
        test_write(cstr);
        test_write(c"\n");
    }

    let boxed = Box::new(Foo::default());
    println!("Debug {boxed:#?}");

    let time = SystemTime::now();
    println!("System time: {time:?}");

    let instant = Instant::now();
    println!("Instant: {instant:?}");
}
