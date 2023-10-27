#![cfg(all(target_arch = "riscv64", feature = "proto-alpha"))]

///! Implements Linux system calls
///! See https://git.musl-libc.org/cgit/musl/tree/arch/riscv64/syscall_arch.h

#[cfg(not(target_os = "none"))]
mod from_libc {
    /// Write data to a file descriptor.
    pub fn write(fd: i64, buf: *const u8, count: usize) -> i64 {
        unsafe { libc::write(fd as i32, buf as *const libc::c_void, count) as i64 }
    }

    pub fn exit(code: i32) -> ! {
        unsafe { libc::exit(code) }
    }
}

#[cfg(target_os = "none")]
mod bare_metal {
    use core::arch::asm;

    /// Issues a system call with 1 argument.
    fn call1(number: u64, arg0: u64) -> u64 {
        let ret;
        unsafe {
            asm!(
                "ecall",
                in("a7") number,
                in("a0") arg0,
                lateout("a0") ret
            )
        }
        ret
    }

    /// Issues a system call with 3 arguments.
    fn call3(number: u64, arg0: u64, arg1: u64, arg2: u64) -> u64 {
        let ret;
        unsafe {
            asm!(
                "ecall",
                in("a7") number,
                in("a0") arg0,
                in("a1") arg1,
                in("a2") arg2,
                lateout("a0") ret
            )
        }
        ret
    }

    /// Write data to a file descriptor.
    pub fn write(fd: i64, buf: *const u8, count: usize) -> i64 {
        call3(64, fd as u64, buf as u64, count as u64) as i64
    }

    /// Exit the kernel with a status code.
    pub fn exit(code: i32) -> ! {
        call1(93, code as u64) as i64;
        unreachable!()
    }
}

#[cfg(not(target_os = "none"))]
pub use from_libc::*;

#[cfg(target_os = "none")]
pub use bare_metal::*;

/// Available file descriptor for output
#[repr(i64)]
pub enum OutputFileDescriptor {
    /// Standard output
    StdOut = 1,

    /// Standard error
    StdErr = 2,
}

pub use OutputFileDescriptor::*;
