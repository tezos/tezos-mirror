use crate::syscalls::{write_str, StdOut};

pub fn main() {
    write_str(StdOut, "Hello World\n");
}
