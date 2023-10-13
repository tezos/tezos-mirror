#![cfg(target_os = "none")]

use core::alloc::GlobalAlloc;

extern "C" {
    /// This symbol resolves to the start of the heap. It is the address
    /// of this name that indicates the start, not its value.
    static _sheap: u8;
}

/// Updates the offset value at the start of the heap.
fn write_offset(offset: usize) {
    unsafe {
        let offset_ptr = &_sheap as *const u8 as usize as *mut usize;
        offset_ptr.write_unaligned(offset);
    }
}

/// Reads the offset value at the start of the heap.
fn read_offset() -> usize {
    unsafe {
        let offset_ptr = &_sheap as *const u8 as usize as *mut usize;
        offset_ptr.read_unaligned()
    }
}

/// Our dumb allocator
pub struct Allocator;

unsafe impl GlobalAlloc for Allocator {
    unsafe fn alloc(&self, layout: core::alloc::Layout) -> *mut u8 {
        let size = layout.size();
        let align = layout.align();

        // This is where the heap starts.
        let heap_start = &_sheap as *const u8 as usize;

        // Get the offset from the heap start where the first allocatable byte
        // exists.
        let offset = read_offset();

        // Calculate the address of the newly allocated area, and properly
        // align it.
        let base_address = heap_start + offset;
        let alignment_offset = align - (base_address % align);
        let address = base_address + alignment_offset;

        // Make sure the offset is now correctly updated.
        write_offset(offset + alignment_offset + size);

        address as *mut u8
    }

    unsafe fn dealloc(&self, _ptr: *mut u8, _layout: core::alloc::Layout) {}
}

unsafe impl Sync for Allocator {}

/// Initialise the dumb allocator.
pub fn init() {
    write_offset(core::mem::size_of::<usize>());
}

/// Register our allocator as the global allocator.
#[global_allocator]
pub static ALLOCATOR: Allocator = Allocator;
