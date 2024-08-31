use std::alloc;
use std::ptr;

pub fn reallocate<T>(previous: *mut T, old_size: usize, new_size: usize) -> *mut T {
    unsafe {
        let layout = alloc::Layout::from_size_align(old_size, align_of::<T>())
            .expect("Could not determine Layout for reallocation.");
        if new_size == 0 {
            alloc::dealloc(previous as *mut u8, layout);
            ptr::null_mut()
        } else {
            alloc::realloc(previous as *mut u8, layout, new_size) as *mut T
        }
    }
}