use std::alloc;
use std::ptr;

pub fn reallocate<T>(previous: *mut T, old_size: usize, new_size: usize) -> *mut T {
    unsafe {
        if new_size == 0 {
            if !previous.is_null() {
                let layout = alloc::Layout::from_size_align(old_size, std::mem::align_of::<T>())
                    .expect("Could not determine Layout for deallocation.");
                alloc::dealloc(previous as *mut u8, layout);
            }
            return ptr::null_mut();
        }

        let layout = alloc::Layout::from_size_align(new_size, std::mem::align_of::<T>())
            .expect("Could not determine Layout for allocation/reallocation.");

        if previous.is_null() {
            alloc::alloc(layout) as *mut T
        } else {
            alloc::realloc(previous as *mut u8, layout, new_size) as *mut T
        }
    }
}

