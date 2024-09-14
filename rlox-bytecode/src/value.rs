use std::cmp::{Ordering, PartialEq};
use std::fmt;
use std::ops::{Add, Div, Mul, Neg, Sub};

use std::alloc::{self, Layout};
use std::ptr;
use std::str;

// Represents a dynamically allocated string in the VM.
pub struct ObjString {
    length: usize,
    chars: *mut u8, // Pointer to the UTF-8 bytes
}

impl ObjString {
    // Creates a new ObjString from a Rust &str.
    fn new(s: &str) -> Self {
        let length = s.len();
        let layout = Layout::array::<u8>(length + 1).unwrap(); // +1 for null-terminator
        unsafe {
            let chars = alloc::alloc(layout);
            ptr::copy_nonoverlapping(s.as_ptr(), chars, length);
            *chars.add(length) = b'\0'; // null-terminate the string
            ObjString { length, chars }
        }
    }

    // Converts the ObjString back to a Rust &str safely.
    fn as_str(&self) -> &str {
        unsafe {
            let slice = std::slice::from_raw_parts(self.chars, self.length);
            str::from_utf8(slice).unwrap()
        }
    }
}

impl Drop for ObjString {
    // Cleans up the allocated memory when the ObjString is dropped.
    fn drop(&mut self) {
        let layout = Layout::array::<u8>(self.length + 1).unwrap(); // +1 for null-terminator
        unsafe {
            alloc::dealloc(self.chars, layout);
        }
    }
}

pub enum Obj {
    String(ObjString),
}

impl Obj {
    fn as_str(&self) -> &str {
        match self {
            Obj::String(s) =>  s.as_str(),
        }
    }
}

#[derive(Debug, PartialEq)]
enum ValueType {
    Bool,
    Nil,
    Number,
    Obj,
}

union V {
    boolean: bool,
    number: f64,
    obj: *mut Obj, // Pointer to a heap-allocated obj
}

pub struct Value {
    typ: ValueType,
    _as: V,
}

pub type ValueArray = Vec<Value>;

impl Value {
    pub fn new_bool(value: bool) -> Value {
        Value {
            typ: ValueType::Bool,
            _as: V { boolean: value },
        }
    }

    pub fn new_nil() -> Value {
        Value {
            typ: ValueType::Nil,
            _as: V { number: 0.0 },
        }
    }

    pub fn new_number(value: f64) -> Value {
        Value {
            typ: ValueType::Number,
            _as: V { number: value },
        }
    }

    pub fn new_string(s: &str) -> Value {
        let obj_string = Box::new(Obj::String(ObjString::new(s)));
        let obj_ptr = Box::into_raw(obj_string);

        Value {
            typ: ValueType::Obj,
            _as: V { obj: obj_ptr },
        }
    }

    pub fn as_bool(&self) -> bool {
        unsafe { self._as.boolean }
    }

    pub fn as_number(&self) -> f64 {
        unsafe { self._as.number }
    }

    pub fn as_string(&self) -> &str {
        assert_eq!(self.typ, ValueType::Obj);
        unsafe { (*self._as.obj).as_str() }
    }

    pub fn is_bool(&self) -> bool {
        self.typ == ValueType::Bool
    }

    pub fn is_nil(&self) -> bool {
        self.typ == ValueType::Nil
    }

    pub fn is_number(&self) -> bool {
        self.typ == ValueType::Number
    }

    pub fn is_string(&self) -> bool {
        unsafe {
            self.typ == ValueType::Obj && !self._as.obj.is_null()
        }
    }
}

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        if self.typ != other.typ {
            false
        } else {
            match self.typ {
                ValueType::Bool => self.as_bool() == other.as_bool(),
                ValueType::Nil => true,
                ValueType::Number => self.as_number() == other.as_number(),
                ValueType::Obj => {
                    if self.is_string() && other.is_string() {
                        self.as_string() == other.as_string()
                    } else {
                        // Additional object type comparisons would go here
                        // For now, just compare pointers for non-string objects
                        unsafe { self._as.obj == other._as.obj }
                    }
                }
            }
        }
    }
}

impl Clone for Value {
    fn clone(&self) -> Self {
        match self.typ {
            ValueType::Bool => Value {
                typ: ValueType::Bool,
                _as: V { boolean: unsafe { self._as.boolean } },
            },
            ValueType::Nil => Value {
                typ: ValueType::Nil,
                _as: V { number: 0.0 }, // 'Nil' value represented as a number
            },
            ValueType::Number => Value {
                typ: ValueType::Number,
                _as: V { number: unsafe { self._as.number } },
            },
            ValueType::Obj => {
                if self.is_string() {
                    Value::new_string( self.as_string() )
                } else {
                    unimplemented!()
                }
            }
        }
    }
}


impl Add for Value {
    type Output = Self;

    fn add(mut self, other: Self) -> Self::Output {
        match (&self.typ, &other.typ) {
            (ValueType::Number, ValueType::Number) => {
                self._as = V {
                    number: self.as_number() + other.as_number(),
                }
            }
            _ => panic!("undefined behaviour"),
        }
        self
    }
}

impl Sub for Value {
    type Output = Self;

    fn sub(mut self, other: Self) -> Self::Output {
        match (&self.typ, &other.typ) {
            (ValueType::Number, ValueType::Number) => {
                self._as = V {
                    number: self.as_number() - other.as_number(),
                }
            }
            _ => panic!("undefined behaviour"),
        }
        self
    }
}

impl Mul for Value {
    type Output = Self;

    fn mul(mut self, other: Self) -> Self::Output {
        match (&self.typ, &other.typ) {
            (ValueType::Number, ValueType::Number) => {
                self._as = V {
                    number: self.as_number() * other.as_number(),
                }
            }
            _ => panic!("undefined behaviour"),
        }
        self
    }
}

impl Div for Value {
    type Output = Self;

    fn div(mut self, other: Self) -> Self::Output {
        match (&self.typ, &other.typ) {
            (ValueType::Number, ValueType::Number) => {
                self._as = V {
                    number: self.as_number() / other.as_number(),
                }
            }
            _ => panic!("undefined behaviour"),
        }
        self
    }
}

impl Neg for Value {
    type Output = Self;

    fn neg(mut self) -> Self::Output {
        match &self.typ {
            ValueType::Number => {
                self._as = V {
                    number: -self.as_number(),
                }
            }
            _ => panic!("undefined behaviour"),
        }
        self
    }
}

impl Eq for Value {}

impl fmt::Debug for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.typ {
            ValueType::Bool => write!(f, "Value({})", self.as_bool()),
            ValueType::Nil => write!(f, "Value(nil)"),
            ValueType::Number => write!(f, "Value({})", self.as_number()),
            ValueType::Obj => {
                if self.is_string() {
                    write!(f, "Value(\"{}\")", self.as_string())
                } else {
                    write!(f, "Value(Object)")
                }
            },
        }
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.typ {
            ValueType::Bool => write!(f, "{}", self.as_bool()),
            ValueType::Nil => write!(f, "nil"),
            ValueType::Number => write!(f, "{}", self.as_number()),
            ValueType::Obj => {
                if self.is_string() {
                    write!(f, "{}", self.as_string())
                } else {
                    write!(f, "[Object]")
                }
            },
        }
    }
}

impl PartialOrd for Value {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        match (&self.typ, &other.typ) {
            (&ValueType::Number, &ValueType::Number) => {
                self.as_number().partial_cmp(&other.as_number())
            }
            _ => None, // Only numbers are comparable
        }
    }

    fn lt(&self, other: &Self) -> bool {
        match (&self.typ, &other.typ) {
            (&ValueType::Number, &ValueType::Number) => self.as_number() < other.as_number(),
            _ => panic!("Comparison is only valid between numbers"),
        }
    }

    fn gt(&self, other: &Self) -> bool {
        match (&self.typ, &other.typ) {
            (&ValueType::Number, &ValueType::Number) => self.as_number() > other.as_number(),
            _ => panic!("Comparison is only valid between numbers"),
        }
    }
}

impl Drop for Value {
    fn drop(&mut self) {
        if self.is_string() {
            unsafe {
                let _ = Box::from_raw(self._as.obj); // Automatically calls drop for ObjString
            }
        }
    }
}
