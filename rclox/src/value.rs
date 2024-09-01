use std::fmt;
use std::ops::{Add, Div, Mul, Neg, Sub};

#[derive(Debug, Clone, PartialEq, Eq)]
enum ValueType {
    Bool,
    Nil,
    Number,
}

#[derive(Clone, Copy)]
union V {
    boolean: bool,
    number: f64,
}

#[derive(Clone)]
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

    pub fn as_bool(&self) -> bool {
        unsafe { self._as.boolean }
    }

    pub fn as_number(&self) -> f64 {
        unsafe { self._as.number }
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
            }
        }
    }
}

impl Add for Value {
    type Output = Self;

    fn add(mut self, other: Self) -> Self::Output {
        match (&self.typ, &other.typ) {
            (ValueType::Number, ValueType::Number) => self._as = V {number: self.as_number() + other.as_number()},
            _ => panic!("undefined behaviour")
        }
        self
    }
}

impl Sub for Value {
    type Output = Self;

    fn sub(mut self, other: Self) -> Self::Output {
        match (&self.typ, &other.typ) {
            (ValueType::Number, ValueType::Number) => self._as = V {number: self.as_number() - other.as_number()},
            _ => panic!("undefined behaviour")
        }
        self
    }
}

impl Mul for Value {
    type Output = Self;

    fn mul(mut self, other: Self) -> Self::Output {
        match (&self.typ, &other.typ) {
            (ValueType::Number, ValueType::Number) => self._as = V {number: self.as_number() * other.as_number()},
            _ => panic!("undefined behaviour")
        }
        self
    }
}

impl Div for Value {
    type Output = Self;

    fn div(mut self, other: Self) -> Self::Output {
        match (&self.typ, &other.typ) {
            (ValueType::Number, ValueType::Number) => self._as = V {number: self.as_number() / other.as_number()},
            _ => panic!("undefined behaviour")
        }
        self
    }
}

impl Neg for Value {
    type Output = Self;

    fn neg(mut self) -> Self::Output {
        match &self.typ {
            ValueType::Number => self._as = V {number: -self.as_number()},
            _ => panic!("undefined behaviour")
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
        }
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.typ {
            ValueType::Bool => write!(f, "{}", self.as_bool()),
            ValueType::Nil => write!(f, "nil"),
            ValueType::Number => write!(f, "{}", self.as_number()),
        }
    }
}