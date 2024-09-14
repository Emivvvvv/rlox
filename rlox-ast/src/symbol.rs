use std::fmt;
use rustc_hash::FxHashMap;

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub struct Symbol(pub usize);

impl Symbol {
    // Helper function to display symbol with resolution from SymbolTable
    pub fn display_with_table<'a>(&self, table: &'a SymbolTable) -> SymbolDisplay<'a> {
        SymbolDisplay {
            symbol: *self,
            table,
        }
    }
}


impl fmt::Display for Symbol {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Symbol({})", self.0)
    }
}

#[derive(Debug)]
pub struct SymbolTable {
    strings: Vec<String>,
    indices: FxHashMap<String, Symbol>,
}

impl SymbolTable {
    pub fn new() -> Self {
        SymbolTable {
            strings: Vec::new(),
            indices: FxHashMap::default(),
        }
    }

    pub fn intern(&mut self, name: &str) -> Symbol {
        if let Some(&symbol) = self.indices.get(name) {
            symbol
        } else {
            let symbol = Symbol(self.strings.len());
            self.strings.push(name.to_string());
            self.indices.insert(name.to_string(), symbol);
            symbol
        }
    }

    pub fn resolve(&self, symbol: Symbol) -> &str {
        &self.strings[symbol.0]
    }
}

pub struct SymbolDisplay<'a> {
    symbol: Symbol,
    table: &'a SymbolTable,
}

impl<'a> fmt::Display for SymbolDisplay<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // Resolve the symbol to its string from the symbol table
        let resolved = self.table.resolve(self.symbol);
        write!(f, "{}", resolved)
    }
}