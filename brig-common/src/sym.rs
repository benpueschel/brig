//! This module is responsible for interning strings.
//!
//! The compiler is passing around strings a lot,
//! and we want to avoid copying them as much as possible. This module provides a way to store
//! strings in a global table, and then pass around an index to that table instead of the string
//! itself.
//!
//! To use this module, you can call [`Symbol::intern()`] to get a [Symbol] object, which
//! is an index into the global table. You can then call [`Symbol::as_str()`] to get a [IntStr]
//! object, which is a thin wrapper around the string.
//!
//! Note that as long as the [IntStr] object is alive, the global table is locked, so you should
//! not hold onto it for long.
//! If you need to hold on to the string, store the [Symbol] object instead, and call
//! [`Symbol::as_str()`] or [`Symbol::to_string()`] when you need the string.
//!

use std::{
    fmt::Display,
    ops::Deref,
    sync::{LazyLock, Mutex, MutexGuard},
};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Symbol(SymbolIndex);
type SymbolIndex = usize;

type Interner = Vec<String>;
static STRINGS: LazyLock<Mutex<Interner>> = LazyLock::new(|| Mutex::new(Interner::new()));

impl Symbol {
    pub fn intern(s: &str) -> Self {
        let mut strings = STRINGS.lock().unwrap();
        strings
            .iter()
            .enumerate()
            .find_map(|(i, x)| if x == s { Some(Self(i)) } else { None })
            .unwrap_or_else(|| {
                strings.push(String::from(s));
                Self(strings.len() - 1)
            })
    }
    pub fn as_str(&self) -> IntStr<'_> {
        IntStr {
            lock: STRINGS.lock().expect("failed to lock STRINGS"),
            sym: self,
        }
    }
}

impl Display for Symbol {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.as_str())
    }
}

pub struct IntStr<'ctx> {
    lock: MutexGuard<'ctx, Vec<String>>,
    sym: &'ctx Symbol,
}

impl<'a> Display for IntStr<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.deref())
    }
}

impl<'a> Deref for IntStr<'a> {
    type Target = String;

    fn deref(&self) -> &Self::Target {
        self.lock
            .get(self.sym.0)
            .expect("IntStr could not get string. This should be impossible")
    }
}
