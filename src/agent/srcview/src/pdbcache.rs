// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

use std::borrow::{Borrow, Cow};
use std::collections::BTreeMap;
use std::fmt::Debug;
use std::fs::File;
use std::path::{Path, PathBuf};

use anyhow::{format_err, Result};
use log::*;
use ouroboros::self_referencing;
use pdb::{FallibleIterator, SymbolData, PDB};

use crate::SrcLine;

/// A struct containing semi-owned PDB streams that are important to us.
///
/// See <https://github.com/willglynn/pdb/issues/12> for context.
struct PdbData<'s> {
    modules: Vec<pdb::ModuleInfo<'s>>,

    address_map: pdb::AddressMap<'s>,
    string_table: pdb::StringTable<'s>,
    debug_info: pdb::DebugInformation<'s>,
}

impl<'s> PdbData<'s> {
    fn new<S: pdb::Source<'s> + 's>(mut pdb: PDB<'s, S>) -> Result<Self> {
        let address_map = pdb.address_map()?;
        let string_table = pdb.string_table()?;
        let debug_info = pdb.debug_information()?;

        let mut data = Self {
            modules: Vec::new(),

            address_map,
            string_table,
            debug_info,
        };

        data.modules = data
            .debug_info
            .modules()?
            .filter_map(|m| {
                Ok(match pdb.module_info(&m)? {
                    Some(mi) => Some(mi),
                    None => None,
                })
            })
            .collect::<Vec<_>>()?;

        Ok(data)
    }
}

#[derive(Debug)]
pub struct PdbCacheData<'d> {
    offset_to_line: BTreeMap<usize, SrcLine<'d>>,
    symbol_to_lines: BTreeMap<Cow<'d, str>, Vec<SrcLine<'d>>>,
    path_to_symbols: BTreeMap<Cow<'d, Path>, Vec<Cow<'d, str>>>,
    path_to_lines: BTreeMap<Cow<'d, Path>, Vec<usize>>,
}

impl<'d> PdbCacheData<'d> {
    fn new(data: &'d mut PdbData<'static>) -> Result<Self> {
        let mut offset_to_line: BTreeMap<usize, SrcLine> = BTreeMap::new();
        let mut symbol_to_lines: BTreeMap<Cow<'d, str>, Vec<SrcLine>> = BTreeMap::new();

        // NOTE: We're using strings as the keys for now while we build the trees, since
        // PathBuf comparisons are expensive.
        let mut path_to_symbols: BTreeMap<Cow<'d, str>, Vec<Cow<'d, str>>> = BTreeMap::new();
        let mut path_to_lines: BTreeMap<Cow<'d, str>, Vec<usize>> = BTreeMap::new();

        let address_map = &data.address_map;
        let string_table = &data.string_table;

        for info in data.modules.iter() {
            let program = info.line_program()?;
            let mut symbols = info.symbols()?;

            while let Some(symbol) = symbols.next()? {
                if let Ok(SymbolData::Procedure(proc)) = symbol.parse() {
                    let proc_name = proc.name.to_string();
                    let mut lines = program.lines_for_symbol(proc.offset);

                    let symbol_to_lines = symbol_to_lines.entry(proc_name.clone()).or_default();

                    while let Some(line_info) = lines.next()? {
                        let rva = line_info
                            .offset
                            .to_rva(&address_map)
                            .ok_or_else(|| format_err!("invalid RVA: {:?}", line_info))?;
                        let file_info = program.get_file_info(line_info.file_index)?;
                        let file_name = file_info.name.to_string_lossy(&string_table)?;

                        let line = line_info.line_start as usize;

                        let srcloc = SrcLine::new(
                            match file_name.clone() {
                                Cow::Owned(p) => Cow::Owned(PathBuf::from(p)),
                                Cow::Borrowed(p) => Cow::Borrowed(Path::new(p)),
                            },
                            line,
                        );

                        offset_to_line.insert(rva.0 as usize, srcloc.clone());
                        path_to_symbols
                            .entry(file_name.clone())
                            .or_default()
                            .push(proc_name.clone());
                        symbol_to_lines.push(srcloc.clone());
                        path_to_lines
                            .entry(file_name.clone())
                            .or_default()
                            .push(line);
                    }
                }
            }
        }

        Ok(Self {
            offset_to_line,
            symbol_to_lines,
            path_to_symbols: path_to_symbols
                .into_iter()
                .map(|(p, s)| {
                    (
                        match p {
                            Cow::Owned(p) => Cow::Owned(PathBuf::from(p)),
                            Cow::Borrowed(p) => Cow::Borrowed(Path::new(p)),
                        },
                        s,
                    )
                })
                .collect(),
            path_to_lines: path_to_lines
                .into_iter()
                .map(|(p, s)| {
                    (
                        match p {
                            Cow::Owned(p) => Cow::Owned(PathBuf::from(p)),
                            Cow::Borrowed(p) => Cow::Borrowed(Path::new(p)),
                        },
                        s,
                    )
                })
                .collect(),
        })
    }
}

#[self_referencing]
struct PdbCacheOwnedData {
    pdb_data: PdbData<'static>,
    #[borrows(mut pdb_data)]
    #[covariant]
    data: PdbCacheData<'this>,
}

impl Debug for PdbCacheOwnedData {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("PdbCacheOwnedData")
            .field("data", &self.borrow_data())
            .finish_non_exhaustive()
    }
}

#[derive(Debug)]
pub struct PdbCache {
    inner: PdbCacheOwnedData,
}

impl PdbCache {
    pub fn new<P: AsRef<Path>>(pdb: P) -> Result<Self> {
        let pdbfile = File::open(pdb)?;

        Ok(Self {
            inner: PdbCacheOwnedDataTryBuilder {
                pdb_data: PdbData::new(PDB::open(pdbfile)?)?,
                data_builder: |file| PdbCacheData::new(file),
            }
            .try_build()?,
        })
    }

    pub fn offset(&self, off: &usize) -> Option<SrcLine> {
        self.inner.borrow_data().offset_to_line.get(off).cloned()
    }

    pub fn paths(&self) -> impl Iterator<Item = &Path> {
        self.inner
            .borrow_data()
            .path_to_lines
            .keys()
            .map(|p| p.borrow())
    }

    pub fn path_symbols<P: AsRef<Path>>(&self, path: P) -> Option<impl Iterator<Item = &str>> {
        self.inner
            .borrow_data()
            .path_to_symbols
            .get(path.as_ref())
            .map(|x| x.iter().map(|y| y.borrow()))
    }

    pub fn path_lines<P: AsRef<Path>>(&self, path: P) -> Option<impl Iterator<Item = &usize>> {
        self.inner
            .borrow_data()
            .path_to_lines
            .get(path.as_ref())
            .map(|x| x.iter())
    }

    pub fn symbol(&self, sym: &str) -> Option<impl Iterator<Item = &SrcLine>> {
        self.inner
            .borrow_data()
            .symbol_to_lines
            .get(sym)
            .map(|x| x.iter())
    }
}
