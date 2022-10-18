// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

use std::borrow::Cow;
use std::cmp::Ordering;
use std::fmt;
use std::path::Path;

/// A path and a line number
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct SrcLine<'p> {
    pub path: Cow<'p, Path>,
    pub line: usize,
}

impl fmt::Display for SrcLine<'_> {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(fmt, "{}:{}", &self.path.display(), self.line)
    }
}

impl Ord for SrcLine<'_> {
    fn cmp(&self, other: &Self) -> Ordering {
        let path_cmp = self.path.cmp(&other.path);

        if path_cmp != Ordering::Equal {
            return path_cmp;
        }

        self.line.cmp(&other.line)
    }
}

impl PartialOrd for SrcLine<'_> {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl<'p> SrcLine<'p> {
    pub fn new(path: Cow<'p, Path>, line: usize) -> Self {
        Self { path, line }
    }

    pub fn to_owned(&'p self) -> SrcLine<'static> {
        SrcLine {
            path: Cow::Owned(self.path.to_path_buf()),
            line: self.line,
        }
    }
}
