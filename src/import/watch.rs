use std::collections::HashSet;
use std::path::{Path, PathBuf};


//------------ WatchSet ------------------------------------------------------

#[derive(Clone, Debug, Default)]
pub struct WatchSet {
    enable: bool,
    paths: HashSet<PathBuf>,
}

impl WatchSet {
    pub fn new() -> Self {
        Default::default()
    }

    pub fn enable(&mut self) {
        self.enable = true;
    }

    pub fn add(&mut self, path: PathBuf) {
        if self.enable {
            self.paths.insert(path);
        }
    }

    pub fn iter(&self) -> impl Iterator<Item = &Path> + '_ {
        self.paths.iter().map(|path| path.as_ref())
    }
}

