use std::path::PathBuf;

use itempath::ItemPathBuf;
use serde::{Deserialize, Serialize};

#[derive(Serialize, Deserialize, Clone)]
pub struct ModuleContent {
    pub path: PathBuf,
    pub module_path: ItemPathBuf,
    pub contents: String,
}

#[derive(Serialize, Deserialize)]
pub struct ModuleRequest {
    pub path: ItemPathBuf,
}
