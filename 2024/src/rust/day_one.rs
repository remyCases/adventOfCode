// Copyright (C) 2024 Rémy Cases
// See LICENSE file for extended copyright information.
// This file is part of adventOfCode project from https://github.com/remyCases/adventOfCode.

use std::path::Path;
use std::env;

use crate::utils_io::EResult;
use crate::utils_io::line_iterator;
use crate::utils_io::ArgPart;

fn read_file_and_compute_distance(file_path: &Path, part: ArgPart) -> EResult {
    let lines = line_iterator(file_path)?;

    Ok(())
}

pub fn main(part: ArgPart) -> EResult {
    let filename = env::current_dir()?.join("2024").join("data").join("input_day_one");
    read_file_and_compute_distance(&filename, part)?;
    Ok(())
}
