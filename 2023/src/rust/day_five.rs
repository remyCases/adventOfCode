// Copyright (C) 2023 Rémy Cases
// See LICENSE file for extended copyright information.
// This file is part of adventOfCode project from https://github.com/remyCases/adventOfCode.

use std::path::Path;
use std::fs::File;
use std::io::{BufRead, Lines, BufReader, Error, ErrorKind};
use std::env;

fn line_iterator(file_path: &Path) -> Result<Lines<BufReader<File>>, Error> {
    let file = File::open(file_path)?;
    Ok(BufReader::new(file).lines())
}

fn read_file_and (file_path: &Path, part: u8) -> Result<(), Error> {
    Ok(())
}

pub fn main(part: u8) -> Result<(), Error> {
    let filename = env::current_dir()?.join("2023").join("data").join("test");
    read_file_and(&filename, part)?;
    Ok(())
}