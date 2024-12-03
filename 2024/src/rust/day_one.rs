// Copyright (C) 2024 Rémy Cases
// See LICENSE file for extended copyright information.
// This file is part of adventOfCode project from https://github.com/remyCases/adventOfCode.

use std::path::Path;
use std::env;
use std::io::{Error, ErrorKind};
use nom::*;
use aoc_utils::*;

fn parse_line(line: &str) -> IResult<&str, (i32, i32)>{
    let (_, (n1, _, n2)) = sequence::tuple((
        combinator::map_res(
            character::complete::digit0,
            str::parse
        ),
        character::complete::multispace0,
        combinator::map_res(
            character::complete::digit0,
            str::parse
        )
    ))(line)?;

    Ok((line, (n1, n2)))
}

fn read_file_and_compute_distance(file_path: &Path, part: argparse::ArgPart) -> io::Result<()> {
    let lines = io::line_iterator(file_path)?;
    let mut vec1 = Vec::new();
    let mut vec2 = Vec::new();

    for (nline, line) in lines.enumerate() {
        let binding_line = line?;
        let (_, (n1, n2)) = parse_line(&binding_line).map_err(|err| Error::new(
            ErrorKind::InvalidData,
            err.to_string() + &format!(" in line: {:}", nline)
        ))?;
        vec1.push(n1);
        vec2.push(n2);
    }

    vec1.sort();
    vec2.sort();

    let result = zip!(vec1, vec2).fold(0, |acc, (n, m)| acc + i32::abs(n - m));
    
    println!("DISTANCE: {:}", result);
    Ok(())
}

pub fn main(part: argparse::ArgPart) -> io::Result<()> {
    let filename = env::current_dir()?.join("2024").join("data").join("input_day_one");
    read_file_and_compute_distance(&filename, part)?;
    Ok(())
}
