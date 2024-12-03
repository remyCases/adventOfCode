// Copyright (C) 2023 Rémy Cases
// See LICENSE file for extended copyright information.
// This file is part of adventOfCode project from https://github.com/remyCases/adventOfCode.

use std::path::Path;
use std::env;
use std::io::{Error, ErrorKind};
use nom::*;
use aoc_utils::*;

const BUFFER_LEN: usize = 3;

fn parse_line<'a>(line: &'a str) -> IResult<&'a str, &'a str>{
    let (_, n) = combinator::map_res(
        character::complete::digit0,
        str::parse
    )(line)?;

    Ok((line, n))
}

fn read_file_and_compute_calories(file_path: &Path, part: argparse::ArgPart) -> io::Result<()> {
    let lines = io::line_iterator(file_path)?;
    let mut max = 0;
    let mut sum = 0;
    let mut calories_buf: Vec<i32> = vec![0; BUFFER_LEN];

    for (nline, line) in lines.enumerate() {
        let binding_line = line?;
        let _ = parse_line(&binding_line, &sum).map_err(|err| Error::new(
            ErrorKind::InvalidData,
            err.to_string() + &format!(" in line: {:}", nline)
        ))?;

        if calories_buf[0] < sum {
            calories_buf[0] = sum;
            calories_buf.sort();
        }
    }
        
    max = match part {
        argparse::ArgPart::PartOne => calories_buf[-1],
        argparse::ArgPart::PartTwo => calories_buf.iter().sum()
    };
    println!("MAX CALORIES: {:}", max);
    Ok(())
}

pub fn main(part: argparse::ArgPart) -> io::Result<()> {
    let filename = env::current_dir()?.join("2022").join("data").join("input_day_one");
    read_file_and_compute_calories(&filename, part)?;
    Ok(())
}
