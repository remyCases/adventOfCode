// Copyright (C) 2023 Rémy Cases
// See LICENSE file for extended copyright information.
// This file is part of adventOfCode project from https://github.com/remyCases/adventOfCode.

use std::path::Path;
use std::io::{Error, ErrorKind};
use std::env;
use aoc_utils::*;
use nom::*;

fn parse_history(line: &str) -> IResult<&str, Vec<i32>>{
    multi::separated_list1(character::complete::multispace1, character::complete::i32)(line)
}

fn compute_histories(history: &[i32]) -> Vec<i32> {
    let mut current: Vec<i32> = Vec::new();
    let mut previous: Vec<i32> = Vec::new();
    let mut size = 0;
    
    for (_, h) in history.iter().enumerate() {
        for (n, _) in previous.iter().enumerate() {
            if n > 0 {
                current[n] = current[n-1] - previous[n-1];
            } else {
                current[0] = *h;
            }
        }

        if current.is_empty() {
            current.push(*h);
            size += 1;
        } else if current[size-1] != 0 {
            current.push(current[size-1] - previous[size-1]);
            size += 1;
        }

        previous = current.clone();
    }
    previous
}

fn read_file_and_compute_extrapolated_values(file_path: &Path) -> io::Result<()> {
    let lines = io::line_iterator(file_path)?;
    let mut result = 0;

    for (nline, line) in lines.enumerate() {
        let (_, vec_history) = parse_history(&line?)
            .map_err(|err| Error::new(
            ErrorKind::InvalidData, 
            err.to_string() + &format!(" in line: {:}", nline)
            )
        )?;

        result += compute_histories(&vec_history).iter().sum::<i32>();
    }
    println!("Sum of extrapolated values: {:}", result);
    Ok(())
}

pub fn main() -> io::Result<()> {
    let filename = env::current_dir()?.join("2023").join("data").join("input_day_nine");
    read_file_and_compute_extrapolated_values(&filename)?;
    Ok(())
}