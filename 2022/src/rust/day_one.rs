// Copyright (C) 2023 Rémy Cases
// See LICENSE file for extended copyright information.
// This file is part of adventOfCode project from https://github.com/remyCases/adventOfCode.

use std::path::Path;
use std::env;
use std::io::{Error, ErrorKind};
use nom::*;
use nom::error::{Error as NomError};
use aoc_utils::*;

const BUFFER_LEN: usize = 3;

fn read_file_and_compute_calories(file_path: &Path, part: argparse::ArgPart) -> io::Result<()> {

    let mut calories_buf: [i32; BUFFER_LEN] = [0; BUFFER_LEN];
    let mut sum = 0;

    parse_compute!(
        file_path,
        branch::alt((
            combinator::map(
                combinator::map_res(
                    character::complete::digit0::<_, NomError<_>>,
                    str::parse
                ),
                Some
            ),
            combinator::map(
                character::complete::multispace0, 
                |_x| None
            )
        ))
        ,
        |o: Option<i32>| if let Some(x) = o {
            sum += x;
            if calories_buf[0] < sum {
                calories_buf[0] = sum;
                calories_buf.sort();
            }
        } else {
            sum = 0;
        }
    );
        
    println!("MAX CALORIES: {:}", match part {
        argparse::ArgPart::PartOne => calories_buf[calories_buf.len() - 1],
        argparse::ArgPart::PartTwo => calories_buf.iter().sum()
    });
    Ok(())
}

pub fn main(part: argparse::ArgPart) -> io::Result<()> {
    let filename = env::current_dir()?.join("2022").join("data").join("input_day_one");
    read_file_and_compute_calories(&filename, part)?;
    Ok(())
}
