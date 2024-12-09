// Copyright (C) 2024 Rémy Cases
// See LICENSE file for extended copyright information.
// This file is part of adventOfCode project from https://github.com/remyCases/adventOfCode.

use std::path::Path;
use std::env;
use std::io::{Error, ErrorKind};
use std::collections::HashMap;
use nom::*;
use nom::error::Error as NomError;
use aoc_utils::*;

fn read_file_and_compute(file_path: &Path, part: argparse::ArgPart) -> io::Result<()> {
    let mut vec1: Vec<i32> = Vec::new();
    let mut vec2: Vec<i32> = Vec::new();
    let mut map1: HashMap<i32, i32> = HashMap::new();
    let mut map2: HashMap<i32, i32> = HashMap::new();

    parse_compute!(
        file_path, part,
        sequence::separated_pair(
            combinator::map_res(
                character::complete::digit0::<_, NomError<_>>,
                str::parse
            ),
            character::complete::multispace0,
            combinator::map_res(
                character::complete::digit0::<_, NomError<_>>,
                str::parse
            )
        ),
        |(x, y)| {
            vec1.push(x); 
            vec2.push(y);
        },
        |(x, y)| {
            map1.entry(x).and_modify(|c| {*c += 1}).or_insert(1); 
            map2.entry(y).and_modify(|c| {*c += 1}).or_insert(1);
        }
    );


    println!("DISTANCE: {:}", match part {
        argparse::ArgPart::PartOne => {
            vec1.sort();
            vec2.sort();
            zip!(vec1, vec2).fold(0, |acc, (n, m)| acc + i32::abs(n - m))
        },
        argparse::ArgPart::PartTwo => {
            map1.iter().fold(0, |acc, (k, v)| acc + *k * *v * map2.get(k).unwrap_or(&0))
        }
    });
    Ok(())
}

main!("2024", "input_day_one");
