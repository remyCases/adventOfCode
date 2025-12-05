// Copyright (C) 2025 Rémy Cases
// See LICENSE file for extended copyright information.
// This file is part of adventOfCode project from https://github.com/remyCases/adventOfCode.

use std::path::Path;
use std::env;
use std::io::{Error, ErrorKind};
use nom::*;
use nom::error::Error as NomError;
use aoc_utils::*;

fn read_file_and_compute(file_path: &Path, part: argparse::ArgPart) -> io::Result<()>
{
    let mut invalid_ids: u32 = 0;

    parse_compute!(
        file_path, part,
        multi::separated_list0(
            character::complete::char::<&str, NomError<_>>(','),
            sequence::separated_pair(
                combinator::map_res(character::complete::digit1, str::parse),
                character::complete::char('-'),
                combinator::map_res(character::complete::digit1, str::parse),
            )
        ), 
        |v: Vec<(u32, u32)>| {
            for (s,e) in v {
                let ns: u32 = s.ilog10();
                let ne: u32 = e.ilog10();
                if ne == ns && ne % 2 == 0 { continue; }
                for i in s..=e {
                    let n: u32 = i.ilog10();
                    if n % 2 == 0 { continue; }
                    if i % ( 10u32.pow(n/2+1) + 1 ) == 0 { invalid_ids += i; println!("{}", i); }
                }
            }
        },
        |v: Vec<(u32, u32)>| {
        }
    );

    println!("INVALID IDS: {}", invalid_ids);
    Ok(())
}

main!("2025", "input_day_two");
