// Copyright (C) 2025 Rémy Cases
// See LICENSE file for extended copyright information.
// This file is part of adventOfCode project from https://github.com/remyCases/adventOfCode.

use std::path::Path;
use std::env;
use std::io::{Error, ErrorKind};
use nom::*;
use nom::error::Error as NomError;
use aoc_utils::*;

fn find_repeated_twice_ids(s: u64, e: u64) -> u64 {
    let n: u32 = s.ilog10();
    if n % 2 == 0 { return 0; }

    let u: u64 = 10u64.pow(n/2 + 1) + 1;
    let us = if s % u == 0 { s / u } else { s / u + 1 };
    let ue = e / u;
    if ue < us { return 0; }
    return ((ue+1)*ue / 2 - (us-1)*us / 2) * u;
}

fn find_repeated_ids(s: u64, e: u64) -> u64 {
    let n: u32 = s.ilog10();
    if n % 2 == 0 { return 0; }

    let u: u64 = 10u64.pow(n/2 + 1) + 1;
    let us = if s % u == 0 { s / u } else { s / u + 1 };
    let ue = e / u;
    if ue < us { return 0; }
    return ((ue+1)*ue / 2 - (us-1)*us / 2) * u;
}

fn read_file_and_compute(file_path: &Path, part: argparse::ArgPart) -> io::Result<()>
{
    let mut invalid_ids: u64 = 0;

    parse_compute!(
        file_path, part,
        multi::separated_list0(
            character::complete::char::<&str, NomError<_>>(','),
            sequence::separated_pair(
                combinator::map_res(character::complete::digit1, str::parse::<u64>),
                character::complete::char('-'),
                combinator::map_res(character::complete::digit1, str::parse),
            )
        ), 
        |v: Vec<(u64, u64)>| {
            for (mut s,e) in v {
                let mut ns: u32 = s.ilog10();
                let ne: u32 = e.ilog10();
                while ns < ne
                {
                    invalid_ids += find_repeated_twice_ids(s, 10u64.pow(ns+1)-1);
                    ns += 1;
                    s = 10u64.pow(ns);
                }
                invalid_ids += find_repeated_twice_ids(s, e);
            }
        },
        |v: Vec<(u64, u64)>| {
            for (mut s,e) in v {
                let mut ns: u32 = s.ilog10();
                let ne: u32 = e.ilog10();
                while ns < ne
                {
                    invalid_ids += find_repeated_ids(s, 10u64.pow(ns+1)-1);
                    ns += 1;
                    s = 10u64.pow(ns);
                }
                invalid_ids += find_repeated_ids(s, e);
            }
        }
    );

    println!("INVALID IDS: {}", invalid_ids);
    Ok(())
}

main!("2025", "input_day_two");
