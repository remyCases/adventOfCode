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
    let mut first_battery: u32 = 0;
    let mut first_index: usize = 0;
    let mut second_battery: u32 = 0;
    let mut sum_joltage: u32 = 0;

    parse_compute!(
        file_path, part,
        multi::many0(
            combinator::map_opt(character::complete::anychar::<&str, NomError<_>>, |c: char| c.to_digit(10))
        ), 
        |v: Vec<u32>| {
            first_battery = 0;
            second_battery = 0;
            for i in 0..(v.len()-1) {
                if v[i] > first_battery {
                    first_battery = v[i];
                    first_index = i;
                }
            }
            for j in (first_index+1)..v.len() {
                if v[j] > second_battery {
                    second_battery = v[j];
                }
            }
            sum_joltage += first_battery * 10 + second_battery;
        },
        |v: Vec<u32>| {
            
        }
    );

    println!("TOTAL JOLTAGE: {}", sum_joltage);
    Ok(())
}

main!("2025", "input_day_three");
