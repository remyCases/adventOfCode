// Copyright (C) 2025 Rémy Cases
// See LICENSE file for extended copyright information.
// This file is part of adventOfCode project from https://github.com/remyCases/adventOfCode.

use std::path::Path;
use std::env;
use std::io::{Error, ErrorKind};
use nom::*;
use nom::error::Error as NomError;
use aoc_utils::*;

const NUMBER_BATTERIES: usize = 12;

fn read_file_and_compute(file_path: &Path, part: argparse::ArgPart) -> io::Result<()>
{
    let mut batteries: [u32; NUMBER_BATTERIES] = [0; NUMBER_BATTERIES];
    let mut indeces: [usize; NUMBER_BATTERIES] = [0; NUMBER_BATTERIES];
    let mut sum_joltage: u64 = 0;

    parse_compute!(
        file_path, part,
        multi::many0(
            combinator::map_opt(character::complete::anychar::<&str, NomError<_>>, |c: char| c.to_digit(10))
        ), 
        |v: Vec<u32>| {
            batteries[0] = 0;
            indeces[0] = 0;
            batteries[1] = 0;
            indeces[1] = 0;

            for i in indeces[0]..(v.len()-1) {
                if v[i] > batteries[0] {
                    batteries[0] = v[i];
                    indeces[1] = i+1;
                }
            }
            for i in indeces[1]..v.len() {
                if v[i] > batteries[1] {
                    batteries[1] = v[i];
                }
            }
            sum_joltage += (batteries[0] * 10 + batteries[1]) as u64;
        },
        |v: Vec<u32>| {
            let mut joltage: u64 = 0;
            for i in 0..NUMBER_BATTERIES {
                batteries[i] = 0;
                indeces[i] = 0;
            }

            for i in 0..NUMBER_BATTERIES {
                for j in indeces[i]..(v.len()-NUMBER_BATTERIES+1+i) {
                    if v[j] > batteries[i] {
                        batteries[i] = v[j];
                        if i != NUMBER_BATTERIES-1 { indeces[i+1] = j+1; }
                    }
                }
                joltage = joltage * 10 + batteries[i] as u64;
            }
            sum_joltage += joltage;
            
        }
    );

    println!("TOTAL JOLTAGE: {}", sum_joltage);
    Ok(())
}

main!("2025", "input_day_three");
