// Copyright (C) 2023 Rémy Cases
// See LICENSE file for extended copyright information.
// This file is part of adventOfCode project from https://github.com/remyCases/adventOfCode.

use crate::utils_io;
use std::path::Path;
use std::io::{Error, ErrorKind};
use std::env;
use std::cmp::Ordering;

use nom::*;

fn parse_race(line: &str) -> IResult<&str, Vec<i32>> {
    let (numerals, _) = branch::alt((
        bytes::streaming::tag("Time:"),
        bytes::streaming::tag("Distance:")
    ))(line)?;
    
    sequence::preceded(
        character::complete::multispace1, 
        multi::separated_list1(character::complete::multispace1, character::complete::i32))
        (numerals)
}

fn compute_possibilites_from_delta(delta: i32, t: i32) -> i32 {
    match delta.cmp(&0) {
        Ordering::Greater => {
            let t1 = (t as f64 - (delta as f64).sqrt())  / 2.0;
            let t2 = (t as f64 + (delta as f64).sqrt())  / 2.0;
            (t2 - 1.0).ceil() as i32 - (t1 + 1.0).floor() as i32 + 1
        },
        Ordering::Less => 0,
        Ordering::Equal => (t%2 == 0) as i32
    }
}

fn read_file_and_compute_garden(file_path: &Path) -> Result<(), Error> {
    let lines = utils_io::line_iterator(file_path)?;
    let mut vec_time: Vec<i32> = Vec::new();
    let mut vec_distance: Vec<i32> = Vec::new();

    for (nline, line) in lines.enumerate() {
        let (_, vec_numerals) = parse_race(&line?)
            .map_err(|err| Error::new(
                ErrorKind::InvalidData, 
                err.to_string() + &format!(" in line: {:}", nline)
            )
        )?;

        if vec_time.is_empty() { vec_time = vec_numerals; } 
        else { vec_distance = vec_numerals; }
    }

    let result: i32 = vec_time.iter()
        .zip(vec_distance)
        .map(|(&t, d)| (t, t*t - 4*d))
        .map(|(t, delta)| compute_possibilites_from_delta(delta, t))
        .product();
    println!("Product of possibilites: {:}", result);
    Ok(())
}

pub fn main() -> Result<(), Error> {
    let filename = env::current_dir()?.join("2023").join("data").join("input_day_six");
    read_file_and_compute_garden(&filename)?;
    Ok(())
}