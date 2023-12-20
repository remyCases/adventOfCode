// Copyright (C) 2023 Rémy Cases
// See LICENSE file for extended copyright information.
// This file is part of adventOfCode project from https://github.com/remyCases/adventOfCode.

use crate::utils_io;
use std::path::Path;
use std::io::{Error, ErrorKind};
use std::env;
use std::cmp::Ordering;

use nom::*;
use nom::error::{Error as NomError, ParseError};

fn parse_race(line: &str) -> IResult<&str, i64> {
    let (numerals, _) = branch::alt((
        bytes::streaming::tag("Time:"),
        bytes::streaming::tag("Distance:")
    ))(line)?;
    
    let (_ , vec_string) =sequence::preceded(
        character::complete::multispace1::<&str, NomError<_>>,
        multi::separated_list1(character::complete::multispace1, character::complete::digit1))
        (numerals)?;
    let joined = vec_string.join("");
    // since joined is a String, it lives on the heap
    // and we dont want to return something related
    // so I changed the return so it does not refer to something on the head anymore
    character::complete::i64::<&str, NomError<_>>(&joined)
        .map_err(|_| Err::Error(NomError::from_error_kind(line, nom::error::ErrorKind::Digit)))
        .map(|(_, value)| (line, value))
}

fn compute_possibilites_from_delta(delta: i64, t: i64) -> i64 {
    match delta.cmp(&0) {
        Ordering::Greater => {
            let t1 = (t as f64 - (delta as f64).sqrt())  / 2.0;
            let t2 = (t as f64 + (delta as f64).sqrt())  / 2.0;
            (t2 - 1.0).ceil() as i64 - (t1 + 1.0).floor() as i64 + 1
        },
        Ordering::Less => 0,
        Ordering::Equal => (t%2 == 0) as i64
    }
}

fn read_file_and_compute_garden(file_path: &Path) -> Result<(), Error> {
    let lines = utils_io::line_iterator(file_path)?;
    let mut time: i64 = 0;
    let mut distance: i64 = 0;

    for (nline, line) in lines.enumerate() {
        let (_, numerals) = parse_race(&line?)
            .map_err(|err| Error::new(
                ErrorKind::InvalidData, 
                err.to_string() + &format!(" in line: {:}", nline)
            )
        )?;

        match nline {
            0 => time = numerals,
            1 => distance = numerals,
            _ => (), // maybe an error instead, but it would be too much for the current project
        }
    }

    let result: i64 = compute_possibilites_from_delta(time*time - 4*distance, time);
    println!("Product of possibilites: {:}", result);
    Ok(())
}

pub fn main() -> Result<(), Error> {
    let filename = env::current_dir()?.join("2023").join("data").join("input_day_six");
    read_file_and_compute_garden(&filename)?;
    Ok(())
}