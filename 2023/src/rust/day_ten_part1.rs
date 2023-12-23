// Copyright (C) 2023 Rémy Cases
// See LICENSE file for extended copyright information.
// This file is part of adventOfCode project from https://github.com/remyCases/adventOfCode.

use std::fs;
use std::path::Path;
use std::io::{Error, ErrorKind};
use std::env;

const MAX_ITERATION: usize = 10_000_000;

fn found_next_pipe_from_start(data: &[u8], start_index: usize, len_line: usize, len_data: usize) -> Result<usize, Error> {
    // if 'S' is against the wall (east or west), then start + 1 or start - 1 is a \n, so no need to check
    // let's try east
    if data[start_index + 1] == b'-' || data[start_index + 1] == b'J' || data[start_index + 1] == b'7'
        { Ok(start_index + 1) }

    // let's try west
    else if data[start_index - 1] == b'-' || data[start_index - 1] == b'L' || data[start_index - 1] == b'F'
        { Ok(start_index - 1) }

    // if 'S' is against the wall (north or south), a check should be made to need be OOB
    // let's try south
    else if start_index < len_data - len_line && 
        (data[start_index + len_line] == b'|' || data[start_index + len_line] == b'L' || data[start_index + len_line] == b'J')
        { Ok(start_index + len_line) }

    // let's try north
    else if start_index >= len_line && 
        (data[start_index - len_line] == b'|' || data[start_index - len_line] == b'7' || data[start_index - len_line] == b'F')
        { Ok(start_index - len_line) }
    else 
        { Err(Error::new(ErrorKind::Other, "Cannot find the following char after start")) }
}

// wont check bounds, maybe I'll regret it later
fn compute_next_index_from_char(data: &[u8], current_index: usize, previous_index: usize, len_line: usize) -> Result<usize, Error> {
    match data[current_index] {
        // west or east
        b'-' => Ok(
            if previous_index == current_index - 1 { current_index + 1 }
            else { current_index - 1 }
        ),
        // south or north
        b'|' => Ok(
            if current_index >= len_line && previous_index == current_index - len_line { current_index + len_line }
            else { current_index - len_line }
        ),
        // south or east
        b'F' => Ok(
            if previous_index == current_index + 1 { current_index + len_line }
            else { current_index + 1 }
        ),
        // south or west
        b'7' => Ok(
            if previous_index == current_index - 1 { current_index + len_line }
            else { current_index - 1 }
        ),
        // north or west
        b'J' => Ok(
            if previous_index == current_index - 1 { current_index - len_line }
            else { current_index - 1 }
        ),
        // north or east
        b'L' => Ok(
            if previous_index == current_index + 1 { current_index - len_line }
            else { current_index + 1 }
        ),
        _ => Err(Error::new(ErrorKind::Other, "Invalid char")),
    }
}

fn read_file_and_compute_extrapolated_values(file_path: &Path) -> Result<(), Error> {
    let lines = fs::read(file_path)?;
    let len_line = lines
        .iter()
        .position(|&c | c == 10)// 10 = LF, if no LF and only CR, it will raise an error
        .ok_or(Error::new(ErrorKind::Other, "Cannot find a LF char"))? 
        + 1; // we dont remove the CRLF chars

    let start_index = lines
        .iter()
        .position(|&c| c == b'S')
        .ok_or(Error::new(ErrorKind::Other, "Not found the start character"))?;

    let mut current_index = found_next_pipe_from_start(&lines, start_index, len_line, lines.len())?;
    let mut previous_index = start_index;
    let mut step = 0;

    while current_index != start_index && step < MAX_ITERATION {
        let next_index = compute_next_index_from_char(&lines, current_index, previous_index, len_line)?;
        
        previous_index = current_index;
        current_index = next_index;
        step += 1;
    }
    // + 1 because we start one step after S and stop one before
    println!("Steps for Start to farthest point: {:}", step/2 + 1);
    Ok(())
}

pub fn main() -> Result<(), Error> {
    let filename = env::current_dir()?.join("2023").join("data").join("input_day_ten");
    read_file_and_compute_extrapolated_values(&filename)?;
    Ok(())
}