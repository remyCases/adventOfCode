// Copyright (C) 2023 Rémy Cases
// See LICENSE file for extended copyright information.
// This file is part of adventOfCode project from https://github.com/remyCases/adventOfCode.

use std::fs;
use std::path::Path;
use std::io::{Error, ErrorKind};
use std::env;
use aoc_utils::*;

const MAX_ITERATION: usize = 10_000_000;

const MIDDLE: usize = 0;
const NORTH: usize = 1;
const SOUTH: usize = 2;
const EAST: usize = 3;
const WEST: usize = 4;

#[derive(Debug, PartialEq)]
enum Status {
    Undetermined,
    Loop,
    Interior,
    Exterior,
    CRLF,
}

fn found_next_pipe_from_start(data: &[u8], start_index: usize, len_line: usize, len_data: usize) -> Result<(usize, usize), Error> {
    // if 'S' is against the wall (east or west), then start + 1 or start - 1 is a \n, so no need to check
    // let's try east
    if data[start_index + 1] == b'-' || data[start_index + 1] == b'J' || data[start_index + 1] == b'7'
        { Ok((start_index + 1, EAST)) }

    // let's try west
    else if data[start_index - 1] == b'-' || data[start_index - 1] == b'L' || data[start_index - 1] == b'F'
        { Ok((start_index - 1, WEST)) }

    // if 'S' is against the wall (north or south), a check should be made to need be OOB
    // let's try south
    else if start_index < len_data - len_line && 
        (data[start_index + len_line] == b'|' || data[start_index + len_line] == b'L' || data[start_index + len_line] == b'J')
        { Ok((start_index + len_line, SOUTH)) }

    // let's try north
    else if start_index >= len_line && 
        (data[start_index - len_line] == b'|' || data[start_index - len_line] == b'7' || data[start_index - len_line] == b'F')
        { Ok((start_index - len_line, NORTH)) }
    else 
        { Err(Error::new(ErrorKind::Other, "Cannot find the following char after start")) }
}

// wont check bounds, maybe I'll regret it later
fn compute_next_index_from_char(data: &[u8], current_index: usize, previous_index: usize, len_line: usize) -> io::Result<usize> {
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

fn pattern_status(c: u8) -> Result<[Status; 5], Error> {
    let mut basic_loop = [Status::Loop, Status::Undetermined, Status::Undetermined, Status::Undetermined, Status::Undetermined];

    match c {
        b'-' => {
            basic_loop[EAST] = Status::Loop;
            basic_loop[WEST] = Status::Loop;
            Ok(basic_loop)
        },
        b'|' => {
            basic_loop[NORTH] = Status::Loop;
            basic_loop[SOUTH] = Status::Loop;
            Ok(basic_loop)
        },
        b'7' => {
            basic_loop[WEST] = Status::Loop;
            basic_loop[SOUTH] = Status::Loop;
            Ok(basic_loop)
        },
        b'J' => {
            basic_loop[NORTH] = Status::Loop;
            basic_loop[WEST] = Status::Loop;
            Ok(basic_loop)
        },
        b'L' => {
            basic_loop[NORTH] = Status::Loop;
            basic_loop[EAST] = Status::Loop;
            Ok(basic_loop)
        },
        b'F' => {
            basic_loop[EAST] = Status::Loop;
            basic_loop[SOUTH] = Status::Loop;
            Ok(basic_loop)
        },
        _ => Err(Error::new(ErrorKind::Other, "Invalid char")), 
    }
}

fn find_neighboor(index: usize, direction: usize, len_line: usize) -> Result<[(usize, usize); 4], Error> {
    match direction {
        MIDDLE => Ok([(index, NORTH), (index, SOUTH), (index, EAST), (index, WEST)]),
        NORTH => Ok([(index-1, NORTH), (index+1, NORTH), (index, MIDDLE), (index - len_line, SOUTH)]),
        SOUTH => Ok([(index-1, SOUTH), (index+1, SOUTH), (index, MIDDLE), (index + len_line, NORTH)]),
        EAST => Ok([(index, MIDDLE), (index+len_line, EAST), (index-len_line, EAST), (index+1, WEST)]),
        WEST => Ok([(index, MIDDLE), (index+len_line, WEST), (index-len_line, WEST), (index-1, EAST)]),
        _ => Err(Error::new(ErrorKind::Other, "Incorrect direction in find neighboor"))
    }
}

fn read_file_and_compute_extrapolated_values(file_path: &Path) -> io::Result<()> {
    let lines = fs::read(file_path)?;
  
    // 5 because, middle + 4 directions
    let mut status_lines: Vec<[Status; 5]> = lines
        .iter()
        .map(|&c| 
            if c == 10 || c == 13 { [
                Status::CRLF, Status::CRLF, Status::CRLF,
                Status::CRLF, Status::CRLF] }
            else { [
                Status::Undetermined, Status::Undetermined, Status::Undetermined,
                Status::Undetermined, Status::Undetermined] })
        .collect();

    let len_line = lines
        .iter()
        .position(|&c | c == 10)// 10 = LF, if no LF and only CR, it will raise an error
        .ok_or(Error::new(ErrorKind::Other, "Cannot find a LF char"))? 
        + 1; // we dont remove the CRLF chars

    let start_index = lines
        .iter()
        .position(|&c| c == b'S')
        .ok_or(Error::new(ErrorKind::Other, "Not found the start character"))?;

    let (mut current_index, direction_from_start) = found_next_pipe_from_start(&lines, start_index, len_line, lines.len())?;
    status_lines[start_index][MIDDLE] = Status::Loop;
    status_lines[start_index][direction_from_start] = Status::Loop;

    // compute loop
    let mut previous_index = start_index;
    let mut step = 0;

    while current_index != start_index && step < MAX_ITERATION {
        status_lines[current_index] = pattern_status(lines[current_index])?;
        let next_index = compute_next_index_from_char(&lines, current_index, previous_index, len_line)?;
        
        previous_index = current_index;
        current_index = next_index;
        step += 1;
    }
    
    // compute Exterior
    let mut flag_no_modification = false;

    while !flag_no_modification {
        flag_no_modification = true;

        for (i, _) in lines.iter().enumerate() {
            for j in MIDDLE..=WEST {
                let neigh = find_neighboor(i, j, len_line)?;
                match status_lines[i][j] {
                    Status::Undetermined => {
                        if i < len_line || i >= lines.len() - len_line || i%len_line == 0 || i%len_line == len_line - 3 {
                            status_lines[i][j] = Status::Exterior;
                            flag_no_modification = false;
                        } 
                        else if neigh.iter().any(|(ii, jj)| status_lines[*ii][*jj] == Status::Exterior) {
                            status_lines[i][j] = Status::Exterior;
                            flag_no_modification = false;
                        }
                    },
                    _ => continue,
                };
            }
            
        }
    }
    
    // compute Interior
    let mut interior_count = 0;

    for (i, _) in lines.iter().enumerate() {
        match status_lines[i][MIDDLE] {
            Status::Undetermined => {
                status_lines[i][MIDDLE] = Status::Interior;
                interior_count += 1;
            },
            _ => continue,
        }; 
    }

    for (&c, s) in lines.iter().zip(status_lines) {
        if true {
            match s[MIDDLE] {
                Status::Loop | Status::CRLF => print!("{:}", c as char),
                Status::Interior => print!("I"),
                Status::Exterior => print!("O"),
                Status::Undetermined => print!("*"),
            };
        }
    }
    println!("");
    println!("Sum of interior: {:}", interior_count);
    Ok(())
}

pub fn main() -> io::Result<()> {
    let filename = env::current_dir()?.join("2023").join("data").join("input_day_ten");
    read_file_and_compute_extrapolated_values(&filename)?;
    Ok(())
}