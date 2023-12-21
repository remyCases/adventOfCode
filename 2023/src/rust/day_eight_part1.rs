// Copyright (C) 2023 Rémy Cases
// See LICENSE file for extended copyright information.
// This file is part of adventOfCode project from https://github.com/remyCases/adventOfCode.

use crate::utils_io;
use std::path::Path;
use std::io::{Error, ErrorKind};
use std::env;

use nom::*;
use nom::error::{Error as NomError, ParseError};
use std::cmp::Ordering;

const END: i32 = 26*26*26-1;
const NB_LETTERS: i32 = 26;
const MAX_ITERATION: usize = 10_000_000;

#[derive(Debug)]
struct Node {
    id: i32,
    left_id: i32,
    right_id: i32,
}

impl PartialEq for Node {
    fn eq(&self, other: &Self) -> bool {
        self.id == other.id
    }
}

impl Eq for Node { }

impl PartialOrd for Node {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        self.id.partial_cmp(&other.id)
    }
}

impl Ord for Node {
    fn cmp(&self, other: &Self) -> Ordering {
        self.id.cmp(&other.id)
    }
}

#[derive(Debug, Clone)]
enum Direction {
    R,
    L,
}

#[derive(Debug)]
struct Map {
    direction: Vec<Direction>,
    nodes: Vec<Node>,
}

fn parse_directions<'a>(line: &'a str, map: &'a mut Map) -> IResult<&'a str, &'a str>{

    let (_, vec_direction) = multi::many1(
        branch::alt((
            character::complete::char::<&str, NomError<_>>('R'), 
            character::complete::char('L'))
        )
    )(line)?;

    match vec_direction.iter()
        .map(|c| match c {
            'R' => Some(Direction::R),
            'L' => Some(Direction::L),
            _ => None,
        })
        .collect::<Option<Vec<_>>>() {
            Some(converted_vec) => map.direction = converted_vec,
            None => return Err(Err::Error(NomError::from_error_kind(line, nom::error::ErrorKind::Char)))
        };
    Ok((line, line))
}

fn parse_maps<'a>(line: &'a str, map: &'a mut Map) -> IResult<&'a str, &'a str>{
    
    let (_, (current, left, right)) = sequence::tuple((
        bytes::complete::take_while(|c| character::is_alphabetic(c as u8)),
        sequence::preceded(
            bytes::complete::tag::<&str, &str, NomError<_>>(" = ("),
            bytes::complete::take_while(|c| character::is_alphabetic(c as u8)),
        ),
        sequence::preceded(
            bytes::complete::tag(", "),
            bytes::complete::take_while(|c| character::is_alphabetic(c as u8)),
        )
    ))
    (line)?;

    let id = current.chars().fold(0i32, |acc, x| acc*NB_LETTERS + (x as i32) - ('A' as i32));
    let left_id = left.chars().fold(0i32, |acc, x| acc*NB_LETTERS + (x as i32) - ('A' as i32));
    let right_id = right.chars().fold(0i32, |acc, x| acc*NB_LETTERS + (x as i32) - ('A' as i32));

    map.nodes.push(Node {id, left_id, right_id });
    Ok((line, line))
}
 
fn read_file_and_compute_steps(file_path: &Path) -> Result<(), Error> {
    let lines = utils_io::line_iterator(file_path)?;
    let mut map = Map { direction: Vec::new(), nodes: Vec::new() };

    let mut flag_direction = true;

    for (nline, line) in lines.enumerate() {

        let binding_line = line?;
        if binding_line.is_empty() {
            flag_direction = false;
            continue;
        }

        let _ = if flag_direction {
            parse_directions(&binding_line, &mut map)
        }
        else {
            parse_maps(&binding_line, &mut map)
        }.map_err(|err| Error::new(
            ErrorKind::InvalidData, 
            err.to_string() + &format!(" in line: {:}", nline)
        ))?;
        
    }
    map.nodes.sort();
    let mut curr_id: i32 = 0;
    let mut curr_index: usize = 0;
    let mut step = 0usize;
    let mut directions = map.direction.iter().cloned().cycle();

    while curr_id != END && step < MAX_ITERATION {
        if let Some(d) = directions.next() {
            match d {
                Direction::R => curr_id = map.nodes[curr_index].right_id,
                Direction::L => curr_id = map.nodes[curr_index].left_id,
            };
            // since the vec is ordered, there is no need to use pointer to look for the next node
            // nevertheless a hashmap would have been quicker than find in an iter
            if let Some((n, _)) = map.nodes.iter().enumerate().find(|(_, i)| i.id == curr_id) {
                curr_index = n;
            } else {
                return Err(Error::new(ErrorKind::Other, format!("Cant find the index associated with {:}", curr_id)));
            }
            step += 1;
        } else {
            return Err(Error::new(ErrorKind::Other, "Invalid direction found"));
        }
    }
    println!("Total steps needed: {:}", step);

    Ok(())
}

pub fn main() -> Result<(), Error> {
    let filename = env::current_dir()?.join("2023").join("data").join("input_day_eight");
    read_file_and_compute_steps(&filename)?;
    Ok(())
}