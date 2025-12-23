// Copyright (C) 2024 Rémy Cases
// See LICENSE file for extended copyright information.
// This file is part of adventOfCode project from https://github.com/remyCases/adventOfCode.

use std::path::Path;
use std::env;
use std::io::{Error, ErrorKind};
use std::collections::{HashMap, HashSet};
use nom::*;
use nom::error::Error as NomError;
use aoc_utils::*;

enum Updates {
    Rules((i32, i32)),
    Pages(Vec<i32>),
    None,
}

fn read_file_and_compute(file_path: &Path, _part: argparse::ArgPart) -> io::Result<()> {

    let mut rules: HashMap<i32, HashSet<i32>>= HashMap::new();
    let mut pages: Vec<Vec<i32>>= Vec::new();

    parse_lines!(
        file_path,
        branch::alt((
            combinator::map(
                sequence::separated_pair(
                    combinator::map_res(
                        character::complete::digit0::<_, NomError<_>>,
                        str::parse::<i32>
                    ), 
                    character::complete::char::<&str, NomError<_>>('|'),
                    combinator::map_res(
                        character::complete::digit0::<_, NomError<_>>,
                        str::parse::<i32>
                    )
                ),
                Updates::Rules
            ),
            combinator::map(
                multi::separated_list1(
                    character::complete::char(','),
                    combinator::map_res(
                        character::complete::digit0::<_, NomError<_>>,
                        str::parse::<i32>
                    )
                ),
                Updates::Pages
            ),
            combinator::map(
                character::complete::multispace0,
                |_x| Updates::None
            )
        )),
        |_, u| {
            match u {
                Updates::Rules((x, y)) => {
                    rules
                        .entry(x)
                        .and_modify(|v| {v.insert(y);})
                        .or_insert(HashSet::from([y]));
                },
                Updates::Pages(v) => {
                    pages.push(v);
                },
                _ => (),
            };
        }
    );

    let mut correct_pages = 0;
    'outer: for page in pages {
        for (n, u) in page.iter().enumerate() {
            if let Some(s) = rules.get(u) {
                if page[..n].iter().any(|v| s.contains(v)) { continue 'outer; }
            }
        }
        correct_pages += page[page.len()/2];
    }
    
    println!("UPDATES: {:#?}", correct_pages);
    Ok(())
}

main!("2024", "input_day_five");
