// Copyright (C) 2023 Rémy Cases
// See LICENSE file for extended copyright information.
// This file is part of adventOfCode project from https://github.com/remyCases/adventOfCode.

use std::path::Path;
use std::fs::File;
use std::io::{BufRead, Lines, BufReader};
use std::env;
use anyhow::{Result};

fn line_iterator(file_path: &Path) -> Result<Lines<BufReader<File>>> {
    let file = File::open(file_path)?;
    Ok(BufReader::new(file).lines())
}

fn read_file_and_compute_calories(file_path: &Path, part: u8) -> Result<()> {
    let lines = line_iterator(file_path)?;
    let mut point = 0;
    const RESULT_FROM_MOVE: [usize; 9] = [
        3, 6, 0,
        0, 3, 6,
        6, 0, 3
    ];

    const RESULT_FROM_CHOICE: [usize; 9] = [
        3, 1, 2,
        1, 2, 3,
        2, 3, 1
    ];

    for line in lines {
        let ok_line = line?;
        let mut a = ok_line.split_whitespace().take(2);
        if let (Some(opponent_index_str), Some(your_index_str)) = (a.next(), a.next()) {
            let your_index = your_index_str.parse::<char>()? as usize - 88;
            let index = 3 * (opponent_index_str.parse::<char>()? as usize - 65) + your_index;

            match part {
                1 => point += RESULT_FROM_MOVE[index] + your_index + 1,
                2 => point += RESULT_FROM_CHOICE[index] + your_index * 3,
                _ => (),
            };
        };
    }

    println!("Score: {:?}", point);
    Ok(())
}

pub fn main(part: u8) -> Result<()> {
    let filename = env::current_dir()?.join("2022").join("data").join("input_day_two.txt");
    read_file_and_compute_calories(&filename, part)?;
    Ok(())
}