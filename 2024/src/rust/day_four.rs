// Copyright (C) 2024 Rémy Cases
// See LICENSE file for extended copyright information.
// This file is part of adventOfCode project from https://github.com/remyCases/adventOfCode.

use std::path::Path;
use std::env;
use std::io::{Error, ErrorKind};
use nom::*;
use nom::error::Error as NomError;
use aoc_utils::*;

struct Matrix<'a, T> {
    data: &'a[T],
    nl: usize,
    nc: usize,
}

impl<'a, T> Matrix<'a, T> {
    fn get(self: &Self, x: usize, y: usize) -> Option<&T> {
        if x >= self.nl || y >= self.nc { None }
        else { Some(&self.data[x + y*self.nl]) }
    }
}

impl <'a, T>IntoIterator  for &'a Matrix<'a, T> {
    type Item = (usize, usize, &'a T);
    type IntoIter = MatrixIterator<'a, T>;

    fn into_iter(self) -> Self::IntoIter {
        MatrixIterator {
            data: self,
            x: 0,
            y: 0
        }
    }
}

struct MatrixIterator<'a, T> {
    data: &'a Matrix<'a, T>,
    x: usize,
    y: usize,
}

impl<'a, T> Iterator for MatrixIterator<'a, T> {
    type Item = (usize, usize, &'a T);
    fn next(&mut self) -> Option<Self::Item> {
        let res = 
            if let Some(v) = self.data.get(self.x, self.y) {
                Some((self.x, self.y, v))
            } else {
                None 
            };

        self.y += (self.x + 1) / self.data.nl;
        self.x = (self.x + 1) % self.data.nl;
        res
    }
}

fn check_xmas(ar: &Matrix<char>, x: usize, y: usize) -> i32 {
    let mut found = 0;
    // line
    if let (Some(&'M'), Some(&'A'), Some(&'S')) = (ar.get(x + 1, y), ar.get(x + 2, y), ar.get(x + 3, y)) {
        found += 1;
    }
    // reversed line
    if let (Some(&'M'), Some(&'A'), Some(&'S')) = (ar.get(x - 1, y), ar.get(x - 2, y), ar.get(x - 3, y)) {
        found += 1;
    }
    // column
    if let (Some(&'M'), Some(&'A'), Some(&'S')) = (ar.get(x, y + 1), ar.get(x, y + 2), ar.get(x, y + 3)) {
        found += 1;
    }
    // reversed column
    if let (Some(&'M'), Some(&'A'), Some(&'S')) = (ar.get(x, y - 1), ar.get(x, y - 2), ar.get(x, y - 3)) {
        found += 1;
    }
    // left diag
    if let (Some(&'M'), Some(&'A'), Some(&'S')) = (ar.get(x - 1, y + 1), ar.get(x - 2, y + 2), ar.get(x - 3, y + 3)) {
        found += 1;
    }
    // reversed left diag
    if let (Some(&'M'), Some(&'A'), Some(&'S')) = (ar.get(x - 1, y - 1), ar.get(x - 2, y - 2), ar.get(x - 3, y - 3)) {
        found += 1;
    }
    // right diag
    if let (Some(&'M'), Some(&'A'), Some(&'S')) = (ar.get(x + 1, y + 1), ar.get(x + 2, y + 2), ar.get(x + 3, y + 3)) {
        found += 1;
    }
    // reversed right diag
    if let (Some(&'M'), Some(&'A'), Some(&'S')) = (ar.get(x + 1, y - 1), ar.get(x + 2, y - 2), ar.get(x + 3, y - 3)) {
        found += 1;
    }
    found
}

fn check_x_mas(ar: &Matrix<char>, x: usize, y: usize) -> i32 {
    let mut found = 0;
    // M-S
    //  A
    // M-S
    if let (Some(&'M'), Some(&'S'), Some(&'M'), Some(&'S')) = (ar.get(x - 1, y - 1), ar.get(x + 1, y - 1), ar.get(x - 1, y + 1), ar.get(x + 1, y + 1)) {
        found += 1;
    }
    // M-M
    //  A
    // S-S
    if let (Some(&'M'), Some(&'M'), Some(&'S'), Some(&'S')) = (ar.get(x - 1, y - 1), ar.get(x + 1, y - 1), ar.get(x - 1, y + 1), ar.get(x + 1, y + 1)) {
        found += 1;
    }
    // S-M
    //  A
    // S-M
    if let (Some(&'S'), Some(&'M'), Some(&'S'), Some(&'M')) = (ar.get(x - 1, y - 1), ar.get(x + 1, y - 1), ar.get(x - 1, y + 1), ar.get(x + 1, y + 1)) {
        found += 1;
    }
    // S-S
    //  A
    // M-M
    if let (Some(&'S'), Some(&'S'), Some(&'M'), Some(&'M')) = (ar.get(x - 1, y - 1), ar.get(x + 1, y - 1), ar.get(x - 1, y + 1), ar.get(x + 1, y + 1)) {
        found += 1;
    }
    found
}

fn check_all_xmas(ar: &Matrix<char>) -> i32 {
    let mut found = 0;
    for (x, y, &i) in ar.into_iter() {
        if i == 'X' {
            found += check_xmas(ar, x, y);
        }
    }
    found
}

fn check_all_x_mas(ar: &Matrix<char>) -> i32 {
    let mut found = 0;
    for (x, y, &i) in ar.into_iter() {
        if i == 'A' {
            found += check_x_mas(ar, x, y);
        }
    }
    found
}

fn read_file_and_compute(file_path: &Path, part: argparse::ArgPart) -> io::Result<()> {

    let mut table= Vec::new();
    let mut size = 0;

    parse_lines!(
        file_path,
        multi::many0(character::complete::anychar::<&str, NomError<_>>),
        |n, x| {table.extend(x); size = n;}
    );

    let m = Matrix {data: &table, nl: size + 1, nc: size + 1};
    let res = match part {
        argparse::ArgPart::PartOne => check_all_xmas(&m),
        argparse::ArgPart::PartTwo => check_all_x_mas(&m),
    };

    println!("OCCURENCES: {:?}", res);
    Ok(())
}

main!("2024", "input_day_four");

#[cfg(test)]
mod day_two_four {
    use super::*;
    const DATA: Matrix<char> = Matrix {
        data: &[
            'M','M','M','S','X','X','M','A','S','M',
            'M','S','A','M','X','M','S','M','S','A',
            'A','M','X','S','X','M','A','A','M','M',
            'M','S','A','M','A','S','M','S','M','X',
            'X','M','A','S','A','M','X','A','M','M',
            'X','X','A','M','M','X','X','A','M','A',
            'S','M','S','M','S','A','S','X','S','S',
            'S','A','X','A','M','A','S','A','A','A',
            'M','A','M','M','M','X','M','M','M','M',
            'M','X','M','X','A','X','M','A','S','X',
        ], 
        nl: 10, 
        nc: 10
    };

    #[test]
    fn check_xmas_line() {
        assert_eq!(check_xmas(&DATA, 5, 0), 1);
    }

    #[test]
    fn check_xmas_line_reverse() {
        assert_eq!(check_xmas(&DATA, 4, 1), 1);
    }

    #[test]
    fn check_xmas_column_and_left_diagonal() {
        assert_eq!(check_xmas(&DATA, 9, 3), 2);
    }

    #[test]
    fn check_xmas_reversed_column_and_reversed_left_diagonal() {
        assert_eq!(check_xmas(&DATA, 9, 9), 2);
    }

    #[test]
    fn check_all_xmas_example() {
        assert_eq!(check_all_xmas(&DATA), 18);
    }

    #[test]
    fn check_x_mas_one_element() {
        assert_eq!(check_x_mas(&DATA, 2, 1), 1);
    }

    #[test]
    fn check_all_x_mas_example() {
        assert_eq!(check_all_x_mas(&DATA), 9);
    }
}
