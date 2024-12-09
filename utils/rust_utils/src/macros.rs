// Copyright (C) 2024 Rémy Cases
// See LICENSE file for extended copyright information.
// This file is part of adventOfCode project from https://github.com/remyCases/adventOfCode.

#[macro_export]
macro_rules! zip {
    ($x: expr) => ($x);
    ($x: expr, $($y: expr), +) => (
        $x.iter().zip(
            zip!($($y), +)
        )
    )
}

#[macro_export]
macro_rules! zip_iter {
    ($x: expr) => ($x);
    ($x: expr, $($y: expr), +) => (
        $x.zip(
            zip_iter!($($y), +)
        )
    )
}

#[macro_export]
macro_rules! parse_compute {
    ($f: ident, $p: expr, $c: expr) => {
        let lines = io::line_iterator($f)?;
        for (nline, line) in lines.enumerate() {
            let binding = line?;
            let (_, r) = $p(&binding).map_err(|err|
                Error::new(
                    ErrorKind::InvalidData,
                    err.to_string() + &format!(" in line: {:}", nline)
                )
            )?;
            $c(r);
        }
    };

    ($f: ident, $pa: expr, $p: expr, $c1: expr, $c2: expr) => {
        let lines = io::line_iterator($f)?;
        for (nline, line) in lines.enumerate() {
            let binding = line?;
            let (_, r) = $p(&binding).map_err(|err|
                Error::new(
                    ErrorKind::InvalidData,
                    err.to_string() + &format!(" in line: {:}", nline)
                )
            )?;
            match $pa {
                argparse::ArgPart::PartOne => $c1(r),
                argparse::ArgPart::PartTwo => $c2(r),
            };
        }
    };
}

#[macro_export]
macro_rules! parse_lines {
    ($f: ident, $p: expr, $a: expr) => {
        let lines = io::line_iterator($f)?;
        for (nline, line) in lines.enumerate() {
            let binding = line?;
            let (_, r) = $p(&binding).map_err(|err|
                Error::new(
                    ErrorKind::InvalidData,
                    err.to_string() + &format!(" in line: {:}", nline)
                )
            )?;
            $a(nline, r)
        }
    };
}

#[macro_export]
macro_rules! main {
    ($y: expr, $f: expr) => {
        pub fn main(part: argparse::ArgPart) -> io::Result<()> {
            let filename = env::current_dir()?.join($y).join("data").join($f);
            read_file_and_compute(&filename, part)?;
            Ok(())
        }
    };
}
