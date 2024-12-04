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
}