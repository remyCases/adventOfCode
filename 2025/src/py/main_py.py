# Copyright (C) 2025 Rémy Cases
# See LICENSE file for extended copyright information.
# This file is part of adventOfCode project from https://github.com/remyCases/adventOfCode.

import argparse

import day_one

def main(day: int, part: int) -> None:
    if day == 1:
        day_one.main(part)
    else:
        print(f"Incorrect combination of day and part. Day {day} and part {part} does not exist (yet).")

if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument("-d","--day", type=int, required=True)
    parser.add_argument("-p","--part", type=int, required=True)
    args = parser.parse_args()
    main(args.day, args.part)
