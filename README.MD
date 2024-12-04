# Advent of code, an adventure to code

*Sorry for the pun.*

This project is an attempt to solve the [advent of code](https://adventofcode.com/) challenge in various programming languages I'm learning.

Currently only a [Nim](https://nim-lang.org/), [Cobol](https://gnucobol.sourceforge.io/), [Rust](https://www.rust-lang.org/), [Zig](https://ziglang.org/) and C versions are available. Maybe other versions will emerged (eg: Asm, Haskell, ...), depending of my spare time and my willingness to test myself.

It should work for both Windows and Linux architectures.

## Solved challenges 2024 edition

|Days | Rust | C99 |
|-----|:-:|:-:|
| 1 |:heavy_check_mark:|:x:
| 2 |:wavy_dash:|:x:
| 3 |:x:|:x:
| 4 |:x:|:x:
| 5 |:x:|:x:
| 6 |:x:|:x:
| 7 |:x:|:x:
| 8 |:x:|:x:
| 9 |:x:|:x:
|10 |:x:|:x:
|11 |:x:|:x:
|12 |:x:|:x:
|13 |:x:|:x:
|14 |:x:|:x:
|15 |:x:|:x:
|16 |:x:|:x:
|17 |:x:|:x:
|18 |:x:|:x:
|19 |:x:|:x:
|20 |:x:|:x:
|21 |:x:|:x:
|22 |:x:|:x:
|23 |:x:|:x:
|24 |:x:|:x:
|25 |:x:|:x:

## Solved challenges 2023 edition

I've currently stopped working on them since the 2024 edition is here.

|Days | Nim | Rust | Zig | C99 |
|-----|:-:|:-:|:-:|:-:|
| 1 |:heavy_check_mark:|:heavy_check_mark:|:heavy_check_mark:|:heavy_check_mark:
| 2 |:heavy_check_mark:|:heavy_check_mark:|:x:|:heavy_check_mark:
| 3 |:heavy_check_mark:|:heavy_check_mark:|:x:|:x:
| 4 |:x:|:heavy_check_mark:|:x:|:x:
| 5 |:x:|:wavy_dash:|:x:|:x:
| 6 |:x:|:heavy_check_mark:|:x:|:x:
| 7 |:x:|:heavy_check_mark:|:x:|:x:
| 8 |:x:|:heavy_check_mark:|:x:|:x:
| 9 |:x:|:heavy_check_mark:|:x:|:x:
|10 |:x:|:wavy_dash:|:x:|:x:
|11 |:x:|:x:|:x:|:x:
|12 |:x:|:x:|:x:|:x:
|13 |:x:|:x:|:x:|:x:
|14 |:x:|:x:|:x:|:x:
|15 |:x:|:x:|:x:|:x:
|16 |:x:|:x:|:x:|:x:
|17 |:x:|:x:|:x:|:x:
|18 |:x:|:x:|:x:|:x:
|19 |:x:|:x:|:x:|:x:
|20 |:x:|:x:|:x:|:x:
|21 |:x:|:x:|:x:|:x:
|22 |:x:|:x:|:x:|:x:
|23 |:x:|:x:|:x:|:x:
|24 |:x:|:x:|:x:|:x:
|25 |:x:|:x:|:x:|:x:

## Solved challenges 2022 edition

I've currently stopped working on them since the 2023 edition is here.

|Days | C99 | GnuCobol | Nim | Rust |
|-----|:-:|:-----:|:---:|:----:|
| 1 |:heavy_check_mark:|:heavy_check_mark:|:heavy_check_mark:|:heavy_check_mark:
| 2 |:heavy_check_mark:|:heavy_check_mark:|:heavy_check_mark:|:heavy_check_mark:
| 3 |:heavy_check_mark:|:heavy_check_mark:|:heavy_check_mark:|:x:
| 4 |:heavy_check_mark:|:heavy_check_mark:|:heavy_check_mark:|:x:
| 5 |:x:|:heavy_check_mark:|:heavy_check_mark:|:x:
| 6 |:x:|:x:|:heavy_check_mark:|:x:
| 7 |:x:|:x:|:x:|:x:
| 8 |:x:|:x:|:x:|:x:
| 9 |:x:|:x:|:x:|:x:
|10 |:x:|:x:|:x:|:x:
|11 |:x:|:x:|:x:|:x:
|12 |:x:|:x:|:x:|:x:
|13 |:x:|:x:|:x:|:x:
|14 |:x:|:x:|:x:|:x:
|15 |:x:|:x:|:x:|:x:
|16 |:x:|:x:|:x:|:x:
|17 |:x:|:x:|:x:|:x:
|18 |:x:|:x:|:x:|:x:
|19 |:x:|:x:|:x:|:x:
|20 |:x:|:x:|:x:|:x:
|21 |:x:|:x:|:x:|:x:
|22 |:x:|:x:|:x:|:x:
|23 |:x:|:x:|:x:|:x:
|24 |:x:|:x:|:x:|:x:
|25 |:x:|:x:|:x:|:x:

## Compiler and tools

### Nim

You'll need the Nim compiler. You should find tutorials about installation on the [official website](https://nim-lang.org/).

### Rust

Same as Nim, compiler and cargo can be found on the official [website](https://www.rust-lang.org/).

### Zig

Same as Nim and Rust, compiler can be found on the official [website](https://ziglang.org/).

### Cobol

First you'll need the Cobol compiler. And depending on your plateform it can be a challenge to get there. I personnally installed GnuCobol via [msys2](https://packages.msys2.org/package/mingw-w64-x86_64-gnucobol?repo=mingw64) on windows. Good luck.

### C99

GCC is used in this project, on windows it can be installed through [msys2](https://packages.msys2.org/base/mingw-w64-gcc). [CMake](https://cmake.org/) is used for compilation and build.

### ASM

Asm is not really supported yet. It's more of a test currently.

### Make

I use a makefile to centralize compilation and build for every language *(yes even Rust)*.

### Lua

Some lua scripts are used during compilation and to run the project. Lua is then mandatory for this project, compiler can be found on the official [website](https://www.lua.org/).

## Building

Once everything is installed, to compile simplely run in the root folder: 
```
make build_all
```

or 
```
make build_xxx
```
where xxx is either `nim`, `rust`, `c99`, `zig` or `cob` if you only want to compile a specific language. More options are available for debugging or to use an other compiler (clang for instance).

## Running

To execute a specific part (PART) of a challenge (DAY) of a peculiar edition (YEAR) in a list of languages (XXX ...), run:
```shell
./adventLaunch.lua XXX ... -y YEAR -d DAY -p PART
```
or
```powershell
lua54 .\adventLaunch.lua XXX ... -y YEAR -d DAY -p PART
```

> Of course not every combinations exists *(yet)*

As far as I know, there are always two parts during each days, so `PART=1` will run the first part, and `PART=2` the second part.


If no language is given, the solution will run all available languages.
For instance `./adventLaunch.lua -y 2022 -d 2 -p 1` will run the first part of the second day of the 2022 edition written every languages (nim, rust, cobol, c99, zig and asm).

## Wait, that's not the right solutions concidering the data I have...

In the `YEAR/data` folder, there are the data used to solve each challenge. I'm using data I was given during the challenge, and I don't know if everyone was given the same (I expect not).