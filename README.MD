# Advent of code 2022 edition, an adventure to code

*Sorry for the pun.*

This project is an attempt to solve the [advent of code](https://adventofcode.com/) challenge in various programming languages I'm learning.

Currently only a [Nim](https://nim-lang.org/), [Cobol](https://gnucobol.sourceforge.io/) and [Rust](https://www.rust-lang.org/) versions are available. Maybe other version will emerged (eg: Zig, Haskell, ...), depending of my spare time and my willingness to test myself.

## Solved challenges

|Days | C99 | GnuCobol | Nim | Rust |
|-----|:-:|:-----:|:---:|:----:|
| 1 |:heavy_check_mark:|:heavy_check_mark:|:heavy_check_mark:|:heavy_check_mark:
| 2 |:heavy_check_mark:|:heavy_check_mark:|:heavy_check_mark:|:heavy_check_mark:
| 3 |:x:|:heavy_check_mark:|:heavy_check_mark:|:x:
| 4 |:x:|:heavy_check_mark:|:heavy_check_mark:|:x:
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

## How to run the Nim code

First you'll need the Nim compiler. You should find tutorials about installation on the [official website](https://nim-lang.org/).

Once it's done, to compile, simplely run in the root folder: 
```
adventBuild.dat nim
```

To execute:
```
adventLaunch.dat nim DAY PART
```

As far as I know, there is always two parts during each days, so `PART=1` will run the first part, and `PART=2` the second part.

## How to run the Cobol code

First you'll need the Cobol compiler. And depending on your plateform it can be a challenge to get there. I personnally installed GnuCobol via [msys2](https://packages.msys2.org/package/mingw-w64-x86_64-gnucobol?repo=mingw64). Good luck.

Once it's done, to compile, simplely run in the root folder: 
```
adventBuild.dat cobol
```

To execute:
```
adventLaunch.dat cobol DAY PART
```
For instance `.\adventLaunch.dat cobol 2 1` will run the first part of the second day.


## How to run the Rust code

Same as Nim, compiler and cargo can be found on the official [website](https://www.rust-lang.org/).

Then, to build:
```
adventBuild.dat rust
```

To execute:
```
adventLaunch.dat rust DAY PART
```

## How to run the C99 code

GCC is used in this project, on windows it can be installed through [msys2](https://packages.msys2.org/base/mingw-w64-gcc). [CMake](https://cmake.org/) is used for compilation and build.

Then, to build:
```
adventBuild.dat C99
```

To execute:
```
adventLaunch.dat C99 DAY PART
```

## Wait, that's not the right solutions concidering the data I have...

In the `2022/data` folder, there are the data used to solve each challenge. I'm using data I was given during the challenge, and I don't know if everyone was given the same (I expect not).