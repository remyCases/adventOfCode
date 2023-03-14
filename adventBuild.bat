@ECHO OFF
REM Copyright (C) 2023 Rémy Cases
REM See LICENSE file for extended copyright information.
REM This file is part of adventOfCode project from https://github.com/remyCases/adventOfCode.

IF %1 == nim GOTO NimBuild
IF %1 == cobol GOTO CobolBuild
IF %1 == rust GOTO RustBuild
GOTO End

:NimBuild
    nim c ./2022/src/nim/mainNim.nim
GOTO End

:CobolBuild
    cobc -m -o ./2022/lib/dayOne.dll -conf=./utils/default.conf ./2022/src/cobol/dayOne.cob
    cobc -m -o ./2022/lib/dayTwo.dll -conf=./utils/default.conf ./2022/src/cobol/dayTwo.cob
    cobc -m -o ./2022/lib/dayThree.dll -conf=./utils/default.conf ./2022/src/cobol/dayThree.cob
    cobc -m -o ./2022/lib/dayFour.dll -conf=./utils/default.conf ./2022/src/cobol/dayFour.cob
    cobc -m -o ./2022/lib/dayFive.dll -conf=./utils/default.conf ./2022/src/cobol/dayFive.cob
    cobc -x -free -o ./2022/bin/mainCob -conf=./utils/default.conf ./2022/src/cobol/mainCob.cob
GOTO End

:RustBuild
    cargo build --release --manifest-path .\2022\src\rust\Cargo.toml --target-dir .\2022\bin
GOTO End

:End