@ECHO OFF
REM Copyright (C) 2023 Rémy Cases
REM See LICENSE file for extended copyright information.
REM This file is part of adventOfCode project from https://github.com/remyCases/adventOfCode.

SET /A DAY = %2
SET /A PART = %3
IF %1 == nim GOTO NimLaunch
IF %1 == cobol GOTO CobolLaunch
IF %1 == rust GOTO RustLaunch
IF %1 == c99 GOTO C99Launch
GOTO End

:NimLaunch
    .\2022\bin\mainNim -d %DAY% -p %PART%
GOTO End

:CobolLaunch
    .\2022\bin\mainCob %DAY%%PART%
GOTO End

:RustLaunch
    .\2022\bin\release\main_rust --day %DAY% --part %PART%
GOTO End

:C99Launch
    .\2022\bin\mainC99 --day %DAY% --part %PART%
GOTO End

:End