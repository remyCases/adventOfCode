@ECHO OFF
SET /A DAY = %2
SET /A PART = %3
IF %1 == nim GOTO NimLaunch
IF %1 == cobol GOTO CobolLaunch
IF %1 == rust GOTO RustLaunch
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

:End