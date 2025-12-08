       >>SOURCE FORMAT FREE
      *> Copyright (C) 2025 Rémy Cases
      *> See LICENSE file for extended copyright information.
      *> This file is part of adventOfCode project from https://github.com/remyCases/adventOfCode.

       IDENTIFICATION DIVISION.
       PROGRAM-ID. MAINCOB.
       AUTHOR. RémyCases

       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WSOption.
           02 WSDay PIC 99.
           02 WSPart PIC A.
       01 WSDayDisplay PIC Z9.
       01 WSParsedDay PIC X VALUE 'N'.
           88 CorrectDay VALUE 'Y'.
           88 IncorrectDay VALUE 'N'.

       PROCEDURE DIVISION.
       Main.
           ACCEPT WSOption FROM COMMAND-LINE
       
           IF WSDay EQUAL TO 1 THEN
               CALL 'DAYONE' USING WSPart
               SET CorrectDay TO TRUE
           END-IF

           IF WSDay EQUAL TO 2 THEN
               CALL 'DAYTWO' USING WSPart
               SET CorrectDay TO TRUE
           END-IF

           IF IncorrectDay
               MOVE WSDay TO WSDayDisplay
               DISPLAY "Incorrect combination of day and part. Day " 
               FUNCTION TRIM(WSDayDisplay) " and part " 
               WSPart " does not exist (yet)." 
           END-IF 
           STOP RUN.
