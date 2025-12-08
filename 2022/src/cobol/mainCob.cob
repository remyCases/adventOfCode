       >>SOURCE FORMAT FREE
       *> Copyright (C) 2023 Rémy Cases
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
       01 WSWordingOption PIC A(80).
       01 TMP PIC A(80).
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
       
           IF WSDay EQUAL TO 3 THEN
               CALL 'DAYTHREE' USING WSPart
               SET CorrectDay TO TRUE
           END-IF
       
           IF WSDay EQUAL TO 4 THEN
               CALL 'DAYFOUR' USING WSPart
               SET CorrectDay TO TRUE
           END-IF
           
           IF WSDay EQUAL TO 5 THEN
           *> I'm cheating here, since I'm not reading the input file
           *> If you have a different input file, you need to change TMP 
           *> to fit your input data
           *> Rule: first digit is the number of stacks, then each block of 
           *> char is the data to put in each stack.
               MOVE "9GTRW GCHPMSVW CLTSGM JHDMWRF PQLHSWFJ PJDNFMS ZBDFGCSJ RTB HNWLC" TO TMP
               STRING WSPart TMP
               INTO WSWordingOption
               CALL 'build/2022/lib/DAYFIVE' USING WSWordingOption
               SET CorrectDay TO TRUE
           END-IF
       
           IF IncorrectDay
               MOVE WSDay TO WSDayDisplay
               DISPLAY "Incorrect combination of day and part. Day " 
               FUNCTION TRIM(WSDayDisplay, LEADING) " and part " 
               FUNCTION TRIM(WSPart, LEADING) " does not exist (yet)." 
           END-IF 
           STOP RUN.
       