       >>SOURCE FORMAT FREE
      *> Copyright (C) 2025 Rémy Cases
      *> See LICENSE file for extended copyright information.
      *> This file is part of adventOfCode project from https://github.com/remyCases/adventOfCode.

       IDENTIFICATION DIVISION.
       PROGRAM-ID. DAYTWO.
       AUTHOR. RémyCases


       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT DataFile ASSIGN TO "2025/data/input_day_two"
               ORGANIZATION IS RECORD BINARY SEQUENTIAL
               FILE STATUS IS WSFileStatus.

       DATA DIVISION.
       FILE SECTION.
       FD DataFile.
       01 FData PIC X(1).

       WORKING-STORAGE SECTION.
       01 WSEOF PIC A(1).
       01 WSFileStatus PIC 99.
       01 WSFieldIndex PIC 999 VALUE 1.
       01 WSCurrentData PIC X(50).
       01 WSData PIC X(50).
       01 WSStart PIC 9(18).
       01 WSEnd PIC 9(18).

       01 WSInvalidIds PIC 9(18).
       01 WSInvalidIdsDisplay PIC Z(17)9.

       LINKAGE SECTION.
       01 LPart PIC 9 VALUE 1.

       PROCEDURE DIVISION USING LPart.
       Main.
           OPEN INPUT DataFile.
               IF WSFileStatus NOT EQUAL TO 0
                   DISPLAY "Error " WSFileStatus ". Exiting program."
                   NEXT SENTENCE
               END-IF

               PERFORM UNTIL WSEOF='Y'
                   READ DataFile INTO FData
                       AT END
                           MOVE 'Y' TO WSEOF
                           IF WSFieldIndex GREATER THAN 1
                               PERFORM ParseData
                           END-IF
                       NOT AT END
                           IF FData EQUAL TO ','
                               PERFORM ParseData
                               MOVE SPACES TO WSCurrentData
                               MOVE 1 TO WSFieldIndex
                           ELSE
                               MOVE FData TO 
                               WSCurrentData(WSFieldIndex:1)
                               COMPUTE WSFieldIndex = WSFieldIndex + 1
                           END-IF
                   END-READ
                END-PERFORM.
           CLOSE DataFile

           MOVE WSInvalidIds TO WSInvalidIdsDisplay
           DISPLAY "DIAL AT ZERO: " FUNCTION TRIM(WSInvalidIdsDisplay)
           EXIT PROGRAM.

       ParseData.
           UNSTRING WSCurrentData DELIMITED BY '-'
           INTO  WSStart
                 WSEnd

           IF LPart EQUAL TO 1 THEN
               DISPLAY WSStart " ," WSEnd
           END-IF.

           IF LPart EQUAL TO 2 THEN
               DISPLAY WSStart " ," WSEnd
           END-IF.
