       >>SOURCE FORMAT FREE
      *> Copyright (C) 2025 Rémy Cases
      *> See LICENSE file for extended copyright information.
      *> This file is part of adventOfCode project from https://github.com/remyCases/adventOfCode.

       IDENTIFICATION DIVISION.
       PROGRAM-ID. DAYONE.
       AUTHOR. RémyCases


       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT DataFile ASSIGN TO "2025/data/input_day_one"
               ORGANIZATION IS LINE SEQUENTIAL
               ACCESS IS SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD DataFile.

       01 FDirection.
           02 FOrientation PIC X(1).
           02 FLength PIC 9(8).

       WORKING-STORAGE SECTION.

       01 WSOrientation PIC X(1).
       01 WSLength PIC 9(8).
       01 WSIncr PIC 9(8).
       01 WSCycles PIC 9(8).
       01 WSEOF PIC A(1).

       01 WSDial PIC 9(8) VALUE 50.
       01 WSPrevDial PIC 9(8).
       01 WSDialAtZero PIC 9(8) VALUE 0.
       01 WSDialAtZeroDisplay PIC Z(7)9.
       01 WSDialMax PIC 9(8) VALUE 100.

       LINKAGE SECTION.
       01 LPart PIC 9 VALUE 1.


       PROCEDURE DIVISION USING LPart.
       Main.
           OPEN INPUT DataFile.
               PERFORM UNTIL WSEOF='Y'
                   READ DataFile INTO FDirection
                       AT END MOVE 'Y' TO WSEOF
                       NOT AT END PERFORM ParseData
                   END-READ
               END-PERFORM
           CLOSE DataFile

           MOVE WSDialAtZero TO WSDialAtZeroDisplay
           DISPLAY "DIAL AT ZERO: " FUNCTION TRIM(WSDialAtZeroDisplay)
           EXIT PROGRAM.

       ParseData.
           MOVE FOrientation TO WSOrientation
           COMPUTE WSLength = FUNCTION NUMVAL(FLength)
           COMPUTE WSIncr = FUNCTION MOD(WSLength, WSDialMax)
           COMPUTE WSCycles = WSLength / WSDialMax
           
           IF LPart EQUAL TO 1 THEN
               PERFORM ComputeAtZero
           END-IF.

           IF LPart EQUAL TO 2 THEN
               PERFORM ComputePassingZero
           END-IF.

        ComputeAtZero.
           IF WSIncr EQUAL TO 0
               NEXT SENTENCE
           END-IF

           EVALUATE WSOrientation
               WHEN 'R'
                   COMPUTE WSDial = WSDial + WSIncr
                   COMPUTE WSDial = FUNCTION MOD(WSDial, WSDialMax)
               WHEN 'L'
                   COMPUTE WSDial = WSDial + WSDialMax - WSIncr
                   COMPUTE WSDial = FUNCTION MOD(WSDial, WSDialMax)
               WHEN OTHER
                   DISPLAY "Error parsing " FDirection
                   CLOSE DataFile
                   EXIT PROGRAM 
           END-EVALUATE

           IF WSDial EQUAL TO 0
               COMPUTE WSDialAtZero = WSDialAtZero + 1
           END-IF.
           
       ComputePassingZero.
           COMPUTE WSDialAtZero = WSDialAtZero + WSCycles

           IF WSIncr EQUAL TO 0
               NEXT SENTENCE
           END-IF

           EVALUATE WSOrientation
               WHEN 'R'
                   COMPUTE WSDial = WSDial + WSIncr
                   IF WSDial GREATER THAN OR EQUAL TO WSDialMax
                       COMPUTE WSDialAtZero = WSDialAtZero + 1
                   END-IF
                   COMPUTE WSDial = FUNCTION MOD(WSDial, WSDialMax)
               WHEN 'L'
                   MOVE WSDial TO WSPrevDial
                   COMPUTE WSDial = WSDial + WSDialMax - WSIncr
                   IF WSDial <= WSDialMax AND WSPrevDial NOT EQUAL TO 0
                       COMPUTE WSDialAtZero = WSDialAtZero + 1
                   END-IF
                   COMPUTE WSDial = FUNCTION MOD(WSDial, WSDialMax)
               WHEN OTHER
                   DISPLAY "Error parsing " FDirection
                   CLOSE DataFile
                   EXIT PROGRAM 
           END-EVALUATE.
