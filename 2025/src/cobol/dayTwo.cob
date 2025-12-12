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
       *> text parsing
       01 WSEOF PIC A(1).
       01 WSFileStatus PIC 99.
       01 WSFieldIndex PIC 999 VALUE 1.
       01 WSCurrentData PIC X(50).
       *> variables for the start of a range
       01 WSStart PIC 9(18).
       01 WSStartDiv PIC 9(18).
       01 WSStartZeros PIC 9(3).
       01 WSStartSize PIC 9(3).
       *> variables for the end of a range
       01 WSEnd PIC 9(18).
       01 WSEndZeros PIC 9(3).
       01 WSRangeEnd PIC 9(18).
       01 WSEndDiv PIC 9(18).
       *> variables for the invalid ids
       01 WSFoundIds.
           05 WSFoundCount PIC 9(5) COMP VALUE 0.
           05 WSFoundVals PIC 9(18) OCCURS 1000 TIMES.
       01 WSFound  PIC 9 COMP VALUE 0.
       01 WSInvalidId PIC 9(18).
       01 WSInvalidIds PIC 9(18).
       01 WSInvalidIdsDisplay PIC Z(17)9.
       *> variables for the divisors
       01 WSDivisorStr PIC X(18).
       01 WSDivisor PIC 9(18).
       01 WSDiv PIC 9(18).
       *> tmp variables
       01 Tmp PIC 9(18).
       *> counters
       01 I PIC 9(5) COMP VALUE 0.
       01 J PIC 9(5) COMP VALUE 0.

       LINKAGE SECTION.
       01 LPart PIC 9 VALUE 1.

       PROCEDURE DIVISION USING LPart.
       Main.
           MOVE 0 TO WSInvalidIds

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
                               ADD 1 TO WSFieldIndex
                           END-IF
                   END-READ
                END-PERFORM.
           CLOSE DataFile

           MOVE WSInvalidIds TO WSInvalidIdsDisplay
           DISPLAY "INVALID IDS: " FUNCTION TRIM(WSInvalidIdsDisplay)
           EXIT PROGRAM.

       ParseData.
           UNSTRING WSCurrentData DELIMITED BY '-'
           INTO  WSStart
                 WSEnd
           MOVE 0 TO WSStartZeros
           MOVE 0 TO WSEndZeros

           INSPECT WSStart TALLYING WSStartZeros FOR LEADING '0'
           INSPECT WSEnd TALLYING WSEndZeros FOR LEADING '0'

           PERFORM UNTIL WSStartZeros EQUAL TO WSEndZeros
               MOVE 0 TO WSRangeEnd
               MOVE 1 TO WSRangeEnd(WSStartZeros:)
               SUBTRACT 1 FROM WSRangeEnd

               IF LPart EQUAL TO 1 THEN
                   PERFORM FindRepeatedTwiceIds
               END-IF

               IF LPart EQUAL TO 2 THEN
                   PERFORM FindRepeatedIds
               END-IF

               MOVE 0 TO WSStart
               MOVE 1 TO WSStart(WSStartZeros:)
               SUBTRACT 1 FROM WSStartZeros
           END-PERFORM

           MOVE WSEnd TO WSRangeEnd

           IF LPart EQUAL TO 1 THEN
               PERFORM FindRepeatedTwiceIds
           END-IF.

           IF LPart EQUAL TO 2 THEN
               PERFORM FindRepeatedIds
           END-IF.

       FindRepeatedTwiceIds.
           COMPUTE Tmp = FUNCTION MOD(WSStartZeros, 2)
           IF Tmp NOT EQUAL TO 0
               NEXT SENTENCE
           END-IF

           COMPUTE Tmp = 9 + WSStartZeros / 2
           MOVE 0 TO WSDivisor
           MOVE 1 TO WSDivisor(Tmp:)
           ADD 1 TO WSDivisor

           COMPUTE Tmp = FUNCTION MOD(WSStart, WSDivisor)
           IF Tmp EQUAL TO 0
               COMPUTE WSStartDiv = WSStart / WSDivisor
           ELSE
               COMPUTE WSStartDiv = WSStart / WSDivisor + 1
           END-IF
           COMPUTE WSEndDiv = WSRangeEnd / WSDivisor

           IF WSEndDiv < WSStartDiv
               NEXT SENTENCE
           END-IF

           MOVE WSStartDiv TO I
           PERFORM UNTIL I GREATER THAN WSEndDiv
               COMPUTE WSInvalidId = I * WSDivisor
               PERFORM AddUniqueId
               ADD 1 TO I
           END-PERFORM.

       FindRepeatedIds.
           MOVE 2 TO WSDiv
           COMPUTE WSStartSize = 18 - WSStartZeros
           PERFORM UNTIL WSDiv GREATER THAN WSStartSize
               PERFORM FindRepeatedIdWithRepetition
               ADD 1 TO WSDiv
           END-PERFORM.

       FindRepeatedIdWithRepetition.
           COMPUTE Tmp = FUNCTION MOD(WSStartSize, WSDiv)
           IF Tmp NOT EQUAL TO 0
               NEXT SENTENCE
           END-IF
    
           MOVE WSDiv TO I
           MOVE ZEROES TO WSDivisorStr
           PERFORM UNTIL I EQUALS TO 0
               COMPUTE Tmp = 18 - WSStartSize / WSDiv * (I - 1)
               MOVE 1 TO WSDivisorStr(Tmp:1)
               SUBTRACT 1 FROM I
           END-PERFORM
           MOVE WSDivisorStr TO WSDivisor

           COMPUTE Tmp = FUNCTION MOD(WSStart, WSDivisor)
           IF Tmp EQUAL TO 0
               COMPUTE WSStartDiv = WSStart / WSDivisor
           ELSE
               COMPUTE WSStartDiv = WSStart / WSDivisor + 1
           END-IF
           COMPUTE WSEndDiv = WSRangeEnd / WSDivisor

           MOVE WSStartDiv TO I
           PERFORM UNTIL I GREATER THAN WSEndDiv
               COMPUTE WSInvalidId = I * WSDivisor
               PERFORM AddUniqueId
               ADD 1 TO I
           END-PERFORM.

       AddUniqueId.
           MOVE 0 TO WSFound
           MOVE 1 TO J

           PERFORM UNTIL J GREATER THAN WSFoundCount 
           OR WSFound EQUALS TO 1
               MOVE WSFoundVals(J) TO Tmp
               IF Tmp EQUALS TO WSInvalidId
                   MOVE 1 TO WSFound
               END-IF
               ADD 1 TO J
           END-PERFORM

           IF WSFound EQUALS TO 0
               ADD 1 TO WSFoundCount
               ADD WSInvalidId TO WSInvalidIds
               MOVE WSInvalidId TO WSFoundVals(WSFoundCount)
           END-IF.
