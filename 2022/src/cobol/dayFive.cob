       >>SOURCE FORMAT FREE
*> Copyright (C) 2023 Rémy Cases
*> See LICENSE file for extended copyright information.
*> This file is part of adventOfCode project from https://github.com/remyCases/adventOfCode.

IDENTIFICATION DIVISION.
PROGRAM-ID. DAYFIVE.

ENVIRONMENT DIVISION.
INPUT-OUTPUT SECTION.
FILE-CONTROL.
       SELECT DATAFILE ASSIGN TO "2022/data/input_day_five" 
           ORGANIZATION IS LINE SEQUENTIAL 
           ACCESS IS SEQUENTIAL.

DATA DIVISION.
FILE SECTION.
FD DATAFILE.
01 F-DATA PIC X(50) VALUE ZEROES.

WORKING-STORAGE SECTION.
*> Data to store in a linked list
01 WS-DATA.
       05 WS-CHARDATA OCCURS 80 TIMES INDEXED BY I PIC X.
01 WS-DATALEN PIC 99 VALUE 0.

*> Action to perform on linked lists
01 WS-ACTION.
       05 WS-QUANTITY PIC 99.
       05 WS-SRC PIC 9.
       05 WS-DES PIC 9.

*> Variable to store size and operation of dynamic array
01 NBYTES-NODE PIC S9(9) BINARY.
01 NBYTES-ANCHOR-TABLE PIC S9(9) BINARY.
01 INCREMENT PIC S9(9) BINARY.

*> Generic pointer used during allocate
01 ADDRSS USAGE POINTER VALUE NULL.

*> Pointer of anchor of a linked-list
01 ANCHOR BASED USAGE POINTER.

*> Temporary pointers for operations on anchor without modifying 
*> ANCHOR value
01 ANCHOR-TMP USAGE POINTER.
01 ANCHOR-TMP2 USAGE POINTER.

*> Pointers for a list of ANCHOR
01 ANCHOR-TABLE USAGE POINTER VALUE NULL.
01 ANCHOR-TABLE-REF USAGE POINTER VALUE NULL.

*> Container to store the result of the challenge
01 WS-RESULT PIC X(50) VALUE " ".

*> DUMMY VARIABLE AND TEMPORARY ONES
01 UNSTRING_DUMPSTER PIC X(50) USAGE DISPLAY.
01 POINTER-FOR-UNSTRING PIC 9(3).         
01 TMP-CNT PIC 99 USAGE DISPLAY VALUE 0.
01 EOF PIC A.
01 INPUT-DATA PIC X USAGE DISPLAY.

LINKAGE SECTION.
01 NODE.
       05 NODE-DATA PIC X USAGE DISPLAY.
       05 NEXT-ITEM USAGE POINTER.
01 L-OPTION.
       05 L-PART PIC 9 VALUE 1.
       05 L-NLINKED PIC 9.
       05 L-INPUT PIC X(80).

*> Expected value sent in L-OPTION:
*> MOVE "13ZN MCD P" TO L-OPTION
*> MOVE "19GTRW GCHPMSVW CLTSGM JHDMWRF PQLHSWFJ PJDNFMS ZBDFGCSJ RTB HNWLC" TO L-OPTION
PROCEDURE DIVISION USING L-OPTION.
Main.
       *> Utilities
       MOVE LENGTH OF NODE TO NBYTES-NODE
       MOVE 1 TO POINTER-FOR-UNSTRING
       COMPUTE NBYTES-ANCHOR-TABLE = NBYTES-NODE * L-NLINKED
   
       *> Creation of all linkedstacks
       ALLOCATE NBYTES-ANCHOR-TABLE CHARACTERS RETURNING ADDRSS
       SET ANCHOR-TABLE TO ADDRSS
       SET ANCHOR-TABLE-REF TO ANCHOR-TABLE
       SET ADDRESS OF ANCHOR TO ANCHOR-TABLE

       PERFORM L-NLINKED TIMES
           MOVE ZEROES TO WS-DATALEN
           UNSTRING L-INPUT DELIMITED BY ALL ' ' INTO WS-DATA
           WITH POINTER POINTER-FOR-UNSTRING
           INSPECT WS-DATA TALLYING WS-DATALEN FOR CHARACTERS BEFORE ' '

           SET ADDRESS OF ANCHOR TO ANCHOR-TABLE
           PERFORM CreateLinkedStack
           PERFORM DisplayLinkedStack
           SET ANCHOR-TABLE UP BY NBYTES-NODE
       END-PERFORM
       
       *> Operations on linkedstacks
       OPEN INPUT DATAFILE.
           PERFORM UNTIL EOF='Y'
               READ DATAFILE INTO F-DATA
                   AT END MOVE 'Y' TO EOF
                   NOT AT END
                   MOVE F-DATA TO EOF
                   *> to skip header
                   IF EOF EQUALS 'm'
                       IF L-Part EQUALS TO 1
                           PERFORM SwitchElementEachLinkedStack
                       END-IF
                       IF L-Part EQUALS TO 2
                           PERFORM SwitchElementByBlockLinkedStack
                       END-IF
                   END-IF
               END-READ
           END-PERFORM
       CLOSE DATAFILE
       
       *> Display linkedstacks after all moving operation were done
       SET ANCHOR-TABLE TO ANCHOR-TABLE-REF
       PERFORM L-NLINKED TIMES
           SET ADDRESS OF ANCHOR TO ANCHOR-TABLE
           SET ADDRESS OF NODE TO ANCHOR
           STRING WS-RESULT NODE-DATA DELIMITED BY " "
           INTO WS-RESULT
           PERFORM DisplayLinkedStack
           SET ANCHOR-TABLE UP BY NBYTES-NODE
       END-PERFORM
       DISPLAY "TOP CRATE: " WS-RESULT

       *> Freeing all linkedstacks
       SET ANCHOR-TABLE TO ANCHOR-TABLE-REF
       PERFORM L-NLINKED TIMES
           SET ADDRESS OF ANCHOR TO ANCHOR-TABLE
           PERFORM FreeLinkedStack
           SET ANCHOR-TABLE UP BY NBYTES-NODE
       END-PERFORM

       SET ADDRSS TO ANCHOR-TMP
       FREE ADDRSS
       SET ADDRSS TO ANCHOR-TABLE
       FREE ADDRSS
       SET ADDRSS TO ANCHOR-TABLE-REF
       FREE ADDRSS.
       GOBACK.
       
CreateLinkedStack.
       MOVE ZEROES TO TMP-CNT
       SET ANCHOR TO NULL
       PERFORM WS-DATALEN TIMES
           ADD 1 TO TMP-CNT
           MOVE WS-CHARDATA(TMP-CNT) TO INPUT-DATA
           PERFORM PushLinkedStack
       END-PERFORM.

DisplayLinkedStack.
       SET ANCHOR-TMP TO ANCHOR
       PERFORM WITH TEST BEFORE UNTIL (ANCHOR-TMP = NULL)
           SET ADDRESS OF NODE TO ANCHOR-TMP
           SET ANCHOR-TMP TO NEXT-ITEM
       END-PERFORM.

PopLinkedStack.
       IF ANCHOR NOT EQUALS TO NULL
           SET ANCHOR-TMP TO ANCHOR
           SET ADDRESS OF NODE TO ANCHOR
           SET ANCHOR TO NEXT-ITEM
           SET ADDRESS OF NODE TO ANCHOR-TMP
           SET NEXT-ITEM TO NULL
       END-IF.

PushLinkedStack.
       ALLOCATE NBYTES-NODE CHARACTERS RETURNING ADDRSS
       SET ADDRESS OF NODE TO ADDRSS
       IF ANCHOR = NULL
           SET NEXT-ITEM TO NULL
       ELSE
           SET NEXT-ITEM TO ANCHOR
       END-IF
       SET ANCHOR TO ADDRSS
       MOVE INPUT-DATA TO NODE-DATA.

PushExistingLinkedStack.
       IF ANCHOR-TMP NOT EQUALS TO NULL
           SET ADDRESS OF NODE TO ANCHOR-TMP
           IF ANCHOR = NULL
               SET NEXT-ITEM TO NULL
           ELSE
               SET NEXT-ITEM TO ANCHOR
           END-IF
           SET ANCHOR TO ANCHOR-TMP
       END-IF.

CutLinkedStack.
       SET ANCHOR-TMP TO ANCHOR
       PERFORM TMP-CNT TIMES
           SET ADDRESS OF NODE TO ANCHOR
           SET ANCHOR TO NEXT-ITEM

           IF ANCHOR EQUALS TO NULL
               NEXT SENTENCE
           END-IF
       END-PERFORM
       SET NEXT-ITEM TO NULL.

GluLinkedStack.
       SET ANCHOR-TMP2 TO ANCHOR-TMP
       PERFORM WITH TEST BEFORE UNTIL (ANCHOR-TMP2 = NULL)
           SET ADDRESS OF NODE TO ANCHOR-TMP2

           IF NEXT-ITEM EQUALS TO NULL
               SET NEXT-ITEM TO ANCHOR
               SET ANCHOR TO ANCHOR-TMP
               NEXT SENTENCE
           END-IF

           SET ANCHOR-TMP2 TO NEXT-ITEM
       END-PERFORM.

SwitchElementEachLinkedStack.
       UNSTRING F-DATA DELIMITED BY ' '
       INTO 
           UNSTRING_DUMPSTER WS-QUANTITY 
           UNSTRING_DUMPSTER WS-SRC 
           UNSTRING_DUMPSTER WS-DES
       PERFORM WS-QUANTITY TIMES
           SET ANCHOR-TMP TO NULL

           SET ANCHOR-TABLE TO ANCHOR-TABLE-REF
           COMPUTE INCREMENT = NBYTES-NODE * (WS-SRC - 1)
           SET ANCHOR-TABLE UP BY INCREMENT
           SET ADDRESS OF ANCHOR TO ANCHOR-TABLE
       
           PERFORM PopLinkedStack
    
           SET ANCHOR-TABLE TO ANCHOR-TABLE-REF
           COMPUTE INCREMENT = NBYTES-NODE * (WS-DES - 1)
           SET ANCHOR-TABLE UP BY INCREMENT
           SET ADDRESS OF ANCHOR TO ANCHOR-TABLE
           
           PERFORM PushExistingLinkedStack
       END-PERFORM.

SwitchElementByBlockLinkedStack.
       UNSTRING F-DATA DELIMITED BY ' '
       INTO
           UNSTRING_DUMPSTER WS-QUANTITY
           UNSTRING_DUMPSTER WS-SRC
           UNSTRING_DUMPSTER WS-DES

       SET ANCHOR-TMP TO NULL

       SET ANCHOR-TABLE TO ANCHOR-TABLE-REF
       COMPUTE INCREMENT = NBYTES-NODE * (WS-SRC - 1)
       SET ANCHOR-TABLE UP BY INCREMENT
       SET ADDRESS OF ANCHOR TO ANCHOR-TABLE

       MOVE WS-QUANTITY TO TMP-CNT
       PERFORM CutLinkedStack

       SET ANCHOR-TABLE TO ANCHOR-TABLE-REF
       COMPUTE INCREMENT = NBYTES-NODE * (WS-DES - 1)
       SET ANCHOR-TABLE UP BY INCREMENT
       SET ADDRESS OF ANCHOR TO ANCHOR-TABLE

       PERFORM GluLinkedStack.

FreeLinkedStack.
       PERFORM WITH TEST BEFORE UNTIL (ANCHOR = NULL)
           SET ADDRESS OF NODE TO ANCHOR
           SET ADDRSS TO ANCHOR
           SET ANCHOR TO NEXT-ITEM
           FREE ADDRSS
       END-PERFORM.
