>>SOURCE FORMAT FREE
IDENTIFICATION DIVISION.
PROGRAM-ID. MAINCOB.
AUTHOR. RémyCases

ENVIRONMENT DIVISION.
DATA DIVISION.
WORKING-STORAGE SECTION.
01 WS-OPTION.
       02 WS-DAY PIC 9.
       02 WS-PART PIC A.
01 WS-WORDING-OPTION PIC A(80).
01 TMP PIC A(80).

PROCEDURE DIVISION.
Main.
       ACCEPT WS-OPTION FROM COMMAND-LINE
       
       IF WS-DAY EQUAL TO 1 THEN
           CALL '2022/lib/DAYONE' USING WS-PART
       END-IF

       IF WS-DAY EQUAL TO 2 THEN
           CALL '2022/lib/DAYTWO' USING WS-PART
       END-IF

       IF WS-DAY EQUAL TO 3 THEN
           CALL '2022/lib/DAYTHREE' USING WS-PART
       END-IF

       IF WS-DAY EQUAL TO 4 THEN
           CALL '2022/lib/DAYFOUR' USING WS-PART
       END-IF
       
       IF WS-DAY EQUAL TO 5 THEN
       *> I'm cheating here, since I'm not reading the input file
       *> If you have a different input file, you need to change TMP 
       *> to fit your input data
       *> Rule: first digit is the number of stacks, then each block of 
       *> char is the data to put in each stack.
           MOVE "9GTRW GCHPMSVW CLTSGM JHDMWRF PQLHSWFJ PJDNFMS ZBDFGCSJ RTB HNWLC" TO TMP
           STRING WS-PART TMP
           INTO WS-WORDING-OPTION
           CALL '2022/lib/DAYFIVE' USING WS-WORDING-OPTION
       END-IF
       STOP RUN.
