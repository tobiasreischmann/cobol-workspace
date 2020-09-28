       IDENTIFICATION DIVISION.
       PROGRAM-ID. FIZZBUZZ.
       ENVIRONMENT DIVISION.
       DATA DIVISION.

       WORKING-STORAGE SECTION.
       01 INPUT-FIELD PIC 999.
       01 OUTPUT-FIELD PIC ZZ9.
       01 STATUS-FIZZBUZZ PIC X(8) VALUE SPACE.
         88 NONE            VALUE SPACE.
         88 FIZZ            VALUE 'FIZZ'.
         88 BUZZ            VALUE 'BUZZ'.
         88 FIZZBUZZ        VALUE 'FIZZBUZZ'.

       PROCEDURE DIVISION.
           PERFORM VARYING INPUT-FIELD FROM 1 by 1
             UNTIL INPUT-FIELD > 100
               PERFORM CALC-FIZZBUZZ
               PERFORM PRINT-FIZZBUZZ
           END-PERFORM
           GOBACK
           .

       CALC-FIZZBUZZ SECTION.
           INITIALIZE STATUS-FIZZBUZZ
           IF FUNCTION MOD(INPUT-FIELD,3) equal 0 THEN
               SET FIZZ TO TRUE
           END-IF
           IF FUNCTION MOD(INPUT-FIELD,5) equal 0 THEN
               IF FIZZ THEN
                   SET FIZZBUZZ TO TRUE
               ELSE
                   SET BUZZ TO TRUE    
               END-IF
           END-IF
       EXIT.
       PRINT-FIZZBUZZ SECTION.
           IF NONE THEN
               MOVE INPUT-FIELD TO OUTPUT-FIELD
               DISPLAY OUTPUT-FIELD
           ELSE
               DISPLAY STATUS-FIZZBUZZ
           END-IF
       EXIT.
      
       END PROGRAM FIZZBUZZ.
