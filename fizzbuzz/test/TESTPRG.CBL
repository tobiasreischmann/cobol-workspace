       IDENTIFICATION DIVISION.
       PROGRAM-ID. FIZZBUZZ.
       ENVIRONMENT DIVISION.
       DATA DIVISION.

       WORKING-STORAGE SECTION.
            COPY ZUTZCWS     .
       01 INPUT-FIELD PIC 999.
       01 OUTPUT-FIELD PIC ZZ9.
       01 STATUS-FIZZBUZZ PIC X(8) VALUE SPACE.
         88 NONE            VALUE SPACE.
         88 FIZZ            VALUE 'FIZZ'.
         88 BUZZ            VALUE 'BUZZ'.
         88 FIZZBUZZ        VALUE 'FIZZBUZZ'.

       PROCEDURE DIVISION.
           PERFORM UT-INITIALIZE

           DISPLAY SPACE
           DISPLAY "TEST SUITE:"
           DISPLAY
           'Testsuite'
           DISPLAY SPACE

           MOVE 'Test, ob BUZZ funktioniert'
               TO UT-TEST-CASE-NAME
           PERFORM UT-BEFORE
               MOVE 5 to INPUT-FIELD
               PERFORM CALC-FIZZBUZZ
           ADD 1 TO UT-TEST-CASE-COUNT
           SET UT-NORMAL-COMPARE TO TRUE
           MOVE UT-TEST-CASE-COUNT TO UT-TEST-CASE-NUMBER
           SET UT-COMPARE-DEFAULT TO TRUE
           IF BUZZ
           MOVE  'TRUE'  TO UT-ACTUAL
           ELSE
           MOVE  'FALSE'  TO UT-ACTUAL
           END-IF
           MOVE 'TRUE' TO UT-EXPECTED
           PERFORM  UT-ASSERT-EQUAL
           PERFORM UT-AFTER
           MOVE 'Test, ob FIZZ funktioniert'
               TO UT-TEST-CASE-NAME
           PERFORM UT-BEFORE
               MOVE 9 to INPUT-FIELD
               PERFORM CALC-FIZZBUZZ
           ADD 1 TO UT-TEST-CASE-COUNT
           SET UT-NORMAL-COMPARE TO TRUE
           MOVE UT-TEST-CASE-COUNT TO UT-TEST-CASE-NUMBER
           SET UT-COMPARE-DEFAULT TO TRUE
           IF FIZZ
           MOVE  'TRUE'  TO UT-ACTUAL
           ELSE
           MOVE  'FALSE'  TO UT-ACTUAL
           END-IF
           MOVE 'TRUE' TO UT-EXPECTED
           PERFORM  UT-ASSERT-EQUAL
           PERFORM UT-AFTER
           MOVE 'Test, ob FIZZBUZZ funktioniert'
               TO UT-TEST-CASE-NAME
           PERFORM UT-BEFORE
               MOVE 15 to INPUT-FIELD
               PERFORM CALC-FIZZBUZZ
           ADD 1 TO UT-TEST-CASE-COUNT
           SET UT-NORMAL-COMPARE TO TRUE
           MOVE UT-TEST-CASE-COUNT TO UT-TEST-CASE-NUMBER
           SET UT-COMPARE-DEFAULT TO TRUE
           IF FIZZBUZZ
           MOVE  'TRUE'  TO UT-ACTUAL
           ELSE
           MOVE  'FALSE'  TO UT-ACTUAL
           END-IF
           MOVE 'TRUE' TO UT-EXPECTED
           PERFORM  UT-ASSERT-EQUAL
           PERFORM UT-AFTER
           MOVE 'Test, ob NONE funktioniert'
               TO UT-TEST-CASE-NAME
           PERFORM UT-BEFORE
               MOVE 7 to INPUT-FIELD
               PERFORM CALC-FIZZBUZZ
           ADD 1 TO UT-TEST-CASE-COUNT
           SET UT-NORMAL-COMPARE TO TRUE
           MOVE UT-TEST-CASE-COUNT TO UT-TEST-CASE-NUMBER
           SET UT-COMPARE-DEFAULT TO TRUE
           IF NONE
           MOVE  'TRUE'  TO UT-ACTUAL
           ELSE
           MOVE  'FALSE'  TO UT-ACTUAL
           END-IF
           MOVE 'TRUE' TO UT-EXPECTED
           PERFORM  UT-ASSERT-EQUAL
           PERFORM UT-AFTER
            COPY ZUTZCPD     .
           .
       UT-BEFORE.

           .
       UT-AFTER.

           .
       UT-INITIALIZE.
           MOVE SPACE TO UT-FILE-INFORMATION

           .
       UT-END.

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
           .
       EXIT.
           .
       PRINT-FIZZBUZZ SECTION.
           IF NONE THEN
               MOVE INPUT-FIELD TO OUTPUT-FIELD
               DISPLAY OUTPUT-FIELD
           ELSE
               DISPLAY STATUS-FIZZBUZZ
           END-IF
           .
       EXIT.

           .
       END PROGRAM FIZZBUZZ.
