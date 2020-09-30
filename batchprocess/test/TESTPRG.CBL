       IDENTIFICATION DIVISION.
       PROGRAM-ID.  BASEM.
      *****************************************************************
      * Implementation des TIC TACT TOE Spiels.
      *****************************************************************
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
            COPY ZUTZCWS     .
       01 001-LINE PIC X(80).
       PROCEDURE DIVISION.
           PERFORM UT-INITIALIZE

           DISPLAY SPACE
           DISPLAY "TEST SUITE:"
           DISPLAY
           'BatchProcess Testsuite'
           DISPLAY SPACE

           MOVE 'Read a first line'
               TO UT-TEST-CASE-NAME
           PERFORM UT-BEFORE
               PERFORM ReadLine
           ADD 1 TO UT-TEST-CASE-COUNT
           SET UT-NORMAL-COMPARE TO TRUE
           MOVE 001-LINE TO UT-ACTUAL
           MOVE "4711112345678900005000000Testüberweisung"
                       TO UT-EXPECTED
           SET UT-COMPARE-DEFAULT TO TRUE
           PERFORM UT-ASSERT-EQUAL
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

           DISPLAY 'HELLO WORLD'
           STOP RUN

           .
       ReadLine SECTION.
           MOVE "4711112345678900005000000Testüberweisung" TO  001-LINE
           .
       EXIT.
