       IDENTIFICATION DIVISION.
       PROGRAM-ID.  BATCHPROCESS.
      *****************************************************************
      * Implementation des TIC TACT TOE Spiels.
      *****************************************************************
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT TRANSACTIONS ASSIGN TO
             "batchprocess/input.txt"
           ORGANIZATION IS LINE SEQUENTIAL
           FILE STATUS IS INPUT-FILE-STATUS.
       DATA DIVISION.
       FILE SECTION.
           FD TRANSACTIONS.
           01 TRANSACTION-F.
            05 BANK-ID-F PIC 9(5).
            05 ACCOUNT-ID-F PIC 9(10).
            05 AMOUNT-F PIC Z(7)9V99.
            05 COMMENT-F PIC X(53).
       WORKING-STORAGE SECTION.
            COPY ZUTZCWS     .
       01 001-LINE PIC X(80).
       01 TRANSACTION.
           05 BANK-ID PIC 9(5).
           05 ACCOUNT-ID PIC 9(10).
           05 AMOUNT PIC Z(7)9V99.
           05 COMMENT PIC X(53).
       01 FILE-STATUS.
           05 EOF PIC X(1).
           05 INPUT-FILE-STATUS PIC X(1).
       PROCEDURE DIVISION.
           PERFORM UT-INITIALIZE

           DISPLAY SPACE
           DISPLAY "TEST SUITE:"
           DISPLAY
           'BatchProcess Testsuite'
           DISPLAY SPACE

           MOVE 'Read a first two lines'
               TO UT-TEST-CASE-NAME
           PERFORM UT-BEFORE
               OPEN INPUT TRANSACTIONS
                   PERFORM ReadLine
               CLOSE TRANSACTIONS
           ADD 1 TO UT-TEST-CASE-COUNT
           SET UT-NORMAL-COMPARE TO TRUE
           MOVE TRANSACTION TO UT-ACTUAL
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
           ADD 1 TO UT-FILE-COUNT
           SET UT-FILE-IX TO UT-FILE-COUNT
           MOVE 'LECT' TO UT-INTERNAL-FILENAME(UT-FILE-IX)
           MOVE 'INPUT-FILE-STATUS' TO
                      UT-FILE-STATUS-FIELD-NAME(UT-FILE-IX)

           .
       UT-END.

           PERFORM ReadLine
           STOP RUN

           .
       ReadFile SECTION.
           OPEN INPUT TRANSACTIONS

           CLOSE TRANSACTIONS
           .
       ReadLine SECTION.
           READ TRANSACTIONS INTO TRANSACTION
             AT END MOVE "Y" TO EOF
           END-READ
           .
       EXIT.

           .
       END PROGRAM BATCHPROCESS.
