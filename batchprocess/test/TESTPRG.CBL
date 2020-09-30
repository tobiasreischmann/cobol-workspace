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
           05 TR-BANK-ID PIC 9(5).
           05 TR-ACCOUNT-ID PIC 9(10).
           05 TR-AMOUNT PIC Z(7)9V99.
           05 TR-COMMENT PIC X(53).
       01 FILE-STATUS.
           05 EOF PIC X(1).
           05 INPUT-FILE-STATUS PIC X(1).
       01 BANK occurs 200 times.
           02 BANK-ID PIC 9(5).
           02 ACCOUNT occurs 700 times.
               05 ACCOUNT-ID PIC 9(10).
               05 SALDO PIC 9(10).
           02 NUMBER-OF-ACCOUNTS PIC 999 VALUE 0.
       01 NUMBER-OF-BANKS PIC 999 VALUE 0.
       01 SEARCH-RESULT.
           05 BANK-INDEX PIC 999.
           05 ACCOUNT-INDEX PIC 999.
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
           MOVE 'Search for bank-id'
               TO UT-TEST-CASE-NAME
           PERFORM UT-BEFORE
                MOVE "12341" TO BANK-ID(1)
                MOVE "12342" TO BANK-ID(2)
                MOVE "12343" TO BANK-ID(3)
                MOVE "12344" TO BANK-ID(4)
                MOVE "12345" TO BANK-ID(5)
                MOVE 5 TO NUMBER-OF-BANKS
                MOVE "12343" TO TR-BANK-ID
                PERFORM SearchBank
           ADD 1 TO UT-TEST-CASE-COUNT
           SET UT-NORMAL-COMPARE TO TRUE
           MOVE BANK-INDEX TO UT-ACTUAL
           MOVE 003
                       TO UT-EXPECTED
           SET UT-COMPARE-DEFAULT TO TRUE
           PERFORM UT-ASSERT-EQUAL

           PERFORM UT-AFTER
           MOVE 'Create bank-id which does not exist'
               TO UT-TEST-CASE-NAME
           PERFORM UT-BEFORE
                MOVE "12341" TO BANK-ID(1)
                MOVE "12342" TO BANK-ID(2)
                MOVE "12343" TO BANK-ID(3)
                MOVE "12344" TO BANK-ID(4)
                MOVE "12345" TO BANK-ID(5)
                MOVE 5 TO NUMBER-OF-BANKS
                MOVE "12346" TO TR-BANK-ID
                PERFORM SearchBank
                DISPLAY BANK-ID(6)
           ADD 1 TO UT-TEST-CASE-COUNT
           SET UT-NORMAL-COMPARE TO TRUE
           MOVE BANK-ID(6) TO UT-ACTUAL
           MOVE "12346"
                       TO UT-EXPECTED
           SET UT-COMPARE-DEFAULT TO TRUE
           PERFORM UT-ASSERT-EQUAL

           PERFORM UT-AFTER
           MOVE 'Search for account-id'
               TO UT-TEST-CASE-NAME
           PERFORM UT-BEFORE
                MOVE 1 TO BANK-INDEX
                MOVE "12341" TO BANK-ID(BANK-INDEX)
                MOVE "1234567891" TO ACCOUNT-ID(BANK-INDEX,1)
                MOVE "1234567892" TO ACCOUNT-ID(BANK-INDEX,2)
                MOVE "1234567893" TO ACCOUNT-ID(BANK-INDEX,3)
                MOVE "1234567894" TO ACCOUNT-ID(BANK-INDEX,4)
                MOVE "1234567895" TO ACCOUNT-ID(BANK-INDEX,5)
                MOVE 1 TO NUMBER-OF-BANKS
                MOVE 5 TO NUMBER-OF-ACCOUNTS(BANK-INDEX)
                MOVE "1234567894" TO TR-ACCOUNT-ID
                PERFORM SearchAccount
           ADD 1 TO UT-TEST-CASE-COUNT
           SET UT-NORMAL-COMPARE TO TRUE
           MOVE ACCOUNT-INDEX TO UT-ACTUAL
           MOVE 004
                       TO UT-EXPECTED
           SET UT-COMPARE-DEFAULT TO TRUE
           PERFORM UT-ASSERT-EQUAL

           PERFORM UT-AFTER
           MOVE 'Create account-id that does not exist'
               TO UT-TEST-CASE-NAME
           PERFORM UT-BEFORE
                MOVE 5 TO BANK-INDEX
                MOVE "12341" TO BANK-ID(BANK-INDEX)
                MOVE "1234567891" TO ACCOUNT-ID(BANK-INDEX,1)
                MOVE "1234567892" TO ACCOUNT-ID(BANK-INDEX,2)
                MOVE "1234567893" TO ACCOUNT-ID(BANK-INDEX,3)
                MOVE "1234567894" TO ACCOUNT-ID(BANK-INDEX,4)
                MOVE "1234567895" TO ACCOUNT-ID(BANK-INDEX,5)
                MOVE 1 TO NUMBER-OF-BANKS
                MOVE 5 TO NUMBER-OF-ACCOUNTS(BANK-INDEX)
                MOVE "1234567896" TO TR-ACCOUNT-ID
                PERFORM SearchAccount
           ADD 1 TO UT-TEST-CASE-COUNT
           SET UT-NORMAL-COMPARE TO TRUE
           MOVE ACCOUNT-ID(BANK-INDEX,6) TO UT-ACTUAL
           MOVE "1234567896"
                       TO UT-EXPECTED
           SET UT-COMPARE-DEFAULT TO TRUE
           PERFORM UT-ASSERT-EQUAL

           PERFORM UT-AFTER
            COPY ZUTZCPD     .
           .
       UT-BEFORE.
               INITIALIZE BANK

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

           PERFORM ReadFile
           DISPLAY BANK(1)
           STOP RUN

           .
       SearchBank SECTION.
           MOVE 1 TO BANK-INDEX
           PERFORM UNTIL BANK-ID(BANK-INDEX) = TR-BANK-ID
              OR BANK-INDEX > NUMBER-OF-BANKS
               ADD 1 TO BANK-INDEX
           END-PERFORM
           IF BANK-INDEX > NUMBER-OF-BANKS THEN
               MOVE TR-BANK-ID TO BANK-ID(BANK-INDEX)
               ADD 1 TO NUMBER-OF-BANKS
           END-IF


           .
       SearchAccount SECTION.
           MOVE 1 TO ACCOUNT-INDEX
           PERFORM UNTIL ACCOUNT-ID(BANK-INDEX,ACCOUNT-INDEX)
              = TR-ACCOUNT-ID
              OR ACCOUNT-INDEX > NUMBER-OF-ACCOUNTS(BANK-INDEX)
               ADD 1 TO ACCOUNT-INDEX
           END-PERFORM
           IF ACCOUNT-INDEX > NUMBER-OF-ACCOUNTS(BANK-INDEX) THEN
               MOVE TR-ACCOUNT-ID TO
                   ACCOUNT-ID(BANK-INDEX,ACCOUNT-INDEX)
               ADD 1 TO NUMBER-OF-ACCOUNTS(BANK-INDEX)
           END-IF


           .
       ReadFile SECTION.
           OPEN INPUT TRANSACTIONS
               PERFORM ReadLine
               PERFORM SearchBank
               PERFORM SearchBank
               PERFORM SearchBank
               PERFORM SearchBank
               PERFORM SearchBank
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
