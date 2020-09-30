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
           PERFORM ReadFile
           DISPLAY BANK(1)
           STOP RUN.

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
           OPEN INPUT TRANSACTIONS.
               PERFORM ReadLine
               PERFORM SearchBank
               PERFORM SearchBank
               PERFORM SearchBank
               PERFORM SearchBank
               PERFORM SearchBank
           CLOSE TRANSACTIONS.
       ReadLine SECTION.
           READ TRANSACTIONS INTO TRANSACTION
             AT END MOVE "Y" TO EOF
           END-READ
       EXIT.

       END PROGRAM BATCHPROCESS.
