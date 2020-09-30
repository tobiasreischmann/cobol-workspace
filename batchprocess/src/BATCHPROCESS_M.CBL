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
           05 BANK-ID PIC 9(5).
           05 ACCOUNT-ID PIC 9(10).
           05 AMOUNT PIC Z(7)9V99.
           05 COMMENT PIC X(53).
       01 FILE-STATUS.
           05 EOF PIC X(1).
           05 INPUT-FILE-STATUS PIC X(1).
       PROCEDURE DIVISION.
           PERFORM ReadLine
           STOP RUN.

       ReadFile SECTION.
           OPEN INPUT TRANSACTIONS.
           
           CLOSE TRANSACTIONS.
       ReadLine SECTION.
           READ TRANSACTIONS INTO TRANSACTION
             AT END MOVE "Y" TO EOF
           END-READ
       EXIT.

       END PROGRAM BATCHPROCESS.
