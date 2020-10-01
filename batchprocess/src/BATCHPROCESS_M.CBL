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
           SELECT ACCOUNT-DATA ASSIGN TO
             "batchprocess/output.txt"
           ORGANIZATION IS LINE SEQUENTIAL
           FILE STATUS IS OUTPUT-FILE-STATUS.
       DATA DIVISION.
       FILE SECTION.
           FD TRANSACTIONS.
           01 TRANSACTION-F.
            05 BANK-ID-F PIC 9(5).
            05 ACCOUNT-ID-F PIC 9(10).
            05 AMOUNT-F PIC 9(10).
            05 COMMENT-F PIC X(53).
           FD ACCOUNT-DATA.
           01 ACCOUNT-DATA-F.
            05 OUTPUT-LINE PIC X(36).
       WORKING-STORAGE SECTION.
       01 001-LINE PIC X(80).
       01 TRANSACTION.
           05 TR-BANK-ID PIC 9(5).
           05 TR-ACCOUNT-ID PIC 9(10).
           05 TR-AMOUNT PIC 9(10).
           05 TR-COMMENT PIC X(53).
       01 FILE-STATUS.
           05 EOF PIC X(1).
           05 INPUT-FILE-STATUS PIC X(1).
           05 OUTPUT-FILE-STATUS PIC X(1).
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
       01 TEMP-VARS.
           05 FORMATTED-SALDO PIC Z(07)9.99.
           05 BANK-COUNTER PIC 999.
           05 ACCOUNT-COUNTER PIC 999.
           05 BANK-SUM PIC 9(10).
           05 OUTPUT-BUFFER PIC X(36).
       PROCEDURE DIVISION.
           PERFORM ReadFile

           PERFORM OutputFile
           
           STOP RUN.
       GenerateAccountLine SECTION.
           INITIALIZE OUTPUT-BUFFER
           COMPUTE FORMATTED-SALDO = SALDO(BANK-COUNTER,ACCOUNT-COUNTER)
                / 100
           MOVE function concatenate('Account ',
               ACCOUNT-ID(BANK-COUNTER,ACCOUNT-COUNTER),
               ':     ', FORMATTED-SALDO)
               TO OUTPUT-BUFFER
           .
       GenerateTotalLine SECTION.
           INITIALIZE OUTPUT-BUFFER
           COMPUTE FORMATTED-SALDO = BANK-SUM / 100
           MOVE function concatenate('Total EUR for ',
               BANK-ID(BANK-COUNTER), ':    ', FORMATTED-SALDO)
               TO OUTPUT-BUFFER
           .
       GenerateEmptyLine SECTION.
           INITIALIZE OUTPUT-BUFFER
           .
       GenerateSeparatorLine SECTION.
           INITIALIZE OUTPUT-BUFFER
           MOVE '------------------------------------' TO OUTPUT-BUFFER
           .

       OutputLine SECTION.
           MOVE OUTPUT-BUFFER TO ACCOUNT-DATA-F
           WRITE ACCOUNT-DATA-F FROM ACCOUNT-DATA-F
           .

       OutputFile SECTION.
           OPEN OUTPUT ACCOUNT-DATA
           PERFORM VARYING BANK-COUNTER from 1 by 1 UNTIL
               BANK-COUNTER > NUMBER-OF-BANKS
               PERFORM PrintBank
           END-PERFORM
           CLOSE ACCOUNT-DATA
           .   
       PrintBank SECTION.
           INITIALIZE BANK-SUM
           PERFORM VARYING ACCOUNT-COUNTER from 1 by 1 UNTIL
               ACCOUNT-COUNTER > NUMBER-OF-ACCOUNTS(BANK-COUNTER)
               ADD SALDO(BANK-COUNTER,ACCOUNT-COUNTER) TO BANK-SUM
               PERFORM GenerateAccountLine
               PERFORM OutputLine
           END-PERFORM
           PERFORM GenerateSeparatorLine
           PERFORM OutputLine
           PERFORM GenerateTotalLine
           PERFORM OutputLine
           PERFORM GenerateEmptyLine
           PERFORM OutputLine
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

       AddToSaldo SECTION.
           ADD TR-AMOUNT TO SALDO(BANK-INDEX,ACCOUNT-INDEX)
           .
       
       ReadFile SECTION.
           OPEN INPUT TRANSACTIONS.
               PERFORM UNTIL EOF =  "Y"
                   PERFORM ReadLine
                   IF NOT EOF = "Y" THEN
                       PERFORM SearchBank
                       PERFORM SearchAccount
                       PERFORM AddToSaldo
                   END-IF
               END-PERFORM
           CLOSE TRANSACTIONS.
       ReadLine SECTION.
           READ TRANSACTIONS INTO TRANSACTION
             AT END MOVE "Y" TO EOF
           END-READ
       EXIT.

       END PROGRAM BATCHPROCESS.
