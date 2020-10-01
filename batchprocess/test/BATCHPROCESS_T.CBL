           TESTSUITE 'BatchProcess Testsuite'

           BEFORE-EACH
               INITIALIZE BANK
           END-BEFORE


           TESTCASE 'Read a first two lines'
               OPEN INPUT TRANSACTIONS.
                   PERFORM ReadLine
               CLOSE TRANSACTIONS.
           EXPECT TRANSACTION TO BE
             "4711112345678900005000000Testüberweisung"

           TESTCASE 'Search for bank-id'
                MOVE "12341" TO BANK-ID(1)
                MOVE "12342" TO BANK-ID(2)
                MOVE "12343" TO BANK-ID(3)
                MOVE "12344" TO BANK-ID(4)
                MOVE "12345" TO BANK-ID(5)
                MOVE 5 TO NUMBER-OF-BANKS
                MOVE "12343" TO TR-BANK-ID
                PERFORM SearchBank
           EXPECT BANK-INDEX TO BE 003

           TESTCASE 'Create bank-id which does not exist'
                MOVE "12341" TO BANK-ID(1)
                MOVE "12342" TO BANK-ID(2)
                MOVE "12343" TO BANK-ID(3)
                MOVE "12344" TO BANK-ID(4)
                MOVE "12345" TO BANK-ID(5)
                MOVE 5 TO NUMBER-OF-BANKS
                MOVE "12346" TO TR-BANK-ID
                PERFORM SearchBank
                DISPLAY BANK-ID(6)
           EXPECT BANK-ID(6) TO BE "12346"

           TESTCASE 'Search for account-id'
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
           EXPECT ACCOUNT-INDEX TO BE 004

           TESTCASE 'Create account-id that does not exist'
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
           EXPECT ACCOUNT-ID(BANK-INDEX,6) TO BE "1234567896"

           TESTCASE 'Add amount to saldo'
                MOVE 1 TO BANK-INDEX
                MOVE 2 TO ACCOUNT-INDEX
                MOVE "12341" TO BANK-ID(BANK-INDEX)
                MOVE "1234567891" TO ACCOUNT-ID(BANK-INDEX,1)
                MOVE "1234567892" TO ACCOUNT-ID(BANK-INDEX,2)
                MOVE "1234567893" TO ACCOUNT-ID(BANK-INDEX,3)
                MOVE "12" TO SALDO(BANK-INDEX,ACCOUNT-INDEX)
                MOVE 1 TO NUMBER-OF-BANKS
                MOVE 3 TO NUMBER-OF-ACCOUNTS(BANK-INDEX)
                MOVE "30" TO TR-AMOUNT
                PERFORM AddToSaldo
           EXPECT SALDO(BANK-INDEX,ACCOUNT-INDEX) TO BE "0000000042"

