           TESTSUITE 'BatchProcess Testsuite'

           TESTCASE 'Read a first two lines'
               OPEN INPUT TRANSACTIONS.
                   PERFORM ReadLine
               CLOSE TRANSACTIONS.
           EXPECT TRANSACTION TO BE
             "4711112345678900005000000Testüberweisung"
