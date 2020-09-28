           TESTSUITE 'Testsuite'

           TESTCASE 'Test, ob BUZZ funktioniert'
               MOVE 5 to INPUT-FIELD
               PERFORM CALC-FIZZBUZZ
           EXPECT BUZZ TO BE TRUE

           TESTCASE 'Test, ob FIZZ funktioniert'
               MOVE 9 to INPUT-FIELD
               PERFORM CALC-FIZZBUZZ
           EXPECT FIZZ TO BE TRUE

           TESTCASE 'Test, ob FIZZBUZZ funktioniert'
               MOVE 15 to INPUT-FIELD
               PERFORM CALC-FIZZBUZZ
           EXPECT FIZZBUZZ TO BE TRUE

           TESTCASE 'Test, ob NONE funktioniert'
               MOVE 7 to INPUT-FIELD
               PERFORM CALC-FIZZBUZZ
           EXPECT NONE TO BE TRUE