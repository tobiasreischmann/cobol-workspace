           TESTSUITE 'Beispiel Testsuite'

           TESTCASE 'Add 1 and 2'
               MOVE "1,2" TO INPUT-STRING
               PERFORM Calculate
           EXPECT RESULT TO BE 3

           TESTCASE 'Add nothing'
               MOVE "" TO INPUT-STRING
               PERFORM Calculate
           EXPECT RESULT TO BE 0

           TESTCASE 'Add 3 and 4'
               MOVE "3,4" TO INPUT-STRING
               PERFORM Calculate
           EXPECT RESULT TO BE 7

           TESTCASE 'Add 3,4,5,7'
               MOVE "3,4,5,7" TO INPUT-STRING
               PERFORM Calculate
           EXPECT RESULT TO BE 19

           TESTCASE 'Add 3,4,5,7,9,12,14'
               MOVE "3,4,5,7,9,12,14" TO INPUT-STRING
               PERFORM Calculate
           EXPECT RESULT TO BE 54