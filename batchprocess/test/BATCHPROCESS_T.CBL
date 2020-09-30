           TESTSUITE 'BatchProcess Testsuite'

           TESTCASE 'Read a first line'
               PERFORM ReadLine
           EXPECT  001-LINE TO BE "4711112345678900005000000Testüberweisung"