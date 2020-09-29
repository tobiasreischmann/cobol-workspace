           TESTSUITE 'Beispiel Testsuite'

           BEFORE-EACH
               INITIALIZE BAUM
           END-BEFORE

           TESTCASE 'Baum der Höhe 0 ist X'
               MOVE 0 TO HOEHE
               PERFORM Baue-Baum-Geruest
           EXPECT SPITZE TO BE "X"
           EXPECT ZWEIGE TO BE ""
           EXPECT STAMM  TO BE ""     
 

           TESTCASE "Baum der Höhe 1 ist X mit Stamm"
               MOVE 1 TO HOEHE
               PERFORM Baue-Baum-Geruest
           EXPECT SPITZE TO BE " X "
           EXPECT ZWEIGE TO BE ""
           EXPECT STAMM  TO BE "| |"

                            
           TESTCASE "Baum der Höhe 2 ist X mit Zweigen und Stamm"
               MOVE 2 TO HOEHE
               PERFORM Baue-Baum-Geruest
           EXPECT SPITZE TO BE " X "
           EXPECT ZWEIGE TO BE " A "
           EXPECT STAMM  TO BE "| |"  

           TESTCASE "Berechne Zweigebene 1 bei Baumhöhe 3"
               MOVE 3 TO HOEHE
               MOVE 1 TO ZWEIGEBENE
               PERFORM Baue-Zweige
           EXPECT ZWEIGE TO BE " / \ "

           TESTCASE "Berechne Zweigebene 1 bei Baumhöhe 4"
               MOVE 4 TO HOEHE
               MOVE 1 TO ZWEIGEBENE
               PERFORM Baue-Zweige
           EXPECT ZWEIGE TO BE "  / \  "

           TESTCASE "Berechne Zweigebene 2 bei Baumhöhe 4"
               MOVE 4 TO HOEHE               
               MOVE 2 TO ZWEIGEBENE
               PERFORM Baue-Zweige
           EXPECT ZWEIGE TO BE " /   \ "

           TESTCASE "Bastel Boden in Zweigebene 2 bei Baumhöhe 4"
               MOVE 4 TO HOEHE               
               MOVE 2 TO ZWEIGEBENE
               PERFORM Bastel-Boden
           EXPECT ZWEIGE TO BE "  ___  "

           TESTCASE "Ein Integrationstest von Bastel-Boden und Baue-Zweige"
               MOVE 4 TO HOEHE               
               MOVE 2 TO ZWEIGEBENE
               PERFORM Baue-Zweige
               PERFORM Bastel-Boden
           EXPECT ZWEIGE TO BE " /___\ "

           TESTCASE "Does not segfault for Höhe = 1"
               MOVE 1 TO HOEHE
               PERFORM Main
           EXPECT 1 TO BE 1