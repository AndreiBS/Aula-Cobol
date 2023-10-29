      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. MAXIMINI.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-AUXILIARES.
           05  WS-MAXIMO   PIC 9(2)  VALUE 0.
           05  WS-MINIMO   PIC 9(2)  VALUE 99.
           05  WS-I        PIC 9(2)  VALUE 0.
           05  WS-TAB      VALUE "1915664112".
               10 WS-ELE   PIC 9(2) OCCURS 5 TIMES.

       PROCEDURE DIVISION.

            PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 5
             IF WS-ELE (WS-I) > WS-MAXIMO
             MOVE WS-ELE (WS-I) TO WS-MAXIMO
             END-IF
             IF WS-ELE (WS-I) < WS-MINIMO
             MOVE WS-ELE (WS-I) TO WS-MINIMO
             END-IF
            END-PERFORM.
            DISPLAY WS-TAB
            DISPLAY 'O numero mais alto e : ' WS-MAXIMO.
            DISPLAY 'O menor numero e     : ' WS-MINIMO.
            STOP RUN.
       END PROGRAM MAXIMINI.
