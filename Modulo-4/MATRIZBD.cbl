      ******************************************************************
      * Author: ANDREI BATISTA
      * Date: 28/10/2023
      * Purpose: Mostrar Matrizes - Bidimensional (não indexada)
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. MATRIZBD.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       01 WS-MATRIZ.
          03 WS-LINHAS            OCCURS 3 TIMES.
             05 WS-LINHAS         PIC X(06) VALUE 'LINHA'.
             05 WS-COLUNAS        OCCURS 5 TIMES.
               07 WS-CELULA       PIC X(06) VALUE 'CELULA'.
       77 WS-L                    PIC 99.
       77 WS-C                    PIC 99.


       PROCEDURE DIVISION.
       MAIN-PROCEDURE.

           PERFORM VARYING WS-L FROM 1 BY 1 UNTIL WS-L > 3
               PERFORM VARYING WS-C FROM 1 BY 1 UNTIL WS-C > 5
                   DISPLAY WS-CELULA(WS-L,WS-C) ' ' WS-L ':' WS-C
               END-PERFORM
           END-PERFORM

            .
            STOP RUN.
       END PROGRAM MATRIZBD.
