      ******************************************************************
      * Author: ANDREI BATISTA
      * Date: 28/10/2023
      * Purpose: MOSTRAR EXEMPLO DE LOOPING COM CONDICAO
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. LOOPCOND.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       77 WS-CONTAR            PIC 999.
       77 WS-CONDICAO          PIC 999.
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
            DISPLAY 'INFORME ATE QUANTO DESEJA CONTAR: '
            ACCEPT WS-CONDICAO

      *     PERFORM VARYING WS-CONTAR FROM 1 BY 2 UNTIL
            PERFORM WITH TEST AFTER UNTIL
                            WS-CONTAR EQUAL WS-CONDICAO
                    ADD 1   TO WS-CONTAR
                    DISPLAY WS-CONTAR
            END-PERFORM.

            STOP RUN.
       END PROGRAM LOOPCOND.
