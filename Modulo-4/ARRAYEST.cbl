      ******************************************************************
      * Author: ANDREI BATISTA
      * Date: 27/10/2023
      * Purpose: Mostrar comandos - OCCURS (ARRAY ESTÁTICO)
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. ARRAYEST.
      ******** HABILITANDO O PROGRAMA PARA CASAS DECIMAIS COM VIRGULA **
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
      ******************************************************************

       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       01 WS-FINANCIAMENTO.
          03 WS-CLIENTE                PIC X(20).
          03 WS-OBJETO                 PIC X(20).
          03 WS-VALOR                  PIC 9(06)V99.
          03 WS-PARCELAS               PIC $$.$$9,99 OCCURS 12 TIMES.
       01 WS-VARIAVEIS.
          03 WS-VR-PARCELAS            PIC 9(05)V99.
          03 WS-IND                    PIC 99.

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
            INITIALISE                 WS-VARIAVEIS
            DISPLAY 'Informe o nome do cliente: '
            ACCEPT WS-CLIENTE
            DISPLAY 'Informe o objeto financiado: '
            ACCEPT WS-OBJETO
            DISPLAY 'Informe o valor do objeto: '
            ACCEPT WS-VALOR

            COMPUTE WS-VR-PARCELAS = WS-VALOR / 12

            PERFORM UNTIL WS-IND EQUAL 12
               ADD 1             TO WS-IND
               MOVE WS-VR-PARCELAS TO WS-PARCELAS(WS-IND)
            END-PERFORM

            PERFORM VARYING WS-IND FROM 1 BY 1 UNTIL WS-IND > 12
               DISPLAY 'Parcela ' WS-IND ': ' WS-PARCELAS(WS-IND)
            END-PERFORM
            .
            GOBACK.
       END PROGRAM ARRAYEST.
