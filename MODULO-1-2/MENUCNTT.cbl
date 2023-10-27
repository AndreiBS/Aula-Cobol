      ******************************************************************
      * Author: ANDREI BATISTA
      * Date: 17/10/2023
      * Purpose: MENU PARA CADASTRO DE CONTATOS
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. MENUCNTT.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       01 WS-COM-AREA.
          03 WS-MENSAGEM                   PIC X(40).
       77 WS-OPCAO                         PIC X.

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
            PERFORM P300-PROCESSA     THRU P300-FIM
                                           UNTIL WS-OPCAO = 'F'OR 'f'
            PERFORM P900-FIM
            .
       P300-PROCESSA.
            MOVE SPACES                    TO WS-OPCAO
            DISPLAY '**************************************************'
            DISPLAY '***           SISTEMA DE CONTATOS              ***'
            DISPLAY '**************************************************'
            DISPLAY '*** Escolha uma opcao:                         ***'
            DISPLAY '***                                            ***'
            DISPLAY '*** 1 - Incluir Contato                        ***'
            DISPLAY '*** 2 - Listar Contato                         ***'
            DISPLAY '*** 3 - Consultar Contato                      ***'
            DISPLAY '*** 4 - Alterar Contato                        ***'
            DISPLAY '*** 5 - Excluir Contato                        ***'
            DISPLAY '***          <TECLE F PARA FINALIZAR>          ***'
            DISPLAY '**************************************************'

            ACCEPT WS-OPCAO

            EVALUATE WS-OPCAO
               WHEN '1'
                   CALL 'C:\COBOL\bin\SISCONTT' USING WS-COM-AREA
               WHEN '2'
                   CALL 'C:\COBOL\bin\VERLISTA' USING WS-COM-AREA
               WHEN '3'
                   CALL 'C:\COBOL\bin\CONSCTT'  USING WS-COM-AREA
               WHEN '4'
                   CALL 'C:\COBOL\bin\ALTECOTT' USING WS-COM-AREA
               WHEN '5'
                   CALL 'C:\COBOL\bin\DELCONTT' USING WS-COM-AREA
               WHEN 'F'
                   DISPLAY 'Obrigado, volte sempre!'
               WHEN 'f'
                   DISPLAY 'Obrigado, volte sempre!'
               WHEN OTHER
                   DISPLAY 'OPCAO INVALIDA! '
            .
       P300-FIM.
       P900-FIM.

            STOP RUN.
       END PROGRAM MENUCNTT.
