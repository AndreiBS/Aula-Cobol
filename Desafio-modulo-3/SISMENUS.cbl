      ******************************************************************
      * Author: ANDREI BATISTA
      * Date: 25/10/2023
      * Purpose: MANU COM AS OPCOES DE CADASTRO ETC DOS ALUNOS
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. SISMENUS.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       01 WS-COM-AREA.
          03 WS-MENSAGEM                   PIC X(50).
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
            DISPLAY '***           SISTEMA DE ALUNOS                ***'
            DISPLAY '**************************************************'
            DISPLAY '*** Escolha uma opcao:                         ***'
            DISPLAY '***                                            ***'
            DISPLAY '*** 1 - Incluir Alunos                         ***'
            DISPLAY '*** 2 - Listar Alunos                          ***'
            DISPLAY '*** 3 - Consultar Alunos                       ***'
            DISPLAY '*** 4 - Alterar Alunos                         ***'
            DISPLAY '*** 5 - Excluir Alunos                         ***'
            DISPLAY '***          <TECLE F PARA FINALIZAR>          ***'
            DISPLAY '**************************************************'

            ACCEPT WS-OPCAO

            EVALUATE WS-OPCAO
               WHEN '1'
                   CALL 'C:\COBOL\Desafio-modulo-3\bin\SISCADAS'
                           USING WS-COM-AREA
               WHEN '2'
                   CALL 'C:\COBOL\Desafio-modulo-3\bin\SISLISTA'
                           USING WS-COM-AREA
               WHEN '3'
                   CALL 'C:\COBOL\Desafio-modulo-3\bin\SISCONSU'
                           USING WS-COM-AREA
               WHEN '4'
                   CALL 'C:\COBOL\Desafio-modulo-3\bin\SISALTER'
                           USING WS-COM-AREA
               WHEN '5'
                   CALL 'C:\COBOL\Desafio-modulo-3\bin\SISDELET'
                           USING WS-COM-AREA
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
       END PROGRAM SISMENUS.
