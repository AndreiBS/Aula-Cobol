      ******************************************************************
      * Author: ANDREI BATISTA
      * Date: 25/10/2023
      * Purpose: DELETAR ALUNOS CADASTRADOS NO PROG SISCADAS
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. SISDELET.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
            DECIMAL-POINT IS COMMA.
            INPUT-OUTPUT SECTION.
            FILE-CONTROL.
               SELECT ALUNOS ASSIGN TO
               'C:\COBOL\Desafio-modulo-3\REGISTRO.DAT'
               ORGANISATION IS INDEXED
               ACCESS  MODE IS RANDOM
               RECORD  KEY  IS ID-ALUNO
               FILE STATUS IS WS-FS.

       DATA DIVISION.
       FILE SECTION.
       FD ALUNOS.
          COPY FD_REGIS.
       WORKING-STORAGE SECTION.
       01 WS-REGISTRO                      PIC X(50) VALUE SPACES.
       01 FILLER REDEFINES WS-REGISTRO.
          03 WS-ID-ALUNO                   PIC 9(03).
          03 WS-NM-ALUNO                   PIC X(20).
          03 WS-TL-ALUNO                   PIC X(15).
       77 WS-FS                            PIC 99.
          88 FS-OK                         VALUE 0.
       77 WS-EOF                           PIC X.
          88 EOF-OK                        VALUE 'S' 's' FALSE 'N'.
       77 WS-EXIT                          PIC X.
          88 EXIT-OK                       VALUE 'F' 'f' FALSE 'N'.
       77 WS-CONFIRM                       PIC X VALUE SPACES.

       LINKAGE SECTION.
       01 LK-COM-AREA.
          03 LK-MENSAGEM                   PIC X(50).
       PROCEDURE DIVISION USING LK-COM-AREA.

       MAIN-PROCEDURE.
            DISPLAY '*** EXCLUI ALUNOS ***'
            SET EXIT-OK           TO FALSE
            PERFORM P300-EXCLUIR THRU P300-FIM UNTIL EXIT-OK
            PERFORM P900-FIM
            .
       P300-EXCLUIR.
            SET EOF-OK            TO FALSE
            SET FS-OK             TO TRUE
            MOVE SPACES           TO WS-CONFIRM
            OPEN I-O ALUNOS

            IF FS-OK THEN
                DISPLAY 'Informe o numero de identificacao do contato: '
                ACCEPT ID-ALUNO

                READ ALUNOS INTO WS-REGISTRO
                   KEY IS ID-ALUNO
                   INVALID KEY
                       DISPLAY 'ALUNO NAO EXISTE! '
                   NOT INVALID KEY
                       DISPLAY 'ALUNO SELECIONADO: ' WS-NM-ALUNO
                       DISPLAY 'TECLE: '
                               '<S> para excluir ou <QUALQUER TECLA>'
                               ' para abortar.'
                       ACCEPT WS-CONFIRM
                       IF WS-CONFIRM EQUAL 'S' OR 's' THEN
                           DELETE ALUNOS RECORD
                           DISPLAY 'Aluno excluido com sucesso!'
                       ELSE
                           DISPLAY 'Alteracao nao realizada.'
                       END-IF
                END-READ
            ELSE
                DISPLAY 'ERRO AO ABRIR O ARQUIVO DE ALUNOS. '
                DISPLAY 'FILE STATUS: ' WS-FS
            END-IF
            CLOSE ALUNOS

            DISPLAY
               'TECLE: '
               '<QUALQUER TECLA> para continuar, ou <F> para finalizar.'
            ACCEPT WS-EXIT
            .
       P300-FIM.
       P900-FIM.
            GOBACK.
       END PROGRAM SISDELET.
