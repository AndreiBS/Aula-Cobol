      ******************************************************************
      * Author: ANDREI BATISTA
      * Date: 25/10/2023
      * Purpose:  ALTERAR ALUNOS CADASTRADOS PELO PROG SISCADAS
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. SISALTER.
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
          88 EOF-OK                        VALUE 'S' FALSE 'N'.
       77 WS-EXIT                          PIC X.
          88 EXIT-OK                       VALUE 'F' 'f' FALSE 'N'.
       77 WS-CONFIRM                       PIC X VALUE SPACES.

       LINKAGE SECTION.
       01 LK-COM-AREA.
          03 LK-MENSAGEM                   PIC X(50).
       PROCEDURE DIVISION USING LK-COM-AREA.
       MAIN-PROCEDURE.
            DISPLAY '*** ALTERACAO DE ALUNOS ***'
            SET EXIT-OK           TO FALSE
            PERFORM P300-ALTERAR THRU P300-FIM UNTIL EXIT-OK
            PERFORM P900-FIM
            .
       P300-ALTERAR.
            SET EOF-OK            TO FALSE
            SET FS-OK             TO TRUE
            MOVE SPACES           TO WS-CONFIRM
            OPEN I-O ALUNOS

            IF FS-OK THEN
                DISPLAY 'Informe o numero de identificacao do aluno: '
                ACCEPT ID-ALUNO

                READ ALUNOS INTO WS-REGISTRO
                   KEY IS ID-ALUNO
                   INVALID KEY
                       DISPLAY 'ALUNO NAO EXISTE! '
                   NOT INVALID KEY
                       DISPLAY 'Nome atual: ' WS-NM-ALUNO
                       DISPLAY 'Informe o novo nome: '
                       ACCEPT WS-NM-ALUNO
                       DISPLAY 'Telefone atual: ' WS-TL-ALUNO
                       DISPLAY 'Informe o novo telefone: '
                       ACCEPT WS-TL-ALUNO
                       DISPLAY 'TECLE: '
                               '<S> para confirmar ou <QUALQUER TECLA>'
                               ' para continuar com o atual.'
                       ACCEPT WS-CONFIRM
                       IF WS-CONFIRM EQUAL 'S' OR 's' THEN
                           MOVE WS-NM-ALUNO    TO  NM-ALUNO
                           MOVE WS-TL-ALUNO    TO  TL-ALUNO
                           REWRITE REG-ALUNO
                           DISPLAY 'Contato atualizado com sucesso!'
                       ELSE
                           DISPLAY 'Alteracao nao realizada.'
                       END-IF
                END-READ
            ELSE
                DISPLAY 'ERRO AO ABRIR O ARQUIVO DE CONTATOS. '
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
       END PROGRAM SISALTER.
