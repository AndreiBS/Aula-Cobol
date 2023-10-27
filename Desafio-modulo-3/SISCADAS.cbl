      ******************************************************************
      * Author: ANDREI BATISTA
      * Date: 17/10/2023
      * Purpose: CADASTRAR ALUNO
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. SISCADAS.

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

       LINKAGE SECTION.
       01 LK-COM-AREA.
          03 LK-MENSAGEM                   PIC X(50).

       PROCEDURE DIVISION USING LK-COM-AREA.
       MAIN-PROCEDURE.
            DISPLAY '*** CADASTRO DE ALUNOS ***'
            SET EXIT-OK           TO FALSE
            PERFORM P300-CADASTRA THRU P300-FIM UNTIL EXIT-OK
            PERFORM P900-FIM
            .
       P300-CADASTRA.
            SET EOF-OK            TO FALSE
            SET FS-OK             TO TRUE
            DISPLAY 'PARA REGISTRAR UM ALUNO, INFORME: '
            DISPLAY 'Um numero para Identificacao e tecle <ENTER>: '
            ACCEPT WS-ID-ALUNO
            DISPLAY 'Um nome para o Contato e tecle <ENTER>: '
            ACCEPT WS-NM-ALUNO
            DISPLAY 'Um numero de Telefone e tecle <ENTER>: '
            ACCEPT WS-TL-ALUNO

            OPEN I-O ALUNOS
            IF WS-FS EQUAL 35 THEN
                OPEN OUTPUT ALUNOS
            END-IF
            IF FS-OK THEN
                MOVE WS-ID-ALUNO         TO ID-ALUNO
                MOVE WS-NM-ALUNO         TO NM-ALUNO
                MOVE WS-TL-ALUNO         TO TL-ALUNO

                WRITE REG-ALUNO
                       INVALID KEY
                           DISPLAY 'ALUNO JA CADASTRADO! '
                       NOT INVALID KEY
                           DISPLAY 'Aluno gravado com Sucesso! '
                END-WRITE
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
       END PROGRAM SISCADAS.
