      ******************************************************************
      * Author: ANDREI BATISTA
      * Date: 25/10/2023
      * Purpose: COMPUTAR A MEDIA DAS NOTAS NA MATERIA E SABER O STATUS
      *        DE APROVADO OU REPROVADO
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. SISCOMPT.
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

               SELECT MATERIAS ASSIGN TO
               'C:\COBOL\Desafio-modulo-3\MATERIAS.DAT'
               ORGANISATION IS INDEXED
               ACCESS  MODE IS SEQUENTIAL
               RECORD  KEY  IS ID-MATERIA
               FILE STATUS IS WS-FS.

               SELECT TODOS ASSIGN TO
               'C:\COBOL\Desafio-modulo-3\TODOS.DAT'
               ORGANISATION IS INDEXED
               ACCESS MODE IS RANDOM
               RECORD KEY IS ID-ALUNO-2
               FILE STATUS IS WS-FS.

       DATA DIVISION.
       FILE SECTION.
       FD ALUNOS.
          COPY FD_REGIS.
       FD MATERIAS.
          COPY FD_MATER.
       FD TODOS.
       01 REG-ALUNO-2.
          03 ID-ALUNO-2                  PIC 9(03).
          03 NM-ALUNO-2                  PIC X(20).
          03 TL-ALUNO-2                  PIC X(15).
       01 REG-MATERIA-2.
          03 ID-MATERIA-2                PIC 9(03).
          03 NM-MATERIA-2                PIC X(20).
          03 NT-APROVACAO-2              PIC 9(02)V99.



       WORKING-STORAGE SECTION.
       01 WS-REGISTRO                      PIC X(50) VALUE SPACES.
       01 FILLER REDEFINES WS-REGISTRO.
          03 WS-ID-ALUNO                   PIC 9(03).
          03 WS-NM-ALUNO                   PIC X(20).
          03 WS-TL-ALUNO                   PIC X(15).
          03 WS-ID-MATERIA                 PIC 9(03).
          03 WS-NM-MATERIA                 PIC X(20).
          03 WS-NT-APROVACAO               PIC 9(02)V99.
       01 WS-VARIAVEIS.
          03 WS-ID-ALUNO                   PIC X VALUE SPACES.
          03 WS-ID-MATERIA                 PIC X VALUE SPACES.
          03 WS-RESULTADO                  PIC X(10)  VALUE SPACES.
          77 WS-DECISAO                    PIC 9(2)V99 VALUE ZEROS.
          77 WS-NOTA-1                     PIC 9(2)V99 VALUE ZEROS.
          77 WS-NOTA-2                     PIC 9(2)V99 VALUE ZEROS.
          77 WS-NOTA-3                     PIC 9(2)V99 VALUE ZEROS.
          77 WS-NOTA-4                     PIC 9(2)V99 VALUE ZEROS.
          77 WS-MEDIA                      PIC 9(2)V99 VALUE ZEROS.
       77 WS-FS                            PIC 99.
          88 FS-OK                         VALUE 0.
       77 WS-EOF                           PIC X.
          88 EOF-OK                        VALUE 'S' FALSE 'N'.
       77 WS-EXIT                          PIC X.
          88 EXIT-OK                       VALUE 'F' 'f' FALSE 'N'.


       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
            INITIALISE WS-VARIAVEIS
            SET EXIT-OK        TO FALSE
            PERFORM P100-DADOS  THRU P100-FIM
            PERFORM P200-CALC   THRU P200-FIM
            PERFORM P300-MOSTRA THRU P300-FIM

            .
       P100-DADOS.

            SET EOF-OK            TO FALSE
            SET FS-OK             TO TRUE

            OPEN I-O ALUNOS
            OPEN I-O MATERIAS
            IF FS-OK THEN
             DISPLAY '*************************************************'
             DISPLAY '***             BOLETIM COMPLETO              ***'
             DISPLAY '*************************************************'
             DISPLAY '*** Informe o ID do Aluno:                    ***'
             ACCEPT ID-ALUNO
             READ ALUNOS INTO WS-REGISTRO
                   KEY IS ID-ALUNO
                   INVALID KEY
                       DISPLAY 'ALUNO NAO EXISTE! '
                   NOT INVALID KEY
             DISPLAY 'Aluno escolhido: ' ID-ALUNO
             DISPLAY '*** Informe o ID da Materia:                  ***'
             ACCEPT ID-MATERIA
             READ MATERIAS INTO WS-REGISTRO
                   KEY IS ID-MATERIA
                   INVALID KEY
                       DISPLAY 'MATERIA NAO EXISTE! '
                   NOT INVALID KEY
             DISPLAY 'DIGITE A PRIMEIRA NOTA: '
             ACCEPT WS-NOTA-1

             DISPLAY 'DIGITE A SEGUNDA NOTA: '
             ACCEPT WS-NOTA-2

             DISPLAY 'DIGITE A TERCEIRA NOTA: '
             ACCEPT WS-NOTA-3

             DISPLAY 'DIGITE A QUARTA NOTA: '
             ACCEPT WS-NOTA-4

            END-IF
            CLOSE ALUNOS
            CLOSE MATERIAS
            .
       P100-FIM.
       P200-CALC.
            COMPUTE WS-MEDIA = (WS-NOTA-1 + WS-NOTA-2 + WS-NOTA-3 +
             WS-NOTA-4) / 4
                       ON SIZE ERROR PERFORM P900-ERRO
            END-COMPUTE
            IF WS-MEDIA >= WS-NT-APROVACAO
                MOVE 'APROVADO'        TO WS-RESULTADO
            ELSE
                MOVE 'REPROVADO'       TO WS-RESULTADO
            END-IF
            .
       P200-FIM.
       P300-MOSTRA.
             DISPLAY '*************************************************'
             DISPLAY '***        RESULTADO BOLETIM COMPLETO         ***'
             DISPLAY '*************************************************'
             DISPLAY 'Nome do Aluno    : ' WS-NM-ALUNO
             DISPLAY 'Nome da Materia  : ' WS-NM-MATERIA
             DISPLAY 'Media            : ' WS-MEDIA
             DISPLAY 'Status           : ' WS-RESULTADO
             DISPLAY '*************************************************'
             DISPLAY
               'TECLE: '
               '<QUALQUER TECLA> para continuar, ou <F> para finalizar.'
            ACCEPT WS-DECISAO
               EVALUATE WS-DECISAO
               WHEN 'F'
                   PERFORM P999-FIM
               WHEN 'f'
                   PERFORM P999-FIM
               WHEN OTHER
                   PERFORM MAIN-PROCEDURE
             .
       P300-FIM.
       P900-ERRO.
            DISPLAY 'ERRO DE PROCESSAMENTO'
            PERFORM MAIN-PROCEDURE.
       P999-FIM.
            DISPLAY 'FIM DO PROGRAMA.'
            GOBACK.
       END PROGRAM SISCOMPT.
