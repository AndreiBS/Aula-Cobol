      ******************************************************************
      * Author: ANDREI BATISTA
      * Date: 13/10/2023
      * Purpose: Criando uma calculadora em COBOL
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CALCULADORA.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       77 WS-MODO                  PIC 9    VALUE 0.
       77 WS-NUM-1                 PIC S9(3)V99.
       77 WS-NUM-2                 PIC S9(3)V99.
       77 WS-RESULTADO             PIC S9(4)V99.
       77 WS-DECISAO               PIC 9    VALUE 0.

       PROCEDURE DIVISION.

       P100-DISPLAY.
            INITIALISE WS-MODO
                       WS-NUM-1
                       WS-NUM-2
                       WS-RESULTADO
                       WS-DECISAO
            DISPLAY '--------------------------------------------------'
            DISPLAY '------------- INICIANDO CALCULADORA --------------'
            DISPLAY '-Selecione o modo que deseja:                    -'
            DISPLAY '-Selecione 1 para somar                          -'
            DISPLAY '-Selecione 2 para subtrair                       -'
            DISPLAY '-Selecione 3 para multiplica                     -'
            DISPLAY '-Selecione 4 para dividir                        -'
            DISPLAY '--------------------------------------------------'
            ACCEPT WS-MODO

            DISPLAY 'Insira o primeiro numero: '
            ACCEPT WS-NUM-1

            DISPLAY 'Insira o segundo numero: '
            ACCEPT WS-NUM-2

            IF WS-NUM-1 NOT NUMERIC
               PERFORM P600-ERRO
            END-IF

            IF WS-NUM-2 NOT NUMERIC
                PERFORM P600-ERRO
            END-IF

            EVALUATE WS-MODO
               WHEN "1"
                     PERFORM P200-SOMA          THRU   P200-FIM
               WHEN "2"
                     PERFORM P300-SUBTRACAO     THRU   P300-FIM
               WHEN "3"
                     PERFORM P400-MULTIPLICACAO THRU   P400-FIM
               WHEN "4"
                     PERFORM P500-DIVISAO       THRU   P500-FIM
               WHEN OTHER
                     PERFORM P600-ERRO
            END-EVALUATE
           .
       P100-FIM.

       P200-SOMA.
            COMPUTE WS-RESULTADO = WS-NUM-1 + WS-NUM-2
                           ON SIZE ERROR PERFORM P600-ERRO.
            DISPLAY 'Resultado:  'WS-NUM-1' + 'WS-NUM-2'= ' WS-RESULTADO
            PERFORM P700-CONCLUSAO
           .
       P200-FIM.

       P300-SUBTRACAO.
            COMPUTE WS-RESULTADO = WS-NUM-1 - WS-NUM-2
                           ON SIZE ERROR PERFORM P600-ERRO.
            DISPLAY 'Resultado:  'WS-NUM-1' - 'WS-NUM-2'= ' WS-RESULTADO
            PERFORM P700-CONCLUSAO
           .
       P300-FIM.

       P400-MULTIPLICACAO.
            COMPUTE WS-RESULTADO = WS-NUM-1 * WS-NUM-2
                           ON SIZE ERROR PERFORM P600-ERRO.
            DISPLAY 'Resultado:  'WS-NUM-1' * 'WS-NUM-2'= ' WS-RESULTADO
            PERFORM P700-CONCLUSAO
           .
       P400-FIM.

       P500-DIVISAO.
            COMPUTE WS-RESULTADO = WS-NUM-1 / WS-NUM-2
                           ON SIZE ERROR PERFORM P600-ERRO.
            DISPLAY 'Resultado:  'WS-NUM-1' / 'WS-NUM-2'= ' WS-RESULTADO
            PERFORM P700-CONCLUSAO
           .
       P500-FIM.

       P600-ERRO.
            INITIALISE WS-DECISAO
            DISPLAY '--------------------------------------------------'
            DISPLAY '-----Erro de Processamento! Deseja recomecar?-----'
            DISPLAY '--Digite 1 para sim                             --'
            DISPLAY '--Digite qualquer tecla para encerrar           --'
            DISPLAY '--------------------------------------------------'
            ACCEPT WS-DECISAO
            EVALUATE WS-DECISAO
              WHEN 1
                   PERFORM P100-DISPLAY
              WHEN OTHER
               DISPLAY '------------CALCULADORA ENCERRADA--------------'
            END-EVALUATE
            GOBACK
            .
       P600-FIM.
       P700-CONCLUSAO.
            INITIALISE WS-DECISAO
            DISPLAY '--------------------------------------------------'
            DISPLAY '---------------Deseja recomecar?------------------'
            DISPLAY '--Digite 1 para sim                             --'
            DISPLAY '--Digite qualquer tecla para encerrar           --'
            DISPLAY '--------------------------------------------------'
            ACCEPT WS-DECISAO
            EVALUATE WS-DECISAO
               WHEN 1
                   PERFORM P100-DISPLAY
               WHEN OTHER
                   DISPLAY '------------CALCULADORA ENCERRADA----------'
            END-EVALUATE
            GOBACK
           .
       P700-FIM.
            GOBACK.
       END PROGRAM CALCULADORA.
