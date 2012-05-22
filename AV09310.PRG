* AV09300 - LIMPA OS ACUMULADOS DO ESTOQUE (PROD) MENSAIS E ANUAIS
* SISTEMA ADMINISTRACAO DE VENDAS
*
USE PROD INDEX PROD.IDX
*
    MENS = 'Exclus„o Mensal (M), Anual (A), ou cancela a opera‡„o (C)'
    DO PEGRESP WITH [MAC]
*
    IF MC = 'C'
      RETURN
    ELSE
      IF MC = 'M'
        MENS = 'Zerando Acumulados Mensais... Aguarde'
        ? MENSAG(MENS)
        @ 23,POS SAY MENS
        DO WHILE .NOT. EOF()
          REPL VAM WITH 0,FME WITH 0
            SKIP
            LOOP
        ENDDO
      ELSE
        MENS = 'Zerando Acumulados Mensais e Anuais... Aguarde'
        ? MENSAG(MENS)
        @ 23,POS say MENS
        DO WHILE .NOT. EOF()
          REPL VAM WITH 0,VAA WITH 0,FME WITH 0,FAC WITH 0
            SKIP
            LOOP
        ENDDO
      ENDIF
    ENDIF
*
CLOS DATA
RETURN