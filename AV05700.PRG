* AV05700 - TRATAMENTO DOS AJUSTES DE INVENTARIO
* SISTEMA ADMINISTRACAO DE VENDAS
* ADAO ELI PEREIRA - 15-04-90
*
MNDOC = 0
*
? CABTELA(0,0,24,79)
MENS = 'Ajustes de Invent rio'
? MENSAGR(MENS)
@ 01,POS SAY MENS
*
@ 03,01 SAY '                    A  t  e  n  ‡  „  o : '
@ 05,06 SAY 'Os Ajustes de Invent rio s„o permitidos para corrigir diferen‡as'
@ 07,06 SAY 'nos saldos de estoque motivadas por perdas, materiais defeituosos'
@ 09,06 SAY 'que precisam ser destruidos ou eventuais erros, sem corre‡„o pre-'
@ 11,06 SAY 'vista nas rotinas normais do Sistema'
@ 14,06 SAY 'O Ajuste de Invent ario ‚ processado indicando-se:'
@ 16,16 SAY '    Entrada (E) => aumenta o saldo do estoque'
@ 18,16 say '    Sa¡da   (S) => diminui o saldo do estoque'
*
? MENSAG('Pressione qualquer Tecla para continuar')
=INKEY(0)
*
@ 03,01 CLEAR TO 21,78
@ 03,03 say 'Ref  Descri‡„o do Produto'
@ 03,57 SAY 'Nr Doc E/S Quantidade'
  LIN = 4
*
SELE 0
  USE MOD3 ORDER X_DATA
SELE 0
  USE PROD INDEX PROD.IDX
*
DO WHILE .T.
*
SELE PROD
MREF = SPACE(04)
MES  = SPACE(01)
MQTD = 0.00
*
@ LIN,3 GET MREF
? MENSAG(ECODIGO)
  READ
  
    IF LASTKEY() = 27
      CLOS DATA
      CLEAR
      RETURN
    ENDIF
    
    IF MREF = SPACE(10)
      ? MENSAG(CALPHAN)
      loop
    ENDIF
*
*
SEEK MREF
  IF !(EOF() .OR. BOF())
    MDES  = DES
    @ LIN,08 SAY SUBS(MDES,1,48)
  ELSE
    ? MENSAG(CORRIJA)
    =INKEY(0)
    LOOP
  ENDIF
*
@ 23,01 SAY SPACE(78)
DO WHILE .T.
@ lin,58 get mndoc pict '999999'
  read
    IF MNDOC > 0
      EXIT
    ENDIF
      ? MENSAG(CNUMER)
ENDDO
*
MES = ' '
? MENSAG('Nao pode ser diferente de (E) Entrada ou (S) Saida')
  @ LIN,65 GET MES PICT '!' VALID(MES$'ES')
    READ
      IF MES = 'E'
        MTDOC = '02'
      ELSE
        MTDOC = '52'
      ENDIF
*
@ 23,01 SAY SPACE(78)
  DO WHILE .T.
    @ LIN,67 GET MQTD PICT '99999999.99'
      READ
        IF MQTD > 0
          EXIT
        ENDIF
          ? MENSAG(CNUMER)
    ENDDO
*
    MENS = DADCORR
    DO PEGRESP WITH [SN]
      IF MC = 'N'
        LOOP
      ENDIF
*
@ 23,01 SAY SPACE(78)
  IF MES = 'E'
    REPL SAL WITH (SAL + MQTD)
  ELSE
    REPL SAL WITH (SAL - MQTD)
  ENDIF
*
SELE MOD3
APPE BLAN
  REPL REF WITH MREF,TDOC WITH MTDOC,DATA WITH DTOC(HOJE)
  REPL QTD WITH MQTD,NDOC WITH MNDOC
*
LIN = LIN + 1
  IF LIN > 21
    @ 04,01 CLEAR TO 21,78
    LIN = 4
  ENDIF
ENDDO
RETURN

