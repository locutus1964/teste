* AV05400 - ENTRADAS NO ESTOQUE POR PRODUCAO OU COMPRA
* SISTEMA ADMINISTRACAO DE VENDAS
* ADAO ELI PEREIRA - 15-04-90 - 20-11-91
*
? CABTELA(0,0,24,79)
MENS = 'Entradas/Saidas por Produ‡„o ou Compra'
? MENSAGR(MENS)
@ 01,POS SAY MENS
*
@ 03,1 SAY '                     A t e n ‡ „ o : '
@ 05,6 SAY 'Ao processar o movimento do dia, s„o eliminadas as do dia anterior'
@ 07,6 SAY 'para emitir o Relat¢rIo  "ENTRADAS/SAIDAS POR PRODU€ŽO OU COMPRA".'
@ 10,6 say "Se Relat¢rio ainda n„o emitido, poder  se-lo antes do in¡cio ou se"
@ 12,6 SAY "preferir, as entradas poder„o ser acumuladas para o pr¢ximo."
@ 15,6 SAY 'Responda (I) para in¡cio do dia e (C) para continua‡„o'
@ 18,6 SAY 'A entrada ‚ indicada por (E) e a corre‡„o das entradas ‚ feito pela'
@ 20,6 say 'reinclus„o do documento com tipo (S) de sa¡da'
*
  MENS = 'In¡cio do dia (I) ou continua‡„o (C) ? '
  DO PEGRESP WITH [IC]
    IF MC = 'I'
      USE REPORTE INDEX REPORTE
      ZAP
      CLOS DATA
    ENDIF
*
@ 03,01 CLEAR TO 21,78
*
  @ 1,03 say dtoc(date())
  @ 3,03 say 'Ref'
  @ 3,08 SAY 'Descri‡„o do Produto'
  @ 3,58 SAY 'Nr.Doc'
  @ 3,65 SAY 'E/S'
  @ 3,69 say 'Quantidade'
    LIN = 4
*
CLOS DATA
SELE 0
  USE MOD3 INDEX MOD3
SELE 0
  USE REPORTE INDEX REPORTE
SELE 0
  USE PROD INDEX PROD
*
DO WHILE .T.
*
SELE PROD
*
MREF  = SPACE(04)
MQTD  = 0.00
MNDOC = 0
TTDOC = SPACE(01)
*
? MENSAG(ECODIGO)
@ LIN,3 GET MREF
  READ
  
    IF LASTKEY() = 27
      CLOS DATA
      CLEAR
      RETURN
    ENDIF
    
    IF MREF = SPACE(10)
      ? MENSAG(CALPHA)
      loop
    ENDIF
*
SEEK MREF
  IF !(EOF() .OR. BOF())
    @ LIN,08  SAY SUBS(DES,1,48)
  ELSE
    ? MENSAG('Produto n„o cadastrado ou Referˆncia errada - ENTER')
    =INKEY(0)
    LOOP
  ENDIF
*
@ 23,01 SAY SPACE(78)
DO WHILE .T.
  @ LIN,58 GET MNDOC PICT '999999'
    READ
      IF MNDOC > 0
        EXIT
      ENDIF
        ? MENSAG('N£mero do Documento n„o pode ser Zeros')
ENDDO
*
? MENSAG('( E ) Aumenta Saldo do Estoque e ( S ) Diminui o Saldo')
TTDOC = ' '
  @ LIN,66 GET TTDOC PICT '!' VALID(TTDOC$'ES')
    READ
*
@ 23,01 SAY SPACE(78)
DO WHILE .T.
  @ LIN,69 GET MQTD PICT '9999999.99'
    READ
      IF MQTD <> 0
        EXIT
      ENDIF
        ? MENSAG(CNUMER)
ENDDO
*
  IF TTDOC = 'E'
    REPL SAL WITH (SAL + MQTD)
  ELSE
    REPL SAL WITH (SAL - MQTD)
  ENDIF
*
IF TTDOC = 'E'
  STOR '01' TO MTDOC
ELSE
  STOR '51' TO MTDOC
ENDIF
*
  MENS = DADCORR
  DO PEGRESP WITH [SN]
    IF MC = 'N'
      LOOP
    ENDIF
      
* CRIACAO DO ARQUIVO DAS ENTRADAS
*
  SELE REPORTE
  APPE BLAN
    REPL REF WITH MREF,TDOC WITH MTDOC,NDOC WITH MNDOC,QTD WITH MQTD
*
* CRIACAO DO ARQUIVO COM OS DADOS PARA EMISSAO DO MODELO 3
*
  SELE MOD3
  APPE BLAN
    REPL REF WITH MREF,TDOC WITH MTDOC,NDOC WITH MNDOC
    REPL DATA WITH DTOC(DATE()),QTD WITH MQTD
*
LIN = LIN + 1
  IF LIN > 21
    @ 04,01 CLEAR TO 21,78
    LIN = 4
  ENDIF
ENDDO
RETURN