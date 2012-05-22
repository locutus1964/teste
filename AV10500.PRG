* AV10500 - SELECIONA DADOS PARA EXECUCAO DO RELATORIO DEMONSTRATIVO ESTOQUE
* SISTEMA ADMINISTRATIVO DE VENDAS
* ADAO ELI PEREIRA - 04-08-90
*
MMES = SPACE(05)
MC   = SPACE(01)
MENS =  'Seleciona dados p/Demonstrativo do Estoque ?'
DO PEGRESP WITH [SN]
  IF MC = 'N'
    RETURN
  ENDIF
*
? JANELA(16,40,20,78)
? BORDA(16,40,20,78)
@ 18,42 SAY 'Informe MES/ANO para sele‡„o 'get mmes pict '99/99'
  READ
    MENS = DADCORR
    DO PEGRESP WITH [SN]

      IF MC = 'N'
        RETURN
      ENDIF

? MENSAG('Selecionando dados para o Demonstrativo... Aguarde')
*
SELE B
  USE MOD3DEM
  ZAP
SELE A
  USE MOD3
*
  DO WHILE .NOT. EOF()
  
    IF SUBS(DATA,4,5) = MMES
      STOR REF TO MREF
      STOR TDOC TO MTDOC
      STOR NDOC TO MNDOC
      STOR QTD TO MQTD
      SELE B
      APPE BLAN
      REPL KEYMOD3 WITH (MREF+MTDOC),QTD WITH MQTD
    ENDIF
      
    SELE A
    SKIP
  ENDDO
*
CLOS DATA
? MENSAG('Classificando Entradas e Saidas p/Demonstrativo... Aguarde')
USE MOD3DEM
  INDEX ON KEYMOD3 TO CADWORK3.IDX
CLOS DATA
*
? MENSAG('Totalizando Entradas e Saidas p/Demonstrativo... Aguarde')
*
USE MOD3DEM INDEX CADWORK3.IDX
  TOTAL ON KEYMOD3 TO CADWORK4 FIELD QTD
clos data

USE CADWORK4
  INDEX ON SUBS(KEYMOD3,1,11) + SUBS(KEYMOD3,12,2) TO CADWORK4.IDX
CLOS DATA
USE MOD3DEM
ZAP
CLOS DATA
RETURN