* AV05500 - RELATORIO DAS ENTRADAS POR PRODUCAO OU COMPRAS
* SISTEMA ADMINISTRACAO DE VENDAS
* ADAO ELI PEREIRA - 10-07-90
*
DO TESTIMP2
  IF CP = 'N'
    RETURN
  ENDIF
*
PAGCTR = 0
TOTQTD = 0
TAMREL = 2
*
? MENSAG('Imprimindo Entradas/Saidas por Produ‡„o ou Compras... Aguarde')
*
DO DEFSAIDA
*
SELE 0
  USE PROD INDEX PROD
SELE 0
  USE REPORTE INDEX REPORTE
*
DO WHILE !EOF()
*
  ? CABEC('AV05500','ENTRADAS/SAIDAS POR PRODUCAO OU COMPRAS',131)
  @ 4,1 SAY 'REF'
  @ 4,06 SAY 'DESCRICAO DO PRODUTO'
  @ 4,55 SAY 'UN'
  @ 4,59 SAY 'CLAS FISC'
  @ 4,70 SAY 'NUM DOC'
  @ 4,78 SAY 'T.DOC'
  @ 4,87 SAY 'QUANTIDADE'
  @ 4,102 SAY 'QTD ACUMUL'
*
  @ 5,1 SAY REPLICATE('-',131)
  
  LIN = 6

  SELE PROD  
  DO WHILE !EOF() .AND. LIN < 58
    @ LIN,1 SAY REF
      MREF = REF
      SEEK MREF
        IF !EOF() .OR. BOF()
          @ LIN,06 SAY SUBS(DES,1,48)
          @ LIN,55 SAY UNI
          @ LIN,59 SAY CCL
        ENDIF
      SELE REPORTE       
    @ LIN,70 SAY STR(NDOC,6)
      IF TDOC = '01'
        MTDOC = 'E'
      ELSE
        MTDOC = 'S'
      ENDIF
    @ LIN,80 SAY MTDOC
    @ LIN,85 SAY QTD PICT '9,999,999.99'
      IF MTDOC = 'E'
        TOTQTD = (QTD + TOTQTD)
      ELSE
        TOTQTD = (TOTQTD - QTD)
      ENDIF
      
      SELE PROD
        SKIP
          IF REF <> MREF
            @ LIN,100 SAY TOTQTD PICT '9,999,999.99'
              TOTQTD = 0
          ENDIF
            LIN = LIN + 1
          IF EOF()
            @ LIN,1 SAY REPLICATE('-',131)
            @ LIN+1,10 SAY 'F I N A L    D E S T E    R E L A T O R I O'
          ENDIF
  ENDDO
ENDDO

DO IMPSAIDA

RETURN
