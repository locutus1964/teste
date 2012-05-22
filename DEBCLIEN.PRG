* AV08500 - LISTA OS DEBITOS POR CLIENTE
* SISTEMA ADMINISTRACAO DE VENDAS
* ADAO ELI PEREIRA - 02/03/87*  -  10/07/92

DO TESTIMP2
  IF CP = 'N'
    RETURN
  ENDIF

@ 15,52 clear to 21,78
@ 15,52 to 21,78 double
@ 17,55 SAY "Data Inicial :"
@ 19,55 SAY "Data Final :"
DTINI = SPACE(08)
DTFIM = SPACE(08)
@ 17,68 GET DTINI PICTURE "99/99/99"
@ 19,68 GET DTFIM PICTURE "99/99/99"
READ

  * PREPARACAO DO ARQUIVO COM DUPLICATAS E VENCIMENTOS
  ? MENSAG('Classificando duplicatas para emissao do relatorio...Aguarde')
*
  USE DUPL
  
  COPY TO CADWORK FOR VENC >= CTOD(DTINI) .AND. VENC <= ;
          CTOD(DTFIM) .AND. LEFT(PAG,2) = '  ' .AND. EMPTY(DTCANCEL)
          
  USE CADWORK
  INDEX ON CGC+SUBSTR(VENC,7,2)+SUBSTR(VENC,4,2)+SUBSTR(VENC,1,2) TO CADWORK
  CLOS DATA
*
  MCGC  = SPACE(16)
  KEY2  = SPACE(08)
  MVENC = SPACE(08)
  MRAZ  = SPACE(30)
  VALDIA = 0.00
  TVAL  = 0
*
  SELE B
    USE CLIEN ORDER P_CGC
  SELE A
    USE CADWORK  INDEX CADWORK

  PAGCTR = 0
  TAMREL = 2
  
    ? MENSAG('Imprimindo os Debitos por Cliente... Aguarde')

  DO DEFSAIDA
  
  TITULO ='DEBITOS POR CLIENTE - DE '+DTINI+' ATE '+DTFIM
*
  DO WHILE .NOT. EOF()

    ? CABEC('AV08500',TITULO,131)

    @ 3,1 SAY 'RAZAO SOCIAL DO CLIENTE'
    @ 3,45 SAY ' VENCTO'
    @ 3,55 SAY 'DUPLICATA'
    @ 3,67 SAY 'EMISSAO'
    @ 3,78 SAY 'VALOR DUPLICATA    VAL.ACUM.CLIEN.'
    @ 3,111 SAY '  VALOR ACUMULADO '

    LIN = 5

    DO WHILE !EOF() .AND. LIN < 60

      SELE A
*
        IF CGC <> MCGC
          TVAL = 0.00
          LIN = LIN + 1
        ENDIF

        IF CGC <> MCGC .OR. LIN = 5
          MCGC = CGC
          SELE B
          SEEK MCGC

          IF !(EOF() .OR. BOF())
            @ LIN,1 SAY LEFT(RAZ,30)
          ELSE
            @ LIN,1 SAY 'CLIENTE NAO CADASTRADO'
          ENDIF

          SELE A

        ENDIF
*
        IF SUBSTR(VENC,1,2)+SUBSTR(VENC,4,2)+SUBSTR(VENC,7,2) <> KEY2 .OR. LIN = 5
          @ LIN,45 SAY VENC
          KEY2 = SUBSTR(VENC,1,2)+SUBSTR(VENC,4,2)+SUBSTR(VENC,7,2)
        ENDIF
*
        @ LIN,55 SAY NDUP
        @ LIN,67 SAY EMIS
        MVDP = (VDP - DESC)
        @ LIN,77 SAY MVDP PICT '99,999,999,999.99'
        TVAL = (TVAL + MVDP - VALDEV)
        VALDIA = (VALDIA + MVDP - VALDEV)

          IF VALDEV > 0
            LIN = LIN + 1
            @ LIN,55 SAY 'VALOR DEVOLUCAO'
            @ LIN,77 SAY VALDEV PICT '99,999,999,999.99'
          ENDIF

        @ LIN,95 SAY TVAL PICT '99,999,999,999.99'
        MOBS = OBS
        SKIP
*
          IF SUBSTR(VENC,1,2)+SUBSTR(VENC,4,2)+SUBSTR(VENC,7,2) <> KEY2 .OR. EOF()
            @ LIN,113 SAY VALDIA PICT '999,999,999,999.99'
          ENDIF

          IF MOBS <> SPACE(10)
            LIN = LIN + 1
            @ LIN,90 SAY MOBS
          ENDIF
*
          IF CGC <> MCGC .OR. EOF()
            LIN = LIN + 1
          ENDIF
*
        LIN = LIN + 1
        LOOP
    ENDDO

  ENDDO
  
  DO IMPSAIDA
  CLOS DATA

RETURN

