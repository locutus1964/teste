* ATRASOS - LISTA ATRASOS DE CLIENTES
* SISTEMA ADMINISTRACAO DE VENDAS
* ADAO ELI PEREIRA - 10/07/92

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

    IF CTOD(DTFIM) > DATE()
      ? MENSAG('Data Final nao pode ser maior que data atual - ENTER')
      =INKEY(0)
      RETURN
    ENDIF

    IF CTOD(DTFIM) < CTOD(DTINI)
      ? MENSAG('Data Final nao pode ser menor que Data Inicial - ENTER')
      =INKEY(0)
      RETURN
    ENDIF
    
    mens = DADCORR
    DO PEGRESP WITH [SN]
       IF MC = 'N'
         RETURN
       ENDIF

    * PREPARACAO DO ARQUIVO COM DUPLICATAS E VENCIMENTOS
    MENS = 'Selecionando Duplicatas para emiss„o do relat¢rio...Aguarde'
    DO MENSMEIO WITH 'Duplicatas'

    USE DUPL
    COPY TO CADWORK FOR CTOD(VENC) >= CTOD(DTINI) .AND. CTOD(VENC) ;
    <= CTOD(DTFIM) .AND. EMPTY(DTCANCEL)
        
    MENS = 'Classificando Duplicatas para emiss„o do relat¢rio...Aguarde'
    DO MENSMEIO WITH 'Duplicatas'
    
    USE CADWORK
    INDEX ON CGC+SUBSTR(VENC,7,2)+SUBSTR(VENC,4,2)+SUBSTR(VENC,1,2) TO CADWORK
    CLOS DATA

    SELE 0
      USE CLIEN ORDER P_CGC

    SELE 0
      USE CADWORK INDEX CADWORK
      SET RELA TO CGC INTO CLIEN

    MCGC   = SPACE(16)
    KEY2   = SPACE(08)
    MVENC  = SPACE(8)
    MRAZ   = SPACE(40)
    TVAL   = 0
    QTTOT  = 0
    MVDP   = 0
    TOTVAL = 0
    DIAS   = 0
    TTDIAS = 0
    ATOTVAL = 0
    CTOTVAL = 0
    STOR 0 TO TOT1,TOT2,TOT3,TOT4,TOT5,TOT6
    QTATR = 0
    MRAZ  = ''
    
    TITULO = 'ATRASOS POR CLIENTE - DE '+DTINI+' ATE '+DTFIM

    PAGCTR = 0
    TAMREL = 2
    LIN = 66
    
    ? MENSAG('Imprimindo Atrasos por Cliente... Aguarde')
 
    DO DEFSAIDA   

    SELE CADWORK
    DO WHILE .NOT. EOF()
      MCGC = CGC
      
      IF LIN > 60
        ? CABEC('AV08900',TITULO,131)
        @ 3,1 SAY 'RAZAO SOCIAL DO CLIENTE'
        @ 3,33 SAY "           VCTO.    PAGTO.  DIAS EMISSAO DUPLICATA "+;
          "VALOR DUPLIC    VAL.ACUM.CLIEN."
        LIN = 5
      ENDIF  
           
      DO WHILE .NOT. EOF() .AND. MCGC = CGC

        DIAS = 0
        
        IF PAG <> SPACE(8)      &&.AND. CTOD(PAG) > CTOD(VENC)
           QTTOT  = QTTOT + 1
           MVDP = (VDP - DESC)
           TOTVAL = (TOTVAL + (MVDP - VALDEV))
           DIAS = CTOD(PAG) - CTOD(VENC)
           TTDIAS = TTDIAS + DIAS
           ATOTVAL = (ATOTVAL + ((MVDP - VALDEV) * DIAS))
           CTOTVAL = (CTOTVAL + (MVDP - VALDEV))
        ENDIF
           
        IF PAG = SPACE(8) .AND. CTOD(DTFIM) > CTOD(VENC)
           QTTOT  = QTTOT + 1
           MVDP = (VDP - DESC)
           TOTVAL = (TOTVAL + (MVDP - VALDEV))
           DIAS = CTOD(DTFIM) - CTOD(VENC)
           TTDIAS = TTDIAS + DIAS
           ATOTVAL = (ATOTVAL + ((MVDP - VALDEV) * DIAS))
           CTOTVAL = (CTOTVAL + (MVDP - VALDEV))
        ENDIF  

        IF DIAS > 0
           QTATR = QTATR + 1
           IF DIAS <= 30
              TOT1 = TOT1 + (MVDP - VALDEV)
           ELSE
              IF DIAS <= 60
                 TOT2 = TOT2 + (MVDP - VALDEV)
              ELSE
                 IF DIAS <= 90
                    TOT3 = TOT3 + (MVDP - VALDEV)
                 ELSE
                    IF DIAS <= 120
                       TOT4 = TOT4 + (MVDP - VALDEV)
                    ELSE
                       IF DIAS <= 150
                          TOT5 = TOT5 + (MVDP - VALDEV)
                       ELSE
                          TOT6 = TOT6 + (MVDP - VALDEV)
                       ENDIF
                    ENDIF   
                 ENDIF
              ENDIF
           ENDIF

           IF MRAZ <> CLIEN->RAZ
              @ LIN,1 SAY LEFT(CLIEN->RAZ,38)
              MRAZ = CLIEN->RAZ
           ENDIF
             
           @ LIN,41 SAY VENC
           @ LIN,51 SAY PAG
           @ LIN,PCOL()+1 SAY DIAS PICTURE "99999"
           @ LIN,PCOL()+1 SAY EMIS
           @ LIN,PCOL()+1 SAY NDUP
           @ LIN,PCOL()+1 SAY MVDP PICT '9,999,999,999.99'
           TVAL = (TVAL + (MVDP - VALDEV))
           @ LIN,PCOL()+1 SAY TVAL PICT '99,999,999,999.99'
           LIN = LIN + 1
        ENDIF
        
         MCGC = CGC    
         SKIP
     ENDDO

     IF QTATR <> 0
        @ LIN+1,15 SAY "NOTAS COM ATRASO :"
        @ PROW()  ,PCOL()+1 SAY QTATR PICTURE "999999"
        @ PROW()  ,PCOL()+5 SAY "ATRASO MEDIO :"
        @ PROW()  ,PCOL()+1 SAY ATOTVAL / CTOTVAL PICTURE "99999"
        ATOTVAL = 0
        CTOTVAL = 0

        @ LIN+2,15 SAY "TOTAL DE NOTAS   :"
        @ PROW()  ,PCOL()+1 SAY QTTOT PICTURE "999999"
        @ PROW()  ,PCOL()+5 SAY "VALOR TOTAL FATURAMENTO :"
        @ PROW()  ,PCOL()+1 SAY (TOTVAL + TVAL) PICT '99,999,999,999.99'
        LIN = LIN + 4
        QTTOT = 0
        QTATR = 0
        QTTOT = 0
        TOTVAL = 0
        TVAL   = 0
     ENDIF

ENDDO

@ PROW()+2,10 SAY "RESUMO POR PERIODOS DE ATRASO"
@ PROW()+2,05 SAY "ATE  30 DIAS DE ATRASO :"
@ PROW()  ,PCOL()+1 SAY TOT1 PICTURE "99,999,999,999.99"
@ PROW()+1,05 SAY "ATE  60 DIAS DE ATRASO :"
@ PROW()  ,PCOL()+1 SAY TOT2 PICTURE "99,999,999,999.99"
@ PROW()+1,05 SAY "ATE  90 DIAS DE ATRASO :"
@ PROW()  ,PCOL()+1 SAY TOT3 PICTURE "99,999,999,999.99"
@ PROW()+1,05 SAY "ATE 120 DIAS DE ATRASO :"
@ PROW()  ,PCOL()+1 SAY TOT4 PICTURE "99,999,999,999.99"
@ PROW()+1,05 SAY "ATE 150 DIAS DE ATRASO :"
@ PROW()  ,PCOL()+1 SAY TOT5 PICTURE "99,999,999,999.99"
@ PROW()+1,05 SAY "ACIMA DE 150 DIAS      :"
@ PROW()  ,PCOL()+1 SAY TOT6 PICTURE "99,999,999,999.99"
@ PROW()+1,05 SAY "TOTAL                  :"
@ PROW()  ,PCOL() SAY TOT1+TOT2+TOT3+TOT4+TOT5+TOT6 PICTURE "999,999,999,999.99"

DO IMPSAIDA

RETURN
