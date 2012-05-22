* AV08900 - LISTA ATRASOS DE CLIENTES
* SISTEMA ADMINISTRACAO DE VENDAS
* VALDOMIRO SPINOLA ABREU - 09/01/90  /  ADAO ELI PEREIRA - 10/07/92

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


STOR 0.00 TO TOTVAL  , TVAL    , TOT1 , TOT2 , QTATR , QTTOT , TTDIAS 
STOR 0.00 TO ATOTVAL , CTOTVAL , TOT3 , TOT4 , TOT5 , TOT6 

    IF CTOD(SUBS(DTFIM,1,2)+'/'+SUBS(DTFIM,3,2)+'/'+SUBS(DTFIM,5,2)) > DATE()
      ? MENSAG('Data Final nao pode ser maior que data atual - ENTER')
      =INKEY(0)
      RETURN
    ENDIF


    * PREPARACAO DO ARQUIVO COM DUPLICATAS E VENCIMENTOS
    ? MENSAG('Classificando duplicatas para emiss„o do relat¢rio...Aguarde')

    USE DUPL
    COPY TO CADWORK FOR CTOD(VENC) <= CTOD(DTFIM) .AND. EMPTY(DTCANCEL)
    USE CADWORK
    INDEX ON CGC+SUBSTR(VENC,7,2)+SUBSTR(VENC,4,2)+SUBSTR(VENC,1,2) TO CADWORK
    CLOS DATA

    SELE 0
      USE CLIEN
      INDEX ON RAZ TO CLIAUX.IDX
      SET ORDER TO CLIAUX.IDX

    SELE 0
      USE CADWORK INDEX CADWORK

    MCGC   = SPACE(16)
    KEY2   = CTOD('  /  /  ')
    MVENC  = CTOD('  /  /  ')
    MRAZ   = SPACE(40)
    TVAL   = 0
    PAGCTR = 0
    TAMREL = 2
    
    TITULO = 'ATRASOS POR CLIENTE - DE '+DTINI+' ATE '+DTFIM

    AUX1 = CTOD(DTINI)
    AUX2 = CTOD(DTFIM)

    SELE CADWORK
    GOTO TOP

    ? MENSAG('Imprimindo Atrasos por Cliente... Aguarde')

*
*   LOOP DE CLIENTES
*

    LIN = 60
    
    DO DEFSAIDA

    DO WHILE .NOT. EOF()
      MCGC = CGC
*
*     PESQUISA CLIENTE NO ARQUIVO DE DUPLICATA
*
      STOR 0.00 TO TOTVAL,ATOTVAL,CTOTVAL,TVAL,QTATR,QTTOT,TTDIAS
*
*            LOOP DO MESMO CLIENTE
*
      DO WHILE !EOF() .AND. CGC = MCGC

         IF LIN > 59
           ? CABEC('AV08900',TITULO,131)
           @ 3,1 SAY 'RAZAO SOCIAL DO CLIENTE'
           @ 3,33 SAY "           VCTO.    PAGTO.  DIAS EMISSAO DUPLICATA "+;
           "VALOR DUPLIC    VAL.ACUM.CLIEN."
           LIN = 5
         ENDIF

         CHIMP = .T.
         
         IF PAG <> SPACE(08) .AND. CTOD(PAG) <= CTOD(VENC) 
             QTTOT  = QTTOT + 1
             MVDP = (VDP - DESC)
             TOTVAL = (TOTVAL + (MVDP - VALDEV))
             DIAS = CTOD(PAG) - CTOD(VENC)
             TTDIAS = TTDIAS + DIAS
             ATOTVAL = (ATOTVAL + ((MVDP - VALDEV) * DIAS))
             CTOTVAL = (CTOTVAL + (MVDP - VALDEV))

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

             SKIP
             LOOP
         ELSE
           QTATR = QTATR + 1
         ENDIF

         IF CHIMP
           SELE CLIEN
           IF SEEK(MCGC)
             LIN = LIN + 1
             @ LIN,1 SAY LEFT(RAZ,40)
             CHIMP = .F.
           ENDIF  
         ENDIF
*
*
         SELE CADWORK
*        IF CTOD(VENC) <> KEY2 .OR. LIN = 5
           @ LIN,42 SAY VENC
           STOR CTOD(VENC) TO KEY2
*        ENDIF
*
         @ LIN,51 SAY PAG
         MVDP = (VDP - DESC)

         IF PAG = "  "
           DIAS = DATE() - CTOD(VENC)
         ELSE
           DIAS = CTOD(PAG) - CTOD(VENC)
         ENDIF

         TTDIAS = TTDIAS + DIAS
         ATOTVAL = (ATOTVAL + ((MVDP - VALDEV) * DIAS))
         CTOTVAL = (CTOTVAL + (MVDP - VALDEV))

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

         @ LIN,PCOL()+1 SAY DIAS PICTURE "99999"
         @ LIN,PCOL()+1 SAY EMIS
         @ LIN,PCOL()+1 SAY NDUP
         @ LIN,PCOL()+1 SAY MVDP PICT '9,999,999,999.99'
         TVAL = (TVAL + (MVDP - VALDEV))
         @ LIN,PCOL()+1 SAY TVAL PICT '99,999,999,999.99'
         SKIP
*
         LIN = LIN + 1
       ENDDO
       
       IF QTATR <> 0
         @ LIN+1,15 SAY "NOTAS COM ATRASO :"
         @ PROW()  ,PCOL()+1 SAY QTATR PICTURE "999999"
         @ PROW()  ,PCOL()+5 SAY "ATRASO MEDIO :"

         IF QTATR <> 0
           @ PROW()  ,PCOL()+1 SAY TTDIAS / QTATR PICTURE "99999"
           @ PROW()  ,PCOL()+1 SAY ATOTVAL / CTOTVAL PICTURE "99999"
           STOR 0.00 TO ATOTVAL , CTOTVAL 
         ENDIF

         @ LIN+2,15 SAY "TOTAL DE NOTAS   :"
         @ PROW()  ,PCOL()+1 SAY QTTOT+QTATR PICTURE "999999"
         @ PROW()  ,PCOL()+5 SAY "VALOR TOTAL FATURAMENTO :"
         @ PROW()  ,PCOL()+1 SAY (TOTVAL + TVAL) PICT '99,999,999,999.99'
         LIN = LIN + 3
       ENDIF

       IF LIN > 60
         lin = 5   &&EJECT
       ENDIF

*     ENDDO
   SELE CADWORK
   
ENDDO

IF PROW() > 53
  lin = 5
ENDIF

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