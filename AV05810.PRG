* AV05810 - CONSULTA DO RAZAO DO ESTOQUE - LIVRO MODELO 3
*
DO CABTELA WITH 0,0,24,79
@ 01,01 SAY SPACE(78)
CLOS DATA
*
SELE 0
USE PROD INDE PROD.IDX
SELE 0
USE SALDOS INDE SALDOS.IDX
SELE 0
USE MOD3 ORDER X_DATA
*
MSALI = 0             && SALDO INICIAL DO MOD3
MREF  = SPACE(04)
*
@ 01,02 SAY 'Ref.'
@ 03,02  SAY 'Transa‡„o'
@ 03,18 say ' Data'
@ 03,31 say 'Documento'
@ 03,44 say 'Entrada'
@ 03,57 say '  Sa¡da'
@ 03,70 say '  Saldo'
@ 04,30 SAY 'Saldo inicial'
*
DO WHILE .T.
*
SELE PROD
? MENSAG(ECODIGO)
@ 01,07 GET MREF
  READ
  
    IF LASTKEY() = 27
      CLOS DATA
      CLEAR
      RETURN
    ENDIF
      
    IF EMPTY(MREF)
      DO MENSAG WITH CALPHA
      =INKEY(0)
      LOOP
    ENDIF
*
@ 05,01 CLEAR TO 21,78
*
  SEEK MREF
    IF (EOF() .OR. BOF())
      ? MENSAG(CORRIJA)
      =INKEY(0)
      LOOP
    ELSE
      @ 01,12 SAY DES
        SELE SALDOS
          SALI = 0
            SEEK MREF
              IF !(EOF() .OR. BOF())
                MSALI = SALI
                @ 04,67 SAY MSALI PICT '9,999,999.99'
              ENDIF
                LIN = 5
*
          SELE MOD3
            SEEK MREF
              IF (EOF() .OR. BOF())
                DO MENSAG WITH 'Produto sem movimenta‡„o no Estoque - ENTER'
                =INKEY(0)
              ELSE
                DO WHILE MREF = REF

                  IF TDOC = '01'
                    TRANS = 'COMP/PRD'
                  ENDIF

                  IF TDOC = '51'
                    TRANS = 'EST.ENTR'
                  ENDIF

                  IF TDOC = '02' .OR. TDOC = '52'
                    TRANS = 'AJUSTE '
                  ENDIF

                  IF TDOC = '03'
                    TRANS = 'CANC.ENTR'
                  ENDIF

                  IF TDOC = '53'
                    TRANS = 'NF ENTREGA'
                  ENDIF

                  IF TDOC = '04'
                    TRANS = 'DEV CLIENTE'
                  ENDIF

                  IF TDOC = '54'
                    TRANS = 'CANC NF DEV'
                  ENDIF

                  @ LIN,2  SAY TRANS
                  @ LIN,17 SAY DATA
                  @ LIN,32 SAY NDOC PICT '999999'

                  IF VAL(TDOC) < 51
                    @ LIN,41 SAY QTD PICT '9,999,999.99'
                    STOR (MSALI + QTD) TO MSALI
                  ELSE
                    @ LIN,54 SAY QTD PICT '9,999,999.99'
                    STOR (MSALI - QTD) TO MSALI
                  ENDIF

                  SKIP

                  IF REF <> MREF
                    @ LIN,67 SAY MSALI PICT '9,999,999.99'
                    MSALI = 0
                  ENDIF

                  LIN = LIN + 1

                  IF LIN > 21
                    LIN = 5
                    DO mensag WITH 'Entre qualquer tecla p/continuar'
                    =INKEY(0)
                    @ 05,01 CLEAR TO 21,78
                  ENDIF

                ENDDO
              ENDIF

              SELE PROD

    ENDIF
*
ENDDO
