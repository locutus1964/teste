* AV05820 - CRIACAO DE SALDOS PARA MODELO 3
*
  USE MOD3
    ZAP
  USE SALDOS
    ZAP
    CLOS DATA
  SELE 0
    USE SALDOS
  SELE 0
    USE PROD
    
    SELE PROD
      DO WHILE .NOT. EOF()
        STOR REF TO MREF
        STOR SAL TO MSAL
        STOR PR3 TO MPR3
          SELE SALDOS
            APPE BLAN
              REPL REF WITH MREF
              REPL SALI WITH MSAL
              REPL PR3 WITH MPR3
                SELE PROD
            SKIP
      ENDDO
  CLOS DATA
    USE SALDOS
      INDEX ON REF TO SALDOS.IDX
    USE MOD3
      INDEX ON REF + DATA + TDOC + STR(NDOC,6) TO MOD3.IDX
    CLOS DATA
  RETURN