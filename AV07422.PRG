* AV07421 - CALCULO DO VALOR E VENCIMENTO DE NOTAS FISCAIS
* SISTEMA ADMINISTRACAO DE VENDAS
* ADAO ELI PEREIRA - 14-07-90  -  14/12/91

STOR 0.00 TO DPAR, MVDP, VDUP1, VDUP2, VDUP3, VDUP4, VPAR, AVPAR 
STOR 0    TO QPAR, QDIA, D1, D2, D3   
FORM  = SPACE(02)
STOR SPACE(08) TO VENC1, VENC2, VENC3, VENC4 
STOR SPACE(08) TO VENDUP1, VENDUP2, VENDUP3, VENDUP4
*
D1 = DAY(DATE())
D2 = MONTH(DATE())
*
INICIO = (DATE())
IF MEDUP <>  'S'
  RETURN
ENDIF
*
IF MCP1 = 0 .OR. MCPD = 'VIS'
  INICIO = DATE()
*  VENDUP1 = DTOC(DATE())
*  VDUP1   = (GTOT + GIPI)
*  RETURN
ENDIF

IF MCPD = 'DFD'
  IF D1 < 10
    INICIO = (DATE()) + (10-D1)
  ELSE
    IF D2 = 02
      INICIO = (DATE()) + (28-D1)
    ELSE
      INICIO = (DATE()) + (30-D1)
    ENDIF
  ENDIF
ENDIF
*
IF MCPD = 'DFQ'
  IF D1 < 15
    INICIO = (DATE()) + (15-D1)
  ELSE
    IF D2 = 02
      INICIO = (DATE()) + (28-D1)
    ELSE
      INICIO = (DATE()) + (30-D1)
    ENDIF
  ENDIF
ENDIF
*
IF MCPD = 'DFM'
  IF D2 = 02
    INICIO = (DATE()) + (28-D1)
  ELSE
    INICIO = (DATE()) + (30-D1)
  ENDIF
ENDIF
*
MAIOR = 0
QPAR  = 1
CPAC  = 1
IF MCP2 > 0
  QPAR = QPAR + 1
  IF MCP3 > 0
    QPAR = QPAR + 1
    IF MCP4 > 0
      QPAR = QPAR + 1
    ENDIF
  ENDIF
ENDIF
*
CDPAR = INT(GTOT*(MPE1/100))+INT(GTOT*(MPE2/100))+INT(GTOT*(MPE3/100))+INT(GTOT*(MPE4/100))
*
IF CDPAR <> GTOT
  DPAR = (GTOT - CDPAR)
ENDIF
*
V1 = VAL(SUBSTR(MVES,1,2))
MAIOR = V1
V2 = VAL(SUBSTR(MVES,4,2))
IF V2 > 0
  MAIOR = V2
ENDIF
V3 = VAL(SUBSTR(MVES,7,2))
IF V3 > 0
  MAIOR = V3
ENDIF
V4 = VAL(SUBSTR(MVES,10,2))
IF V4 > 0
  MAIOR = V4
ENDIF
V5 = VAL(SUBSTR(MVES,13,2))
IF V5 > 0
  MAIOR = V5
ENDIF
V6 = VAL(SUBSTR(MVES,16,2))
IF V6 > 0
  MAIOR = V6
ENDIF
*
STOR SPACE(01) TO D1,D2,D3,D4
DO WHILE QPAR > 0.00
* ---------------------------------------------------------------------------
  CPA = STR(CPAC,2)
*  IF MCP1 > 0 
    VDUP1 = (GIPI + INT(GTOT * (MPE1 / 100)) + DPAR)
    VENC = INICIO + MCP1
    MD1 = DAY(VENC)
    X   = MONTH(VENC)
    DO MAIOR
    IF MCPD = 'ANT' .OR. MCPD = 'VIS'
      VENDUP1 = DTOC(DATE())
    ELSE
      VENDUP1 = DTOC(VENC)
    ENDIF
*  ENDIF
*
    IF MCP2 > 0 .AND. D2 = ' '
      VDUP2 = INT(GTOT * (MPE2 / 100))
      VENC = INICIO + MCP2
      MD1 = DAY(VENC)
      X   = MONTH(VENC)
      DO MAIOR
      IF MCPD = 'ANT'
        VENDUP2 = 'ANTECIPADO'
      ELSE
        VENDUP2 = DTOC(VENC)
      ENDIF
    ENDIF
*
      IF MCP3 > 0 .AND. D3 = ' '
        VDUP3 = INT(GTOT * (MPE3 / 100))
        VENC  = INICIO + MCP3
        MD1   = DAY(VENC)
        X     = MONTH(VENC)
        DO MAIOR
        IF MCPD = 'ANT'
          VENDUP3 = 'ANTECIPADO'
        ELSE
          VENDUP3 = DTOC(VENC)
        ENDIF
      ENDIF
*
        IF MCP4 > 0 .AND. D4 = ' '
          VDUP4 = INT(GTOT * (MPE4 / 100))
          VENC = INICIO + MCP4
          MD1   = DAY(VENC)
          X     = MONTH(VENC)
          DO MAIOR
          IF MCPD = 'ANT'
            VENDUP4 = 'ANTECIPADO'
          ELSE
            VENDUP4 = DTOC(VENC)
          ENDIF
        ENDIF

*
  CPAC = CPAC + 1
  QPAR = (QPAR - 1)
*
ENDDO
*
PROCEDURE MAIOR

  IF MAIOR > 0
    IF MD1 > MAIOR
      IF X = 2
        DIF = 28
      ELSE
        IF X = 9 .OR. X = 4 .OR. X = 6 .OR. X = 11
          DIF = 30
        ELSE
          DIF = 31
        ENDIF
      ENDIF
      VENC = VENC + ((DIF + V1) - MD1)
    ELSE
      IF V1 >= MD1
        VENC = VENC + (V1 - MD1)
        STOR 0 TO V2, V3, V4, V5, V6
      ELSE
        IF V2 >= MD1
          VENC = VENC + (V2 - MD1)
          STOR 0 TO V3, V4, V5, V6
        ELSE
          IF V3 >= MD1
            VENC = VENC + (V3 - MD1)
            STOR 0 TO V4, V5, V6
          ELSE
            IF V4 >= MD1
              VENC = VENC + (V4 - MD1)
              STOR 0 TO V5, V6 
            ELSE
              IF V5 >= MD1
                VENC = VENC + (V5 - MD1)
                V6   = 0
              ELSE
                IF V6 >= MD1
                  VENC = VENC + (V6 - MD1)
                ENDIF
              ENDIF
            ENDIF
          ENDIF
        ENDIF
      ENDIF
    ENDIF
  ENDIF

RETURN
