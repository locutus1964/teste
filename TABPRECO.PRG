set talk off
CLOSE DATA
CLEAR
SET STAT ON
DIMENSION AGRUPO(5)
SELE 0
USE PROD ORDER P_REF
SELE 0
USE TABREF ORDER P_TABREF

MTABOLD = SPACE(05)
MTABNEW = SPACE(05)
AGRUPO[1] = 0.0
AGRUPO[2] = 0.0
AGRUPO[3] = 0.0
AGRUPO[4] = 0.0
AGRUPO[5] = 0.0
@10, 20 SAY 'TABELA ANT.:' GET MTABOLD PICT [99/99]
@11, 20 SAY 'TABELA NOVA:' GET MTABNEW PICT [99/99]
@14, 21 SAY 'GRUPOS:   00    10    20    30    40'
@15, 21 SAY 'REAJ.%:' 
@15, 29 GET AGRUPO[1] PICT [99.99]
@15, 35 GET AGRUPO[2] PICT [99.99]
@15, 41 GET AGRUPO[3] PICT [99.99]
@15, 47 GET AGRUPO[4] PICT [99.99]
@15, 53 GET AGRUPO[5] PICT [99.99]
READ

if lastkey() = 27
  close data
  return
endif

MRESP = 'N'
@17, 20 SAY 'Confirma [S/N]?' GET MRESP PICT [@!]
READ

IF MRESP = 'S'
  GO TOP
  DO WHILE !EOF()
    IF TABREF = MTABOLD .AND. SEEK(REF, 'PROD') THEN
      MPORC = 0
      DO CASE 
        CASE PROD->GRUPO = '00'         
          MPORC = AGRUPO[1]
        CASE PROD->GRUPO = '10'         
          MPORC = AGRUPO[2]
        CASE PROD->GRUPO = '20'         
          MPORC = AGRUPO[3]
        CASE PROD->GRUPO = '30'         
          MPORC = AGRUPO[4]
        CASE PROD->GRUPO = '40'         
          MPORC = AGRUPO[5]
      ENDCASE
      NREC = RECNO()
      MREF   = REF
      MPRECO = CALCPRECO()
      IF ! SEEK(MTABNEW+MREF)
        APPEND BLANK
      ENDIF  
      REPLACE TABREF WITH MTABNEW
      REPLACE REF    WITH MREF
      REPLACE PRECO  WITH MPRECO
      REPLACE DTALT  WITH DATE()
      GO NREC
    ENDIF
    SKIP
  ENDDO
ENDIF

******************
FUNCTION CALCPRECO
******************

  mnovo = round(PRECO * (1 + (MPORC / 100)),2)
  minteiro = int(mnovo)
  mcentavo = str(mnovo - minteiro, 4, 2)
  c = VAL(substr(mcentavo,4,1))
  do case 
    case c >= 0 .and. c <= 2
      r = minteiro + val(substr(mcentavo, 1, 3))
    case c >= 3 .and. c <= 5
      r = minteiro + val(substr(mcentavo, 1, 3)) + 0.05
    case c >= 6 .and. c <= 9
      r = minteiro + val(substr(mcentavo, 1, 3)) + 0.10
  endcase
  
  mvalor = r
  
RETURN mvalor


  