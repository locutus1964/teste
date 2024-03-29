CLOSE ALL
SET TALK OFF
NOVA = '08/99'
HOJE = DATE()
SELE 0
USE TABREF ORDER P_TABREF
SET FILTER TO TABREF = '04/99'
GO TOP
DO WHILE ! EOF()
  MREF = REF
  MPRECO = PRECO
  MSIGLA = SIGLA
  R = RECNO()
  IF ! SEEK(NOVA+REF, 'TABREF') 
    APPEND BLANK
    REPLACE TABREF WITH NOVA
    REPLACE REF    WITH MREF
    REPLACE PRECO  WITH MPRECO
    REPLACE SIGLA  WITH MSIGLA
    REPLACE DTALT  WITH HOJE
    GO R
  ENDIF
  SKIP  
ENDDO
CLOSE ALL
=TABREFCDX()
CLOSE ALL
SET TALK ON

******************
PROCEDURE TABREFCDX
******************

  USE TABREF
  PACK
  DELETE TAG ALL
  INDEX ON DTALT TAG X_DATA DESC
  INDEX ON TABREF+REF TAG P_TABREF

RETURN
