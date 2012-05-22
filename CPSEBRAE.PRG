CLEAR
DIRAV4  = "E:\SISTDOS\AV4\AV191"
DIRAVSP = "E:\SISTDOS\AVSP"
CLOSE ALL
cDrive = SPACE(10)
@10, 20 SAY "Selecione o drive: " GET cDrive
READ
IF LASTKEY() = 27
  RETURN
ENDIF
IF ! EMPTY(cDrive) THEN
  =Copiando()
ENDIF  
CLOSE ALL
SET DEFAULT TO &DIRAV4

RETURN

******************
PROCEDURE COPIANDO
******************

  SET DEFAULT TO &DIRAVSP
  
  =Copiar('CABPEDSP.DBF')
  =Copiar('DESCPED.DBF')
  =Copiar('ITEMSP.DBF')  
  =Copiar('PRECOSP.DBF')
    
  SET DEFAULT TO &DIRAV4
  =Copiar('TABREF.DBF')
  
RETURN


****************
PROCEDURE COPIAR
****************
  PARAMETER cNome
  PRIVATE Destino
  
  Destino = ALLTRIM(cDrive) + '\' + cNome
  
  COPY FILE &cNome TO &Destino
  
RETURN  