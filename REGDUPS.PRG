*---- Elimina os Registros Duplicados em CABECNF
SET TALK OFF
SET DELETED ON
USE CABECNF
INDEX ON NNF TO TST
DO WHILE ! EOF()
  ANT = NNF
  SKIP
  IF NNF = ANT
     SKIP -1
     DELETE
     SKIP
  ENDIF   
ENDDO    
SET TALK ON