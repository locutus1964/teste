CLEAR
USE CLIEN  
nReg = 0
DO WHILE ! EOF()
  IF UPPER(EST) = 'SC'
    nReg = nReg + 1 
    REPLACE DDD1 WITH SUBSTR(DDD1,1,3)
    REPLACE DDD2 WITH SUBSTR(DDD2,1,3) 
  ENDIF  
  SKIP
ENDDO
? STR(nReg,6) + ' Atualizados'