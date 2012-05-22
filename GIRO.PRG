SET TALK OFF
CLEAR 
dInicio = {01/01/96}
dFim    = DATE()

Si = 0
Sf = 0
SELE 0
USE DUPL 
DO WHILE ! EOF()
  IF VENC = dInicio
    Si = Si + VDP
  ENDIF
  IF VENC = dfIM
    Sf = Sf + VDP
  ENDIF    
  SKIP
ENDDO
SELE 0
USE CABECNF
Vp = 0
DO WHILE ! EOF()
  IF DEMISSAO >= dInicio .AND. DEMISSAO <= dFim 
    Vp = Vp + VALPROD + VALIPI 
  ENDIF  
  SKIP
ENDDO
K = (Si + Vp - Sf) / ((Si + Sf) / 2)
N = dFim - dInicio
? "K=" + STR(K)
? "N="+ STR(N)
? "N/K=" + STR(N/K,5,3)
CLOSE DATABASE
SET TALK ON