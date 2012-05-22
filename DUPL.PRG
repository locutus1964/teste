clear
set talk off
Inicio = {}
Fim = {}
@ 10, 10 SAY "DATA INICIAL:" GET Inicio 
@ 11, 10 SAY "DATA FINAL..:" GET Fim 
READ
set printer to arq.prn
set devi to printer
close data
use dupl 
INDEX on NDUP TO TST
SET FILTER TO EMIS >= INICIO .AND. EMIS <= FIM
GO TOP
ANT = NDUP
DO WHILE ! EOF() 
  DO WHILE NDUP - ANT > 1 
    ANT = ANT + 1
    @ ROW(),00 SAY ANT
  ENDDO   
  ANT = NDUP
  SKIP
ENDDO 	
set talk on
set devi to SCREEN
set printer to
close data