CLOSE ALL
SET TALK OFF
SET STATUS ON
SELE 0
USE CABECNF 
SELE 0 
USE CADNF ORDER F_NNF
COPY FIELD NNF,PED,QTD,PREU STRU TO TMP
SELE 0
USE TMP
SELE CABECNF
SET FILTER TO CANCELADA = .F.
GO TOP
DO WHILE !EOF()
  MPED = PED
  MNNF = NNF
  SELE CADNF
  =SEEK(MNNF)
  DO WHILE !EOF() .AND. MNNF = NNF
    IF MPED <> PED   
      NUM = RECNO('CADNF')
      SELE TMP
      APPEND BLANK
      REPLACE NNF WITH CADNF->NNF
      REPLACE PED WITH CADNF->PED
      REPLACE QTD WITH CADNF->QTD
      REPLACE PREU WITH CADNF->PREU
    ENDIF  
    SELE CADNF
    SKIP
  ENDDO    
  SELE CABECNF
  SKIP
ENDDO
CLOSE ALL
SET TALK ON
SET STATUS ON
