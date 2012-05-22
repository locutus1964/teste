CLEAR
CLOSE DATA
SET TALK OFF
SET SAFETY OFF
DIMENSION COND(4)
USE CADNF 
INDEX ON NNF TO TST1
SELE 0
USE DUPL 
INDEX ON STR(NDUP)+STR(PDUP) TO TST2
SELE 0
USE DBF\DUPL ALIAS VELHO
INDEX ON STR(NDUP)+STR(PDUP)+DTOS(CTOD(EMIS)) TO TST3
SELE CADNF
GO TOP
DO WHILE ! EOF()
  * Procura no arquivo de duplicatas atual
  NOTA = NNF
  EMISSAO = CTOD(DEMI)
  COND(1) = CP1
  COND(2) = CP2
  COND(3) = CP3
  COND(4) = CP4
  FOR I = 1 TO 4
    SELE CADNF
    IF ! SEEK( STR(NOTA)+STR(I), 'DUPL' )
       IF SEEK( STR(NOTA)+STR(I)+DTOS(EMISSAO), 'VELHO' )
          SELE DUPL
          APPEND FROM DBF\DUPL FOR NDUP=NOTA .AND.;
                                   PDUP=I .AND.;
                                   CTOD(EMIS)=EMISSAO
       ENDIF                            
    ENDIF
  NEXT I                              
  SELE CADNF
  SKIP
  @ 10, 00 SAY 'NOTA: ' + STR(NNF)
ENDDO  
CLOSE DATA       
SET TALK ON           
SET SAFETY ON

            
   