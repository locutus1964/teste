CLOSE DATA
SET TALK OFF
SET SAFETY OFF
USE CADNF 
INDEX ON NNF TO TST1
SELE 0
USE DUPL 
INDEX ON NDUP TO TST2
SELE 0
USE DBF\DUPL ALIAS VELHO
INDEX ON STR(NDUP)+DTOS(CTOD(EMIS)) TO TST3
SELE CADNF
GO TOP
TOTIMP = 0
CONT = 0
DO WHILE ! EOF()
  * Procura no arquivo de duplicatas atual
  NOTA = NNF
  EMISSAO = CTOD(DEMI)
  IF ! SEEK( NNF, 'DUPL' )
    * se nao encontrar, procura no arquivo antigo
    IF SEEK( STR(NNF)+DTOS(CTOD(DEMI)), 'VELHO' )
      * Se encontrou, copia todas as duplicatas para o arquivo atual com mesma data de emissao
      SELE DUPL
      APPEND FROM DBF\DUPL FOR NDUP=NOTA .AND. CTOD(EMIS) = EMISSAO
    ENDIF
  ENDIF   
  SELE CADNF
  SKIP
ENDDO  
CLOSE DATA       
SET TALK ON           
SET SAFETY ON

            
   