CLEAR
CLOSE ALL
SET TALK OFF
SET STATUS OFF
N = ''
SELE 0
USE CLIEN ORDER X_GUE
COPY FIELD CGC,GUE,RAZ STRU EXTE TO T
sele 0
CREATE TMP FROM T

SELE CLIEN
GO TOP
FIM = .F.
DO WHILE ! EOF() .AND. ! FIM
  MCGC = CGC
  MGUE = GUE
  MRAZ = RAZ
  First = .T.
  @ 0, 0 SAY 'Buscando...'
  A = .T.
  DO WHILE ! EOF() .AND. ! FIM .AND. MGUE = GUE
    IF ! First THEN
*      BROW FIELD CGC, GUE, RAZ TITLE '<CTRL+W> para gravar e continuar a busca  <ESC> para encerrar o programa'
*      IF LASTKEY() = 27
*        FIM = .T.
*      ENDIF    
      IF A THEN
        SELECT TMP
        APPEND BLANK
        REPLACE CGC WITH MCGC
        REPLACE GUE WITH MGUE
        REPLACE RAZ WITH MRAZ
        A = .F.
      ENDIF    
      SELECT TMP
      APPEND BLANK
      REPLACE CGC WITH CLIEN->CGC
      REPLACE GUE WITH CLIEN->GUE
      REPLACE RAZ WITH CLIEN->RAZ
    ENDIF
    SELECT CLIEN
    SKIP
    First = .F.
  ENDDO
ENDDO
SELECT TMP
INDEX ON GUE TAG XGUE
LIST GUE,CGC,RAZ TO ARQ.TXT
CLOSE DATA
CLEAR
SET TALK ON
SET STATUS ON
