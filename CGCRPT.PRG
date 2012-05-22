CLEAR
CLOSE ALL
SET TALK OFF
SET STATUS OFF
N = ''
SELE 0
USE CLIEN ORDER P_CGC
SELE CLIEN
GO TOP
FIM = .F.
DO WHILE ! EOF() .AND. ! FIM
  MCGC = CGC
  First = .T.
  @ 0, 0 SAY 'Buscando...'
  DO WHILE ! EOF() .AND. ! FIM .AND. MCGC = CGC
    IF ! First THEN
      BROW FIELD CGC, GUE, RAZ TITLE '<CTRL+W> para gravar e continuar a busca  <ESC> para encerrar o programa'
      IF LASTKEY() = 27
        FIM = .T.
      ENDIF    
    ENDIF
    SELECT CLIEN
    SKIP
    First = .F.
  ENDDO
ENDDO
CLOSE DATA
CLEAR
SET TALK ON
SET STATUS ON
