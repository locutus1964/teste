=MENSAG('Insira o disquete com os dados de atualiza‡Æo no drive A: e tecle <ENTER>')
k=INKEY(0)
IF k = 13
  =MENSAG('Atualizando Clientes...')
  SELE 0
  USE A:CLINEW ALIAS FONTE
  SELE 0
  USE CLIEN ORDER X_GUE
  SELE FONTE
  GO TOP
  DO WHILE ! EOF()
    IF SEEK(GUE, 'CLIEN')
      REPLACE CLIEN->AVSP  WITH .T.
      REPLACE CLIEN->ULCSP WITH ULCSP
    ENDIF  
    SKIP
  ENDDO
ENDIF  
CLOSE DATA
=MENSAG('')