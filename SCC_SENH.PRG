* SCC_SENH - CADASTRAMENTO DE SENHAS
* SISTEMA SCC
* ADAO ELI PEREIRA - 27-08-90
*
vNOMEPRG = 'SCC_SENH'
SELE 0
  USE CTR_SENH INDE CTR_SENH.IDX

    DO WHILE .T.
      vSENHA = SPACE(6)
      MENS = 'Digite sua Senha autorizada: '
      ? MENSAG(MENS)
      @ 23,POS SAY MENS GET vSENHA PICT [@!] COLOR W+/B+,X
      READ
          SEEK RTRIM(vNOMEPRG)+SPACE(8 - LEN(RTRIM(vNOMEPRG)))+vSENHA
          IF !FOUND()
            MENS = [Senha inv lida. Quer tentar de novo ? ]
            DO PEGRESP WITH [SN]
            IF MC = [N]
              CLOS DATA
              CANCEL
            ENDIF
          ELSE
            @ 23,01 SAY SPACE(78)
              EXIT
          ENDIF
    ENDDO


vNOMEPRG = SPACE(08)
vSENHA   = SPACE(06)
*
? JANELA(18,52,21,78)
? BORDA(18,52,21,78)
@ 19,55 SAY 'Programa   :'
@ 20,55 SAY 'Senha      :'

SAVE SCRE TO TELA
DO WHILE .T.
? MENSAG('Informe o Nome do Programa a criar Senha de Acesso')
@ 19,68 get Vnomeprg pict '@!'
  READ
  IF LASTKEY() = 27
    EXIT
  ENDIF
@ 20,68 get vSENHA PICT '@!'
  read
  SEEK RTRIM(vNOMEPRG)+SPACE(8-LEN(RTRIM(vNOMEPRG)))+vSENHA
    IF FOUND()
      MENS = 'Senha j  cadastrada - Exclui ?'
      DO PEGRESP WITH [SN]
      IF MC = 'S'
        DELE
      ELSE
        LOOP
      ENDIF
    ELSE
      APPE BLAN
      REPL PROGRAMA WITH vNOMEPRG
      REPL SENHA    WITH vSENHA
    ENDIF
*
  MENS = 'Continua a atualiza‡„o das Senhas ?'
  DO PEGRESP WITH [SN]
    IF MC = 'N'
      REST SCRE FROM TELA
      EXIT
    ENDIF
ENDDO
CLEAR
CLOS DATA
RETURN
