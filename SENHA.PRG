* SENHA - PROGRAMA PARA TESTAR VALIDADE DAS SENHAS
* SISTEMA SCC
* ADAO ELI PEREIRA   -   14/07/93
    DO WHILE .T.
      vSENHA = SPACE(6)
      MENS = 'Digite sua Senha autorizada: '
      ? MENSAG(MENS)
      @ 23,POS SAY MENS GET vSENHA PICT [@!] COLOR W+/B+,X
      READ
      
      IF upper(vSENHA) <> "ADAO" THEN
        MENS = [Senha inv lida. Quer tentar de novo ? ]
        DO PEGRESP WITH [SN]
        IF MC = [N]
          CLOS DATA
          CANCEL
        ENDIF
        *SELE CTR_SENH
        *SEEK RTRIM(vNOMEPRG)+SPACE(8 - LEN(RTRIM(vNOMEPRG)))+vSENHA
        *IF !FOUND()
        *  MENS = [Senha inv lida. Quer tentar de novo ? ]
        *  DO PEGRESP WITH [SN]
        *  IF MC = [N]
        *    CLOS DATA
        *    CANCEL
        *  ENDIF
      ELSE
        @ 23,01 SAY SPACE(78)
        EXIT
      ENDIF
    ENDDO
