* FECHAMES - ENCERRA O MES E ALTERA COMPETENCIA DO PARAMETRO
*
USE PARAM
  IF UCOMPET = PCOMPET
     ? MENSAG('Mes j  foi encerrado. ENTER para sair')
     =INKEY(0)
     RETURN
  ENDIF

MC = ' '
MENS = 'Tem certeza que o mes pode ser encerrado? (S/N) '
? MENSAG(MENS)
@ 23,POS SAY MENS GET MC PICT '!' VALID(MC$'SN')
  READ

  IF MC = 'N'
     RETURN
  ENDIF

  IF MC = 'S'
    MENS = 'Poossoo sseerr uumm ppoouuccoo cchhaattoo?? Confirme novamente (SN) '
    MC   = ' '
    ? MENSAG(MENS)
    @ 23,POS SAY MENS GET MC PICT '!' VALID(MC$'SN')
      READ
  ENDIF

  IF MC = 'N'
    RETURN
  ELSE
    REPL UCOMPET WITH PCOMPET
    CLOS DATA
  ENDIF
