* AV05998 - EXCLUSAO DE TABELA DE PRECOS
* SISTEMA ADMINISTRACAO DE VENDAS
* ADAO ELI PEREIRA - 09-07-90
*
MTAB = SPACE(3)
MC   = SPACE(1)
TTAB = SPACE(1)
*
? JANELA(07,40,12,76)
? BORDA(07,40,12,76)
*
*
  ? MENSAG(ECODIGO)
  @ 09,43 SAY 'Informe C¢digo da Tabela  'GET MTAB
    READ
      IF LASTKEY() = 27
        RETURN
      ENDIF  
*
  @ 10,43 SAY 'Tabela (T) ou Codigos (C) 'GET TTAB PICT '!' VALID(TTAB$'CT')
    READ
*
  ? MENSAG(DADCORR)
  @ 23,POS SAY DADCORR GET MC PICT '!' VALID(MC$'SN')
    READ
    IF MC = 'N'
      RETURN
    ENDIF
*
  IF TTAB = 'T'
    USE TABPRE INDEX TABPRE
  ELSE
    USE CODPRE INDEX CODPRE
  ENDIF
*
  SEEK MTAB
  IF .NOT. (EOF() .OR. BOF())
    DELE ALL FOR TAB = MTAB
    PACK
      IF TTAB = 'T'
        INDEX ON TAB + REF TO TABPRE
      ELSE
        INDEX ON TAB + CPR TO CODPRE
      ENDIF
  ELSE
      MENS ="Tabela n„o est  cadastrada - ENTER" 
      ? MENSAG(MENS)
      =INKEY(0)
      CLOS DATA
      RETURN
  ENDIF
*
CLOS DATA
RETURN