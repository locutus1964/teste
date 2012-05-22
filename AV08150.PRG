* AV08400 - CANCELAMENTO DE DUPLICATAS
* SISTEMA CONTAS A RECEBER
* ADAO ELI PEREIRA - 28/02/87 - 6/5/90 - 10/07/92 - 02/10/93
*
USE PARAM
MPCOMPET = VAL(SUBSTR(PCOMPET,4,4)+SUBSTR(PCOMPET,1,2))
CLOS DATA

SELE A
  USE DUPL order xdupano
*
? CABTELA(0,0,24,79)
*
MENS = 'Cancelamento de Duplicatas'
? MENSAGR(MENS)
@ 01,POS SAY MENS

@ 08,17 SAY 'N£mero da Duplicata                         Ano Emis.'
@ 10,17 say 'Parcela'
@ 12,17 SAY 'Valor da Duplicata'
@ 14,17 SAY 'Data do Vencimento (DD/MM/AA)'
@ 16,17 say 'Data do Cancelamento (DD/MM/AA'
@ 18,17 SAY 'Obs:'
*
*
DO WHILE .T.
*
  MVENC  = ctod('')
  MPAG   = ctod('')
  MDTCANCEL = ctod('')
  PARDUP = 0
  MVDP   = 0.00
  NUMDUP = 0
  VANO   = 1996
  MOBS   = SPACE(40)
  ? MENSAG('[ESC] para encerrar atualiza‡„o')
  @ 08,50 GET NUMDUP PICT '999999'
  @ 08,71 GET VANO   PICT '9999'
    READ
    IF LASTKEY() = 27
      CLOS DATA
      CLEAR
      RETURN
    ENDIF
    IF NUMDUP = 0
      ? MENSAG('Duplicata n„o pode ser zeros - ENTER')
      =INKEY(0)
      LOOP
    ENDIF
*
  @ 10,50 GET PARDUP PICT '9'
    READ
*
    IF !SEEK( STR(NUMDUP,6) + STR(PARDUP,1) + str(VANO,4))
      ? MENSAG('Duplicata/Parcela/Ano nao encontrada - ENTER')
      =INKEY(0)
      LOOP
    ELSE
      @ 12,50 SAY VDP PICT '9,999,999,999.99' COLOR &CORVER
      @ 14,50 SAY VENC COLOR &CORVER
        MVENC = VENC
      @ 16,50 SAY DTCANCEL COLOR &CORVER
        MDTCANCEL = DTCANCEL
      @ 18,22 SAY OBS COLOR &CORVER
        MDEMIS = EMIS

      IF ! EMPTY(DTCANCEL)
        MC = ' '
        MENS = 'Duplicata cancelada em ' + DTOC(DTCANCEL) +' - Exclui, Descancela ou Sai ?'
        DO PEGRESP WITH [EDS]
          IF MC = 'D'
            IF VAL(SUBSTR(DTOS(MDTCANCEL),5,4)) <= MPCOMPET
              ? MENSAG('N„o pode Descancelar duplicata j  contabilizada - ENTER')
              =INKEY(0)
              LOOP
            ENDIF
            REPL DTCANCEL WITH CTOD('')
            LOOP
          ENDIF

          IF MC = 'E'
            IF VAL(SUBSTR(DTOS(MDTCANCEL),5,4)) <= MPCOMPET
              ? MENSAG('N„o pode Excluir duplicata j  contabilizada - ENTER')
              =INKEY(0)
              LOOP
            ENDIF
            DELE
            LOOP
          ENDIF

          IF MC = 'S'
            LOOP
          ENDIF
      ENDIF

      IF ! EMPTY(PAG)
        MENS = 'Duplicata recebida em '+ DTOC(PAG) + ' - ENTER p/sair'
        ? MENSAG(MENS)
        =INKEY(0)
        LOOP
      ENDIF

    ENDIF
*
    @ 12,50 SAY VDP PICT '9,999,999,999.99' COLOR &CORVER
    @ 14,50 SAY VENC   COLOR &CORVER
    @ 16,50 SAY MDTCANCEL  COLOR &CORVER
    @ 18,22 SAY OBS        COLOR &CORVER
      STOR OBS TO MOBS
*
    MATU = ' '
    MENS = 'Cancela a Duplicata (C), Exclui (E) ou Sai (S) ?'
    DO PEGRESP WITH [CES]
      MATU = MC
*
      IF MATU = 'S'
        LOOP
      ENDIF
*
      IF MATU = 'E' .OR. MATU = 'C'
        IF VAL(SUBS(DTOS(MDEMIS), 5, 4)) <= MPCOMPET
          ? MENSAG('Duplicata j  contabilizada - n„o pode Excluir/Cancelar - ENTER')
          =INKEY(0)
          LOOP
        ENDIF
      ENDIF

      IF MATU = 'E'
        MC = ' '
        MENS = 'Tem certeza que pode excluir ?'
        DO PEGRESP WITH [NS]
          IF MC = 'N'
            LOOP
          ENDIF
          DELE
          LOOP
      ENDIF
*
      IF MATU = 'C'
*
        @ 16,50 GET MDTCANCEL PICT '99/99/99'
          READ
          ? FDATA(MDTCANCEL)
          IF TDATA <> ' '
            LOOP
          ENDIF

        @ 18,22 GET MOBS
          READ
*
        MENS = DADCORR
        DO PEGRESP WITH [SN]
          IF MC = 'N'
            LOOP
          ENDIF

      ENDIF

      REPL DTCANCEL WITH MDTCANCEL,OBS WITH MOBS
*
ENDDO
CLOSE DATA
RETURN
