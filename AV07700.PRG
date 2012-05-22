* AV07700 - EMBARQUES OU CANCELAMENTO DE NOTAS FISCAIS - 13-01-87
* SISTEMA DE ADMINISTRACAO DE VENDAS
* ADAO ELI PEREIRA - 6/5/90
*
MDEL = 0
*
=CABTELA(0,0,24,79)
*
MENS = 'Embarques e Cancelamentos de Notas Fiscais'
=MENSAGR(MENS)
@01,POS SAY MENS
*
@10,5 say '                     A  t  e  n  ‡  „  o'

@14,5 say 'Para cancelar uma Nota Fiscal j  embarcada, precisa cancelar o embarque'
@16,5 say 'e depois cancelar a Nota Fiscal, portanto em duas opera‡”es.'
*
=MENSAG('Pressione qualquer tecla para continuar...')
=INKEY(0)
*
clos data
@04,01 CLEAR TO 21,78
*
STOR SPACE(03) TO mcpd, mven
STOR SPACE(20) TO mcgc, key1
ment  = space(08)
mpint = space(06)
mref  = space(10)
STOR 0 TO mqtd, mdel, mcom, ncom, msal, nsal, mfme, nfme, mfac
STOR 0 TO nfac, mped, mnnf, mcp1, mcp2, mcp3, mcp4, mpe1, mpe2
STOR 0 TO mpe3, mpe4, mnfi, mnnf, mipi, mval, mdeb, ndeb
*
SELE 0
USE CADNF index CADNF,NFREF,NFPED
SELE 0
USE CLIEN INDEX CLIEN.IDX
SELE 0
USE PROD  INDEX PROD
SELE 0
USE PEDID INDEX PEDIDM
SELE 0
USE DUPL  INDEX DUPL
@05,15 say 'N£mero da Nota Fiscal'
@07,15 say 'Data da emiss„o da N.Fiscal'
DO WHILE .T.
   @07,50 say SPACE(08)
   @08,01 CLEAR TO 21,78
   MNFI = 0
   sele CADNF
   =mensag(ECODIGO)
   @05,50 get MNNF pict '999999' valid mnnf > 0
   read
   if lastkey() = 27
      if mdel = 1
         MENS = 'Houve exclus”es. Compacta o Arquivo de Notas Fiscais=(S/N) '
         DO PEGRESP WITH [SN]
         IF MC = 'S'
            sele CADNF
            pack
         ENDIF
      endif
      exit
   ENDIF
   save screen
   seek str(mnnf,6)
   =mostra(10,02,21,78)
   browse fields nnf:H='N§ Nf.',demi:H='Dt. Emiss„o', ref:H='Referˆncia',des:H='Descri‡„o' IN WINDOW TELA
   MNNF = NNF
   deactivate window tela
   rest screen

   MBEST = BEST
   MEDUP = EDUP
   MNOPE = NOPE
   MENT  = ENT
   MDEMI = DEMI
   key1 = str(ped,6) + ref
   mref = ref
   mqtd = qtd
   mval = 0.00
   mipi = 0.00
   mcgc = cgc
   mdesconto = desconto
   @07,50 say MDEMI
   IF CANCELNF <> SPACE(08)
      @09,15 SAY 'Data do cancelamento da N.Fiscal'
      @09,50 SAY CANCELNF
      =MENSAG('Nota Fiscal foi cancelada em '+cancelnf+ ' - ENTER')
      =INKEY(0)
      LOOP
   ENDIF
   IF MENT <> SPACE(08)
      @09,15 SAY 'Data da entrega'
      @09,50 SAY MENT
      =MENSAG('Nota Fiscal com entrega confirmada em '+ment+ ' - ENTER')
      =INKEY(0)
      MENS = 'Vai cancelar o embarque da Nota Fiscal ?'
      DO PEGRESP WITH [NS]
      IF MC = 'N' .or. empty(mc)
         LOOP
      ELSE
         do while str(NNF,6) = str(MNNF,6) .and. !eof()
            MREF = REF
            MQTD = QTD
            sele PEDID
            if seek(STR(PED,6) + MREF)
               repl nnf with 0,dent with space(8),CENT WITH SPACE(1)
            ENDIF
            sele CADNF
            repl ent with space(8)
            skip
         ENDDO
         sele DUPL
         IF SEEK(STR(MNNF,6))
            DO WHILE STR(NDUP,6) = STR(MNNF,6)
               REPL ENTREGA WITH SPACE(08)
               SKIP
            ENDDO
         ENDIF
      ENDIF
      LOOP
   ENDIF
   **------------------------------------------------------------
   @09,15 say space(60)
   IF MENT = SPACE(08)
      mens = 'Confirma a Entrega [ E ], Cancela a NF [ C ] ou Sai [ S ] ?'
      DO PEGRESP WITH [ECS]
      =mensagr(mens)
      MATU = MC
      IF MATU = 'S'
         LOOP
      ENDIF
   ENDIF
   **-------------------------------------------------------------
   **           E N T R E G A    A O    C L I E N T E
   **-------------------------------------------------------------
   if matu = 'E'
      sele CADNF
      @09,15 say 'Data da Entrega ao Cliente '
      DO WHILE .T.
         @09,50  get ment pict '99/99/99'
         read
         =FDATA(MENT)
         IF TDATA = ' '
            IF CTOD(MENT) < CTOD(MDEMI)
               =MENSAG('Data n„o pode ser menor que emiss„o da N.F. - ENTER')
               =INKEY(0)
               LOOP
            ENDIF
            EXIT
         ENDIF
      ENDDO
      @11,15 SAY 'Confirme N£mero da Nota Fiscal'
      @11,50 get mnfi pict '999999' valid mnfi = MNNF
      read
      if lastkey() = 27
         close data
         return
      endif
      do while str(NNF,6) = str(MNNF,6) .and. !eof()
         mped = ped
         key1 = (MCGC + STR(ped,6) + ref)
         mqtd = qtd
         mref = ref
         mval = (val + mval)
         mipi = (ipi + mipi)
         mbest = best
         medup = edup
         REPL NFI WITH MNFI, ENT WITH MENT
         sele PEDID
         if SEEK(str(MPED,6) + MREF)
            repl nnf with mnnf,dent with ment,CENT WITH SPACE(1)
         ENDIF
         sele PROD
         IF MBEST = 'S'
            IF SEEK(MREF)
               REPL COM WITH (COM - MQTD), SAL WITH (SAL - MQTD)
               REPL FME WITH (FME + MQTD), FAC WITH (FAC + MQTD)
            ENDIF
         ENDIF
         sele CADNF
         SKIP
      ENDDO
      sele DUPL
      if SEEK(STR(MNNF,6))
         DO WHILE STR(NDUP,6) = STR(MNNF,6)
            REPL ENTREGA WITH MENT
            SKIP
         ENDDO
      ENDIF
      LOOP
   ENDIF
   **------------------------------------------------------------------------
   **             C A N C E L A M E N T O    D E   N.  F I S C A L
   **------------------------------------------------------------------------
   IF MATU = 'C'
      MENS = 'Tem certeza que pode cancelar a Nota Fiscal ?'
      DO PEGRESP WITH [SN]
      IF MC = 'N'
         LOOP
      ENDIF
      sele CADNF
      @09,15 say 'Data do cancelamento da N.Fiscal'
      DO WHILE .T.
         @09,50  get ment pict '99/99/99'
         read
         =FDATA(MENT)
         IF TDATA = ' '
            IF CTOD(MENT) < CTOD(MDEMI)
               =MENSAG('Data n„o pode ser menor que emiss„o da N.F. - ENTER')
               =INKEY(0)
               LOOP
            ENDIF
            EXIT
         ENDIF
      ENDDO
      do while str(NNF,6) = str(MNNF,6) .and. !eof()
         key1 = STR(ped,6) + ref
         MREF = REF
         MQTD = QTD
         MPED = PED
         IF EDUP = 'S'
            mval = ((val + ipi + mval) - mdesconto)
            mfre = 0.00
            memb = 0.00
            mdesconto = 0.00
         ENDIF
         REPL CANCELNF WITH MENT
         sele PROD
         IF MBEST = 'S'
            IF SEEK(MREF)
               REPL PEA WITH (PEA + MQTD)
               REPL COM WITH (COM - MQTD)
            ENDIF
         ENDIF
         sele PEDID
         IF !SEEK(STR(MPED,6) + MREF)
            MENS = 'Nao encontrado '+key1
            =MENSAG(MENS)
            =INKEY(0)
            CLOS DATA
            RETURN
         ELSE
            REPL SAL WITH (SAL + MQTD)
         ENDIF
         sele CADNF
         skip
      ENDDO
      sele CLIEN
      IF SEEK(MCGC)
         REPL DEB WITH (DEB - MVAL)
         REPL ALT WITH '*'
      ENDIF
      sele DUPL
      IF SEEK(STR(MNNF,6))
         DO WHILE STR(NDUP,6) = STR(MNNF,6)
            REPL DTCANCEL WITH MENT
            SKIP
         ENDDO
      ENDIF
   ENDIF
enddo
clos data
return

