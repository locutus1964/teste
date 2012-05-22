* AV07701 - EMBARQUES OU CANCELAMENTO DE NOTAS FISCAIS
* SISTEMA DE ADMINISTRACAO DE VENDAS
* CORRIGIDO POR JAIR GONCALVES - 11/07/97
*
MDEL = 0
*
=CABTELA(0,0,24,79)
*
MENS = 'Embarques e Cancelamentos de Notas Fiscais'
=MENSAGR(MENS)
@01,POS SAY MENS
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
mncoleta = SPACE(10)
*
SELE 0
USE TRANS INDEX TRANS
SELE 0
USE CABECNF INDEX CABECNF
SELE 0
USE CLIEN INDEX CLIEN.IDX
SELE 0
USE CADNF index NUMNF  && ,CADNF,NFREF,NFPED
SET RELATION TO CGC INTO CLIEN,;
                TRA INTO TRANS
SELE 0
USE PROD  INDEX PROD
SELE 0
USE PEDID INDEX PEDIDM
SELE 0
USE DUPL  INDEX DUPL
DO WHILE .T.
  MNOTA = 0
  SELECT CADNF
  @08,01 CLEAR TO 21,78
  @04,02 say 'Nota Fiscal....:' GET MNOTA PICT [999999]
  @06,02 say 'Cliente........:' 
  @07,02 say 'Data de EmissÆo:'
  READ
  MNFI = 0
  =mensag(ECODIGO)
  IF LASTKEY() = 27
    =FINALIZA()
    RETURN
  ENDIF  
  
  *save screen
  *=mostra(10,02,21,78)
  *BROWSE FIELDS NNF:H='N§ Nf.',;
  *              DEMISSAO:H='Dt. Emiss„o',;
  *              CLIEN->RAZ:H='RazÆo Social';
  *              IN WINDOW TELA
  *deactivate window tela
  *rest screen
  
  IF ! SEEK( MNOTA ) .OR. ! SEEK( STR(MNOTA,6), 'CABECNF')
    =MENSAG( 'Nota Fiscal nÆo encontrada' )
    =INKEY(0)
    LOOP
  ENDIF 
  MNNF  = NNF
  MDEMI = DEMI
  MENT  = ENT
  IF ! EOF('CLIEN')
    @ 06, 19 SAY CLIEN->RAZ
  ENDIF  
  @ 07, 19 SAY MDEMI
  MBEST = BEST
  MEDUP = EDUP
  MNOPE = NOPE
  key1 = str(ped,6) + ref
  mref = ref
  mqtd = qtd
  mval = 0.00
  mipi = 0.00
  mcgc = cgc
  mdesconto = desconto

  IF ! EMPTY(CANCELNF)
     @09,15 SAY 'Data do cancelamento da N.Fiscal'
     @09,50 SAY CANCELNF
     =MENSAG('Nota Fiscal foi cancelada em '+cancelnf+ ' - ENTER')
     =INKEY(0)
     LOOP
  ENDIF
  IF ! EMPTY(MENT)
     @09,15 SAY 'Data da entrega: ' + MENT
     @10,15 SAY 'Transportadora.: ' + TRANS->GUETRA
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
            replace nnf  with 0,;
                    dent with space(8),;
                    CENT WITH SPACE(1)
          ENDIF
          sele CADNF
          repl ent with space(8)
          *--------- Jair: Grava da a Data da Entrega em CABECNF
          REPLACE CABECNF->DTENTREG WITH {}
          REPLACE CABECNF->NCOLETA  WITH SPACE(10)
          skip
       ENDDO
       sele DUPL
       IF SEEK(STR(MNNF,6))
         DO WHILE STR(NDUP,6) = STR(MNNF,6)
           REPL ENTREGA WITH ctod('')
           SKIP
         ENDDO
       ENDIF
      ENDIF
      LOOP
   ENDIF
   **------------------------------------------------------------
   @09,15 say space(60)
   IF EMPTY(MENT)
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
      @09,14 say 'Data da Entrega ao Cliente/Transp.: '
      @10,14 say 'N£mero da Coleta..................: '
      @11,14 say 'Transportadora....................: '
      DO WHILE .T.
         mncoleta = SPACE(10)
         ment = SPACE(08)
         IF ! EOF( 'TRANS' )
           @ 11, 51 SAY TRANS->GUETRA
         ELSE  
           @ 11, 51 SAY '<< Transp.nÆo encontrada >> ' + STR(TRA,4,0)
         ENDIF  
         @09,51 get ment     pict '99/99/99'
         @10,51 get mncoleta pict [@!]
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
      @13,15 SAY 'Confirme N£mero da Nota Fiscal'
      @13,50 get mnfi pict '999999' valid mnfi = MNNF
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
         REPL NFI WITH MNFI,;
              ENT WITH MENT
         *------ Jair: Grava Data da Entrega em CABECNF     
         REPLACE CABECNF->DTENTREG WITH CTOD(MENT)
         REPLACE CABECNF->NCOLETA  WITH MNCOLETA
         sele PEDID
         if SEEK(str(MPED,6) + MREF)
            replace nnf with mnnf,;
                    dent with ment,;
                    CENT WITH SPACE(1)
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
         *---- Jair: Grava o cancelamento em CABECNF
         REPLACE CABECNF->VALPROD   WITH 0
         REPLACE CABECNF->VALIPI    WITH 0
         REPLACE CABECNF->CANCELADA WITH .T.
         REPLACE CABECNF->DTCANCEL  WITH CTOD(MENT)
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

*================
FUNCTION FINALIZA
*================
   CLOSE DATABASES
RETURN   