* AV07702 - EMBARQUES OU CANCELAMENTO DE NOTAS FISCAIS
* SISTEMA DE ADMINISTRACAO DE VENDAS
* NOVA VERSAO * 
* AUTOR......: JAIR GONCALVES - 11/07/97
*
MDEL = 0

=CABTELA(0,0,24,79)

MENS = 'Embarques e Cancelamentos de Notas Fiscais'
=MENSAGR(MENS)
@01,POS SAY MENS

clos data
@04,01 CLEAR TO 21,78

STOR SPACE(03) TO mcpd, mven
STOR SPACE(20) TO mcgc, key1
ment  = space(08)
mpint = space(06)
mref  = space(10)
STOR 0 TO mqtd, mdel, mcom, ncom, msal, nsal, mfme, nfme, mfac
STOR 0 TO nfac, mped, mnnf, mcp1, mcp2, mcp3, mcp4, mpe1, mpe2
STOR 0 TO mpe3, mpe4, mnfi, mipi, mval, mdeb, ndeb
mncoleta = SPACE(10)

SELE 0
USE TRANS INDEX TRAGUE.IDX, TRANS.IDX
SELE 0
USE CABECNF ORDER P_NNF
SELE 0
USE CLIEN ORDER P_CGC
SELE 0
USE CADNF ORDER F_NNF
SELE 0
USE PROD  INDEX PROD.IDX
SELE 0
USE PEDID ORDER X_PEDREF
SELE 0
USE DUPL  ORDER xdupano

SELE CABECNF

@04,01 CLEAR TO 21,78                
SAVE SCREEN TO cTela

@10,5 say '                     A  t  e  n  ‡  „  o'

@14,5 say 'Para cancelar uma Nota Fiscal j  embarcada, precisa cancelar o embarque'
@16,5 say 'e depois cancelar a Nota Fiscal, portanto em duas opera‡”es.'
=INKEY(0)
@04,01 CLEAR TO 21,78

DO WHILE .T.
  @09,15 say space(60)
  MC = ''
  mens = 'Confirma a Entrega [ E ], Cancela a NF [ C ] ou Sair [ <Esc> ] ?'
  DO PEGRESP WITH [EC]
  =mensagr(mens)
  MATU = MC
  IF EMPTY(MC)
    EXIT
  ENDIF
  DO CASE
    CASE MATU = 'C'
      DO CancelaNF
    CASE MATU = 'E'
      DO EntregaNF
  ENDCASE
ENDDO  
=FINALIZA()
RESTORE SCREEN FROM cTela
RETURN

*=======================================================================

*-----------------------------------------------------------------------
*             C A N C E L A M E N T O    D E   N.  F I S C A L
*-----------------------------------------------------------------------

*==================
PROCEDURE CANCELANF
*==================
  PRIVATE cTela, dCancela, MDEMI, MNOTA, MNFI, MGUERRA, MNNF
  
  SAVE SCREEN TO cTela
  DO WHILE .T.
    @06, 19 SAY SPACE(45)
    @07, 19 SAY SPACE(08)
    MNOTA = 0
    MNFI = 0
    dCancela = {}
    MGUERRA = SPACE(20)
    =mensag(ECODIGO)
    SELECT CABECNF
    @08,01 CLEAR TO 21,78
    @04,02 say 'Nota Fiscal....:' GET MNOTA PICT [999999] VALID GETCLI(MGUERRA)
    @06,02 say 'Cliente........:' 
    @07,02 say 'Data de EmissÆo:'
    READ
  
    IF LASTKEY() = 27
      EXIT
    ENDIF  
 
    MNNF  = NNF
    MDEMI = DEMISSAO
 
    @07, 19 SAY MDEMI PICT [99/99/99]
    
    IF CANCELADA
      DO MSGNFCAN
      LOOP
    ENDIF
 
    @ 07, 19 GET MDEMI PICT [99/99/99]
    READ
 
    MENS = 'Tem certeza que pode cancelar a Nota Fiscal ?'
    DO PEGRESP WITH [SN]
    IF MC = 'N'
      LOOP
    ENDIF
    
    @09,15 say 'Data do cancelamento da N.Fiscal'
    DO WHILE .T.
      @09,50  get dCancela
      read
      IF dCancela < DEMISSAO
        =MENSAG('Data n„o pode ser menor que emiss„o da N.F. - ENTER')
        =INKEY(0)
        LOOP
      ENDIF
      EXIT
    ENDDO
    
    SELE CADNF
    SEEK( MNNF )
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
      REPL CANCELNF WITH DTOC(dCancela)
      sele PROD
      IF MBEST = 'S'
        IF SEEK(MREF)
          REPL PEA WITH (PEA + MQTD)
          REPL COM WITH (COM - MQTD)
        ENDIF
      ENDIF
      
      sele PEDID
      IF SEEK(STR(MPED,6) + MREF)
        REPL SAL WITH (SAL + MQTD)
      ENDIF
      sele CADNF
      skip
      
    ENDDO
    
    *---- Jair: Grava o cancelamento em CABECNF
    SELECT CABECNF
    
    IF EOF( 'CABECNF' )
      APPEND BLANK
      REPLACE NNF      WITH MNOTA
    ENDIF  
    
    REPLACE DEMISSAO  WITH MDEMI
    REPLACE VALPROD   WITH 0
    REPLACE VALIPI    WITH 0
    REPLACE VALICM    WITH 0
    REPLACE VBASEICM  WITH 0
    REPLACE CANCELADA WITH .T.
    REPLACE DTCANCEL  WITH dCancela

    *----- Retorna o debito do cliente
    sele CLIEN
    IF SEEK(MCGC)
      REPL DEB WITH (DEB - MVAL)
      REPL ALT WITH '*'
    ENDIF

    *----- Cancela as Duplicatas    
    sele DUPL
    IF SEEK(STR(MNNF,6))
      DO WHILE STR(NDUP,6) = STR(MNNF,6)
        REPL DTCANCEL WITH dCancela
        SKIP
      ENDDO
    ENDIF
    
  ENDDO
  RESTORE SCREEN FROM cTela
RETURN 

**-------------------------------------------------------------
**           E N T R E G A    A O    C L I E N T E
**-------------------------------------------------------------

*==================
PROCEDURE ENTREGANF
*==================
  PRIVATE cTela, dEntrega
  
  SAVE SCREEN TO cTela
  DO WHILE .T.
    =MENSAG('')
    SELECT CABECNF
    @04,01 CLEAR TO 21,78
    MNOTA    = 0    
    mncoleta = SPACE(10)
    dEntrega = {}
    @04,02 say 'Nota Fiscal...............: '
    @05,02 say 'Cliente...................: ' 
    @06,02 say 'Data de EmissÆo...........: '
    @07,02 say 'Entrega ao Cliente/Transp.: '
    @08,02 say 'N£mero da Coleta .........: '
    @09,02 say 'Transportadora... ........: '
    
    @04,30 get mnota    pict [999999]
    read
    
    IF LASTKEY() = 27
      EXIT
    ENDIF
      
    IF ! SEEK( MNOTA )
      =MENSAG( 'Nota Fiscal nÆo encontrada' )
      =INKEY(0)
      LOOP
    ENDIF
    
    IF CANCELADA 
      DO MsgNfCan
      LOOP
    ENDIF

    SELECT TRANS
    SET ORDER TO TRANS
    IF ! SEEK(CABECNF->TRA)
      MGUETRA = '*' + CABECNF->TRA + '* - << Transportadora nÆo encontrada >>'
    ELSE
      MGUETRA = TRANS->GUETRA  
    ENDIF 

    SELECT CLIEN
    IF SEEK( CABECNF->CGC )
      cRazao = CLIEN->GUE
    ELSE
      cRazao = 'Cliente nao encontrado - ' + CABECNF->CGC  
    ENDIF
        
    SELECT CABECNF
    @05,30 SAY cRazao
    @06,30 SAY DEMISSAO
    @09,30 SAY MGUETRA
            
    dEntrega = DTENTREG
    MNCOLETA = NCOLETA
    MNNF     = NNF

    SELECT TRANS
    IF EMPTY(dEntrega)

      SET ORDER TO TRAGUE
      @07,30 get dEntrega     
      @08,30 get mncoleta pict [@!]
      @09,30 get MGUETRA  VALID CHKTRANS()
      READ        
     
      SET ORDER TO TRANS
      MENS = DADCORR
      DO PEGRESP WITH [SN]
      IF MC = 'N' .OR. EMPTY(MC)
        LOOP
      ENDIF  
      
      IF dEntrega < CABECNF->DEMISSAO
        =MENSAG('Data n„o pode ser menor que emiss„o da N.F. - ENTER')
        =INKEY(0)
        LOOP
      ENDIF
    ELSE
      @07,30 SAY dEntrega
      @08,30 SAY MNCOLETA
      @09,30 SAY MGUETRA
      MENS = 'Editar/Cancelar embarque da Nota Fiscal (E/C) ?'
      DO PEGRESP WITH [EC]
      
      IF Empty(mc)
        LOOP
      ENDIF
      
      IF MC = 'C' 
        dEntrega = {}
        MNCOLETA = SPACE(10)
      ELSE
        SELECT TRANS
        SET ORDER TO TRAGUE
        @07,30 get dEntrega pict [99/99/99] valid chkData()    
        @08,30 get mncoleta pict [@!]
        @09,30 get MGUETRA  VALID CHKTRANS()
        READ        
    
        SET ORDER TO TRANS
        MENS = DADCORR
        DO PEGRESP WITH [SN]

        IF MC = 'N' .OR. EMPTY(MC)
          LOOP
        ENDIF  
    
      ENDIF 
    ENDIF
      
    SELE CADNF
    SEEK( MNNF )
    do while str(NNF,6) = str(MNNF,6) .and. !eof()
      MREF = REF
      MQTD = QTD
      repl ent with Dtoc(dEntrega)
      skip
    ENDDO    
         
    *------ Jair: Grava Data da Entrega em CABECNF     
    SELECT CABECNF
    REPLACE DTENTREG WITH dEntrega
    REPLACE NCOLETA  WITH MNCOLETA
    REPLACE TRA      WITH TRANS->TRA
    
    sele DUPL
    if SEEK(STR(MNNF,6))
      DO WHILE STR(NDUP,6) = STR(MNNF,6)
        REPL ENTREGA WITH dEntrega
        SKIP
      ENDDO
    ENDIF
    
  ENDDO
  RESTORE SCREEN FROM cTela
 
RETURN 

*=================
PROCEDURE MSGNFCAN
*=================
  @10,15 SAY 'Data do cancelamento da N.Fiscal'
  @10,50 SAY CABECNF->DTCANCEL
  =MENSAG( 'Pressione qualquer tecla para continuar...' )
  =INKEY(0)
RETURN

*================
FUNCTION FINALIZA
*================
   CLOSE DATABASES
RETURN   

*==============
FUNCTION GETCLI
PARAMETER nome
*==============
  PRIVATE cCliente, lRet
  
  lRet = .T.
  IF .NOT. SEEK( MNOTA, 'CABECNF')
    MC = ''
    MENS= 'Nota Fiscal nÆo encontrada, deseja cancelar assim mesmo ?'
    DO PEGRESP WITH [SN] 
    lRet = (MC = 'S')
  ENDIF 

  IF SEEK(CABECNF->CGC, 'CLIEN')
    cCliente = CLIEN->RAZ
  ELSE
    cCliente = '<< Cliente nao encontrado >>'  
  ENDIF  
  @ 06, 19 SAY PADR(cCliente,45)
  
RETURN lRet

*****************
FUNCTION CHKTRANS
*****************
  PRIVATE lAchou
  
  lAchou = SEEK( MGUETRA, 'TRANS' )
  IF lAchou
    MGUETRA = TRANS->GUETRA
    @09,30 SAY SPACE(20)
    @09,30 SAY PADR(MGUETRA,20)  
  ELSE  
    =MENSAG( '<< Transp.nÆo encontrada >> - *' + TRA + '*' )
    =INKEY(0)
  ENDIF 
 
RETURN lAchou
  
****************  
FUNCTION CHKDATA
****************
  PRIVATE lValida
  
  lValida = .T.
  
  IF dEntrega < CABECNF->DEMISSAO
     =MENSAG('Data n„o pode ser menor que emiss„o da N.F. - ENTER')
     =INKEY(0)
     lValida = .F.
  ENDIF

RETURN lValida



