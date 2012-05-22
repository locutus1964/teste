* AV08200 - LIQUIDACAO / ALTERACAO DE DUPLICATAS
* SISTEMA CONTAS A RECEBER - ADAO ELI PEREIRA - 02/10/93
*
close data
sele 0
USE TPCART ORDER P_TIPO
sele 0
use PARAM
MPCOMPET = VAL((SUBS(PCOMPET,4,2)+SUBS(PCOMPET,1,2)))
use BANCOS INDEX BANCOS.IDX
sele 0
use CLIEN ORDER X_GUE
sele 0
use DUPL ORDER XDUPANO
=CABTELA(0,0,24,79)
sele DUPL
@01,mensagr('Altera‡„o ou Recebimento de Duplicatas') ;
say 'Altera‡„o ou Recebimento de Duplicatas'
@03,17 say 'N£mero da Duplicata/Parcela/Ano'
@04,17 say "Data de Emiss„o (DD/MM/AAAA)"
@05,17 say 'Valor da Parcela'
@06,17 say 'Data do Vencimento (DD/MM/AAAA)'
@07,17 say 'Data do Pagamento (DD/MM/AAAA)'
@08,17 say 'Valor dos Juros recebidos'
@09,17 say 'Valor dos Descontos concedidos'
@10,17 say 'Valor das Custas de Cart¢rio'
@11,17 say 'Valor Total recebido'
@12,17 say 'Valor da Corre‡ao Monet ria'
@13,17 say 'Devolu‡„o Cliente' 
@14,17 say ' - N.Fiscal....:'
@14,51 say ' - Dt.N.Fiscal.:'
@15,17 say ' - Val.devol...:'
@15,51 say ' - Dupl.devol..:'
@16,17 say 'Banco/Agˆncia p/Cobran‡a/Descto'
@17,17 say 'Tipo de Carteira               '
@18,17 say 'Data do Desconto Duplicata - Banco'
@19,17 say 'Data da Baixa do Desconto'
@20,17 say 'Obs:'
STOR  0  TO MDEL,TPAR
DO WHILE .T.
   MCGC    = SPACE(16)
   MGUERRA = SPACE(15)
   MTIPOCART = SPACE(2)
   MDTDESCTO = {}
   MBXDESCTO = {}
   MPAG      = {}
   STOR SPACE(08) TO MNDUP
   MDUPDEV = CTOD('')
   MVENC = CTOD('')
   MEMIS = CTOD('')
   MDTNFCLI = CTOD('')
   STOR 0 TO MVDP, MJUROS, MDESC, MLIQ, MVALDEV, MCUSTAS
   STOR SPACE(40) TO MOBS
   STOR 0 TO MNBANCO, MAGENCIA, MNFCLI, MNFI, NUMDUP, PARDUP, MVALREC, MCORRECAO
   stor YEAR(DATE()) to vano
   @04,55 say MEMIS COLOR &CORVER
   @05,50 say MVDP      PICT '9,999,999,999.99' COLOR &CORVER
   @06,55 say MVENC COLOR &CORVER
   @07,55 say MPAG COLOR &CORVER
   @08,50 say MJUROS    PICT '9,999,999,999.99' COLOR &CORVER
   @09,50 say MDESC     PICT '9,999,999,999.99' COLOR &CORVER
   @10,50 say MCUSTAS   PICT '9,999,999,999.99' COLOR &CORVER
   @11,50 say MVALREC   PICT '9,999,999,999.99' COLOR &CORVER
   @12,50 say MCORRECAO PICT '9,999,999,999.99' COLOR &CORVER
   @14,34 say PADL(MNFCLI,6,'0') COLOR &CORVER
   @14,68 say MDTNFCLI COLOR &CORVER
   @15,34 say MVALDEV   PICT '9,999,999,999.99' COLOR &CORVER
   @15,68 say MDUPDEV COLOR &CORVER
   @16,54 say PADL(MNBANCO,3,'0') COLOR &CORVER
   @16,58 say PADL(MAGENCIA,4,'0') COLOR &CORVER
   @17,54 say MTIPOCART COLOR &CORVER
   @17,57 say GETCART(MTIPOCART)
   @18,55 say MDTDESCTO  COLOR &CORVER
   @19,55 say MBXDESCTO  COLOR &CORVER
   @20,23 say MOBS       COLOR &CORVER
   @21,01 say SPACE(78)
   =MENSAG('[ESC] para encerrar atualiza‡„o')
   @03,55 get NUMDUP PICT '999999'
   READ
   IF LASTKEY() = 27
      IF MDEL = 1
         PACK
         MDEL = 0
      ENDIF
      CLOSE DATA
      CLEAR
      RETURN
   ENDIF
   IF NUMDUP = 0
      =MENSAG('Duplicata n„o pode ser zeros ou brancos - ENTER')
      =INKEY(0)
      LOOP
   ENDIF
   @03,62 GET PARDUP PICT '9'
   @03,64 get vano   pict '9999'
   READ
   MATU = ' '
   sele DUPL
   KEYDUP = STR(NUMDUP,6) + STR(PARDUP,1) + str(vano,4)
   IF !seek(KEYDUP)
     MENS = 'Duplicata/Parcela n„o encontrada - Inclui ?'
     DO PEGRESP WITH [SN]
     MATU = MC
     IF MC = 'N' .or. empty(mc)
       LOOP
      ENDIF
   ELSE
      =SEEK(TIPOCART,'TPCART')
      MTIPOCART = TIPOCART
      @04,55 say EMIS COLOR &CORVER
      STOR EMIS TO MEMIS
      @05,50 say VDP PICT '9,999,999,999.99' COLOR &CORVER
      STOR VDP TO MVDP
      @06,55 say VENC COLOR &CORVER
      STOR VENC TO MVENC
      @07,55 say PAG COLOR &CORVER
      MPAG = PAG
      @08,50 say JUROS PICT '9,999,999,999.99' COLOR &CORVER
      STOR JUROS TO MJUROS
      @09,50 say DESC  PICT '9,999,999,999.99' COLOR &CORVER
      STOR DESC TO MDESC
      @10,50 say CUSTAS  PICT '9,999,999,999.99' COLOR &CORVER
      STOR CUSTAS TO MCUSTAS
      IF ! empty(PAG)
         @11,50 say (VDP + JUROS + CUSTAS - DESC + CORRECAO) PICT '9,999,999,999.99' COLOR &CORVER
      ENDIF
      @12,50 say CORRECAO  PICT '9,999,999,999.99' COLOR &CORVER
      MCORRECAO = CORRECAO
      IF NFCLI > 0
         @14,34 say PADL(NFCLI,6,'0') COLOR &CORVER
         STOR NFCLI TO MNFCLI
         @14,68 say DTNFCLI COLOR &CORVER
         STOR DTNFCLI TO MDTNFCLI
         @15,34 say VALDEV PICT '9,999,999,999.99' COLOR &CORVER
         STOR VALDEV TO MVALDEV
         @15,68 say DUPDEV COLOR &CORVER
         STOR DUPDEV TO MDUPDEV
      ENDIF
      @16,54 say PADL(NBANCO,3,'0') COLOR &CORVER
      MNBANCO = NBANCO
      @16,58 say PADL(AGENCIA,4,'0') COLOR &CORVER
      MAGENCIA = AGENCIA
      @17,54 say TIPOCART COLOR &CORVER
      @17,57 say GETCART(TIPOCART) 
      MTIPOCART = TIPOCART
      @18,55 say DTDESCTO  COLOR &CORVER
      MDTDESCTO = DTDESCTO
      @19,55 say BXDESCTO  COLOR &CORVER
      MBXDESCTO = BXDESCTO
      @20,23 say OBS       COLOR &CORVER
      STOR OBS TO MOBS
      MCGC  = CGC
      sele CLIEN
      SET ORDER TO P_CGC
      IF seek(MCGC)
         @21,01 say SPACE(78)
         @21,17 say 'Cliente: '+RAZ
         MGUERRA = GUE
      ENDIF
      SET ORDER TO X_GUE
      sele DUPL
      IF ! EMPTY(DTCANCEL)
         MENS = 'Duplicata cancelada em '+DTCANCEL+' - ENTER'
         =MENSAG(MENS)
         =INKEY(0)
         LOOP
      ENDIF
      MENS = '[ 1 ] Retorna, [ 2 ] Altera ou [ 4 ] Registrar Recebimento ?'
      IF MATU <> 'S'
         MATU = ' '
         DO PEGRESP WITH [124]
         MATU = MC
      ENDIF
   ENDIF
   IF MATU = '1' .or. empty(mc)
      LOOP
   ENDIF
   IF MATU = '4' 
      IF ! EMPTY(PAG)
         =MENSAG('Duplicata j  foi recebida em  '+PAG +' - ENTER p/continuar')
         =INKEY(0)
         LOOP
      ENDIF
   ENDIF
   DATRASO = 0
   EMISANT = MEMIS
   @04,55 GET MEMIS 
   READ
   IF MEMIS <> EMISANT
      =FDATA(DTOC(MEMIS))
      IF TDATA = ' '
         IF VAL(SUBSTR(DTOS(MEMIS),1,6)) <= MPCOMPET 
            =MENSAG('Duplicata j  contabilizada - n„o pode alterar data - ENTER')
            =INKEY(0)
            LOOP
         ENDIF
      ENDIF
      if YEAR(memis) <> vano
         =MENSAG('Ano nao est  condizente com a informa‡…o anterior - ENTER')
         =INKEY(0)
         LOOP
      endif
   ENDIF
   @05,50 GET MVDP   PICT '9,999,999,999.99'
   READ
   MVENCA = MVENC
   @06,55 GET MVENC 
   READ
   =FDATA(dtoc(MVENC))
   IF TDATA <> ' '
      =MENSAG('Data do vencimento incorreta... ENTER')
      =INKEY(0)
      LOOP
   ENDIF
   IF MVENC <> MVENCA
      IF VAL(DTOS(MVENC)) <= MPCOMPET
         =MENSAG('Duplicata j  contabilizada - n„o pode alterar data - ENTER')
         =INKEY(0)
         LOOP
      ENDIF
   ENDIF
   IF MATU = '4'.OR. MATU = '2'
      @07,55 GET MPAG VALID MPAG <= DATE()
      READ
   ENDIF
   IF ! EMPTY(MPAG)
      @08,50 GET MJUROS PICT '9,999,999,999.99'
      READ
   ENDIF
   @09,50 GET MDESC    PICT '9,999,999,999.99'
   READ
   @10,50 GET MCUSTAS  PICT '9,999,999,999.99'
   READ
   nValReceb = MVDP + MJUROS + MCUSTAS - MDESC + MCORRECAO
   @11,50 say nValReceb PICT '9,999,999,999.99' COLOR &CORVER
   IF MATU = '4' .and. ! EMPTY(mpag)
      nTecla = 0
      DO WHILE nValReceb <> MVALREC .AND. nTecla <> 27
         @11,50 GET MVALREC PICT '9,999,999,999.99'
         read
         IF nValReceb <> MVALREC
            @21,50 say (MVALREC - nValReceb) pict '999,999,999.99'
            @23,01 say space(78)
            =mensag('Existe diferen‡a nos valores - Corrigir ou ESC para sair')
            nTecla = INKEY(0)
            IF nTecla = 13
              LOOP
            ENDIF  
            if nTecla = 27
               EXIT
            endif
         ELSE
            @11,50 say MVALREC PICT '9,999,999,999.99'
            EXIT
         ENDIF
      ENDDO
      IF nTecla = 27
        LOOP
      ENDIF  
   ENDIF
   @14,34 GET MNFCLI PICT '999999'
   READ
   IF MNFCLI > 0
      @14,68 GET MDTNFCLI 
      @15,34 GET MVALDEV PICT '9,999,999,999.99'
      @15,68 GET MDUPDEV
      READ
   ENDIF

   SELE BANCOS
   DO WHILE .T.
     @16,54 GET MNBANCO PICT '999'
     @16,58 GET MAGENCIA PICT '9999'
     READ
     IF MNBANCO > 0
       IF !seek(STR(MNBANCO,3) + STR(MAGENCIA,4))
         =MENSAG(CORRIJA)
         =INKEY(0)
         LOOP
       ELSE
         EXIT
       ENDIF
     ELSE
       EXIT
     ENDIF
   ENDDO
   
   DO WHILE .T.
     
     @17,54 GET MTIPOCART PICT [@!] VALID SEEK( MTIPOCART, 'TPCART' )
     READ

     @17, 57 SAY GETCART(MTIPOCART)     
     
     IF MTIPOCART = 'DE'
   
       @18,55 GET MDTDESCTO VALID !EMPTY(MDTDESCTO)
       @19,55 GET MBXDESCTO 
       READ
       
       IF ! EMPTY(MPAG) .AND. EMPTY(MBXDESCTO)
         =mensag( 'Dupl.descontada c/data de pgto. Falta a data da baixa do desconto - ENTER' )
         =INKEY(0)
         LOOP
       ENDIF
       
     ENDIF
     EXIT
   ENDDO  
   
   @20,23 GET MOBS
   READ
   IF MATU = 'S'
      sele CLIEN
      @21,01 say SPACE(78)
      @21,10 say 'Informe o Nome de Guerra do Cliente 'GET MGUERRA
      READ
      SET ORDER TO X_GUE
      IF !seek(RTRIM(MGUERRA))
         =MENSAG(CORRIJA)
         =INKEY(0)
         sele DUPL
         LOOP
      ELSE
         MCGC = CGC
         MGUERRA = GUE
      ENDIF
   ENDIF
   MENS = DADCORR
   DO PEGRESP WITH [SN]
   IF MC = 'N' .or. empty(mc)
      LOOP
   ENDIF
   sele DUPL
   Chave = STR(NUMDUP,6) + STR(PARDUP,1) + STR(YEAR(EMIS),4)
   IF !seek(chave)
      APPEND BLANK
   ENDIF
   REPL NDUP     WITH NUMDUP  
   REPL PDUP     WITH PARDUP  
   REPL VDP      WITH MVDP    
   REPL EMIS     WITH MEMIS   
   REPL VENC     WITH MVENC      
   REPL CGC      WITH MCGC       
   REPL PAG      WITH MPAG       
   REPL JUROS    WITH MJUROS     
   REPL DESC     WITH MDESC      
   REPL CUSTAS   WITH MCUSTAS    
   REPL OBS      WITH MOBS       
   REPL NFCLI    WITH MNFCLI     
   REPL DTNFCLI  WITH MDTNFCLI   
   REPL VALDEV   WITH MVALDEV    
   REPL DUPDEV   WITH MDUPDEV    
   REPL CORRECAO WITH MCORRECAO  
   REPL DTDESCTO WITH MDTDESCTO  
   REPL BXDESCTO WITH MBXDESCTO  
   REPL NBANCO   WITH MNBANCO    
   REPL AGENCIA  WITH MAGENCIA  
   REPL TIPOCART WITH MTIPOCART
ENDDO
CLOSE DATA
RETURN

*===============
FUNCTION GETCART
*===============
PARAMETER Tipo

  IF SEEK( Tipo, 'TPCART')
    cTexto = TPCART->DESCRICAO
  ELSE
    cTexto = SPACE(20)
  ENDIF
  
RETURN cTexto    
