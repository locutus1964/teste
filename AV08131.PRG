* AV08200 - LIQUIDACAO / ALTERACAO DE DUPLICATAS
* SISTEMA CONTAS A RECEBER - ADAO ELI PEREIRA - 02/10/93
*
close data
use PARAM
MPCOMPET = VAL((SUBS(PCOMPET,4,2)+SUBS(PCOMPET,1,2)))
use BANCOS INDEX BANCOS
sele 0
use CLIEN INDEX GUERRA.IDX,CLIEN.IDX
sele 0
use DUPL INDEX DUPL,DUPCGC
=CABTELA(0,0,24,79)
sele DUPL
@01,mensagr('Altera‡„o ou Recebimento de Duplicatas') ;
say 'Altera‡„o ou Recebimento de Duplicatas'
@03,17 say 'N£mero da Duplicata/Parcela/Ano'
@04,17 say "Data de Emiss„o (DD/MM/AA)"
@05,17 say 'Valor da Parcela'
@06,17 say 'Data do Vencimento (DD/MM/AA)'
@07,17 say 'Data do Pagamento (DD/MM/AA)'
@08,17 say 'Valor dos Juros recebidos'
@09,17 say 'Valor dos Descontos concedidos'
@10,17 say 'Valor das Custas de Cart¢rio'
@11,17 say 'Valor Total recebido'
@12,17 say 'Valor da Corre‡ao Monet ria'
@13,17 say 'Devolu‡„o Cliente - Nota Fiscal'
@14,17 say '                  - Data N.Fiscal'
@15,17 say '                  - Val. devolucao'
@16,17 say 'Duplicata que se refere a devolu‡„o'
@17,17 say 'Banco/Agˆncia p/Cobran‡a/Descto'
@18,17 say 'Data do Desconto Duplicata - Banco'
@19,17 say 'Data da Baixa do Desconto'
@20,17 say 'Obs:'
STOR  0  TO MDEL,TPAR
DO WHILE .T.
   MCGC    = SPACE(16)
   MGUERRA = SPACE(15)
   STOR SPACE(08) TO MDUPDEV, MNDUP, MDTDESCTO, MBXDESCTO, MEMIS, MVENC, MPAG, MDTNFCLI
   STOR 0 TO MVDP, MJUROS, MDESC, MLIQ, MVALDEV, MCUSTAS
   STOR SPACE(40) TO MOBS
   STOR 0 TO MNBANCO, MAGENCIA, MNFCLI, MNFI, NUMDUP, PARDUP, MVALREC, MCORRECAO
   stor YEAR(DATE())-1900 to vano
   @04,55 say MEMIS COLOR &CORVER
   @05,50 say MVDP      PICT '9,999,999,999.99' COLOR &CORVER
   @06,55 say MVENC COLOR &CORVER
   @07,55 say MPAG COLOR &CORVER
   @08,50 say MJUROS    PICT '9,999,999,999.99' COLOR &CORVER
   @09,50 say MDESC     PICT '9,999,999,999.99' COLOR &CORVER
   @10,50 say MCUSTAS   PICT '9,999,999,999.99' COLOR &CORVER
   @11,50 say MVALREC   PICT '9,999,999,999.99' COLOR &CORVER
   @12,50 say MCORRECAO PICT '9,999,999,999.99' COLOR &CORVER
   @13,57 say PADL(MNFCLI,6,'0') COLOR &CORVER
   @14,55 say MDTNFCLI COLOR &CORVER
   @15,50 say MVALDEV   PICT '9,999,999,999.99' COLOR &CORVER
   @16,54 say MDUPDEV COLOR &CORVER
   @17,54 say PADL(MNBANCO,3,'0') COLOR &CORVER
   @17,58 say PADL(MAGENCIA,4,'0') COLOR &CORVER
   @18,55 say MDTDESCTO  COLOR &CORVER
   @19,55 say MBXDESCTO  COLOR &CORVER
   @20,23 say MOBS       COLOR &CORVER
   @21,01 say SPACE(78)
   =MENSAG('[ESC] para encerrar atualiza‡„o')
   @03,55 GET NUMDUP PICT '999999'
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
   @03,64 get vano   pict '99'
   READ
   MATU = ' '
   sele DUPL
   KEYDUP = STR(NUMDUP,6) + STR(PARDUP,1) + str(vano,2)
   IF !seek(KEYDUP)
     MENS = 'Duplicata/Parcela n„o encontrada - Inclui ?'
     DO PEGRESP WITH [SN]
     MATU = MC
     IF MC = 'N' .or. empty(mc)
       LOOP
      ENDIF
   ELSE
      @04,55 say EMIS COLOR &CORVER
      STOR EMIS TO MEMIS
      @05,50 say VDP PICT '9,999,999,999.99' COLOR &CORVER
      STOR VDP TO MVDP
      @06,55 say VENC COLOR &CORVER
      STOR VENC TO MVENC
      @07,55 say PAG COLOR &CORVER
      STOR PAG TO MPAG
      @08,50 say JUROS PICT '9,999,999,999.99' COLOR &CORVER
      STOR JUROS TO MJUROS
      @09,50 say DESC  PICT '9,999,999,999.99' COLOR &CORVER
      STOR DESC TO MDESC
      @10,50 say CUSTAS  PICT '9,999,999,999.99' COLOR &CORVER
      STOR CUSTAS TO MCUSTAS
      IF LEFT(PAG,2) <> SPACE(02)
         @11,50 say (VDP + JUROS + CUSTAS - DESC + CORRECAO) PICT '9,999,999,999.99' COLOR &CORVER
      ENDIF
      @12,50 say CORRECAO  PICT '9,999,999,999.99' COLOR &CORVER
      MCORRECAO = CORRECAO
      IF NFCLI > 0
         @13,57 say PADL(NFCLI,6,'0') COLOR &CORVER
         STOR NFCLI TO MNFCLI
         @14,55 say DTNFCLI COLOR &CORVER
         STOR DTNFCLI TO MDTNFCLI
         @15,50 say VALDEV PICT '9,999,999,999.99' COLOR &CORVER
         STOR VALDEV TO MVALDEV
         @16,54 say DUPDEV COLOR &CORVER
         STOR DUPDEV TO MDUPDEV
      ENDIF
      @17,54 say PADL(NBANCO,3,'0') COLOR &CORVER
      MNBANCO = NBANCO
      @17,58 say PADL(AGENCIA,4,'0') COLOR &CORVER
      MAGENCIA = AGENCIA
      @18,55 say DTDESCTO  COLOR &CORVER
      MDTDESCTO = DTDESCTO
      @19,55 say BXDESCTO  COLOR &CORVER
      MBXDESCTO = BXDESCTO
      @20,23 say OBS       COLOR &CORVER
      STOR OBS TO MOBS
      MCGC  = CGC
      sele CLIEN
      SET ORDER TO 2
      IF seek(MCGC)
         @21,01 say SPACE(78)
         @21,17 say 'Cliente: '+RAZ
         MGUERRA = GUE
      ENDIF
      SET ORDER TO 1
      sele DUPL
      IF ! EMPTY(DTCANCEL)
         MENS = 'Duplicata cancelada em '+DTOC(DTCANCEL)+' - ENTER'
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
      IF PAG <> SPACE(8)
         =MENSAG('Duplicata j  foi recebida em  '+PAG +' - ENTER p/continuar')
         =INKEY(0)
         LOOP
      ENDIF
   ENDIF
   DATRASO = 0
   EMISANT = MEMIS
   @04,55 GET MEMIS PICTURE "99/99/99"
   READ
   IF MEMIS <> EMISANT
      =FDATA(MEMIS)
      IF TDATA = ' '
         IF VAL((SUBS(MEMIS,7,2)+SUBS(MEMIS,4,2)))<=MPCOMPET
            =MENSAG('Duplicata j  contabilizada - n„o pode alterar data - ENTER')
            =INKEY(0)
            LOOP
         ENDIF
      ENDIF
      if right(memis,2) # str(vano,2)
         =MENSAG('Ano nao est  condizente com a informa‡…o anterior - ENTER')
         =INKEY(0)
         LOOP
      endif
   ENDIF
   @05,50 GET MVDP   PICT '9,999,999,999.99'
   READ
   MVENCA = MVENC
   @06,55 GET MVENC PICT '99/99/99'
   READ
   =FDATA(MVENC)
   IF TDATA <> ' '
      =MENSAG('Data do vencimento incorreta... ENTER')
      =INKEY(0)
      LOOP
   ENDIF
   IF MVENC <> MVENCA
      IF VAL((SUBS(MVENC,7,2)+SUBS(MVENC,4,2)))<=MPCOMPET
         =MENSAG('Duplicata j  contabilizada - n„o pode alterar data - ENTER')
         =INKEY(0)
         LOOP
      ENDIF
   ENDIF
   IF MATU = '4'.OR. MATU = '2'
      @07,55 GET MPAG PICT '99/99/99'
      READ
      IF SUBS(MPAG,1,2) <> SPACE(02)
         =FDATA(MPAG)
         IF TDATA = ' '
            IF VAL((SUBS(MPAG,7,2)+SUBS(MPAG,4,2)))<=MPCOMPET
               =MENSAG('Data do Recebimento n„o pode ser menor que £ltima Compet. - ENTER')
               =INKEY(0)
               LOOP
            ENDIF
         ENDIF
      ELSE
         MPAG = SPACE(08)
         @07,55 say MPAG
      ENDIF
   ENDIF
   IF SUBS(MPAG,1,2) <> SPACE(02)
      @08,50 GET MJUROS PICT '9,999,999,999.99'
      READ
   ENDIF
   @09,50 GET MDESC    PICT '9,999,999,999.99'
   READ
   @10,50 GET MCUSTAS  PICT '9,999,999,999.99'
   READ
   nValReceb = MVDP + MJUROS + MCUSTAS - MDESC + MCORRECAO
   @11,50 say nValReceb PICT '9,999,999,999.99' COLOR &CORVER
   IF MATU = '4' .and. left(mpag,2) <> space(02)
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
   @13,57 GET MNFCLI PICT '999999'
   READ
   IF MNFCLI > 0
      @14,55 GET MDTNFCLI 
      @15,50 GET MVALDEV PICT '9,999,999,999.99'
      @16,54 GET MDUPDEV
      READ
   ENDIF
      sele BANCOS
      DO WHILE .T.
        @17,54 GET MNBANCO PICT '999'
        @17,58 GET MAGENCIA PICT '9999'
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
   IF LEFT(MPAG,2) = SPACE(02)
      DO WHILE .T.
         @18, 55 GET MDTDESCTO PICT '99/99/99'
         READ
         IF SUBS(MDTDESCTO,1,2) <> SPACE(02)
            =FDATA(MDTDESCTO)
            IF TDATA = ' '
               EXIT
            ENDIF
         ELSE
            EXIT
         ENDIF
      ENDDO
      DO WHILE .T.
         @19,55 GET MBXDESCTO PICT '99/99/99'
         READ
         IF SUBS(MBXDESCTO,1,2) <> SPACE(02)
            IF SUBS(DTDESCTO,1,2) = SPACE(02)
               =MENSAG('Duplicata n„o descontada - Baixa errada - ENTER')
               =INKEY(0)
               LOOP
            ENDIF
            =FDATA(MBXDESCTO)
            IF TDATA = ' '
               EXIT
            ENDIF
         ELSE
            EXIT
         ENDIF
      ENDDO
   ENDIF
   @20,23 GET MOBS
   READ
   IF MATU = 'S'
      sele CLIEN
      @21,01 say SPACE(78)
      @21,10 say 'Informe o Nome de Guerra do Cliente 'GET MGUERRA
      READ
      SET ORDER TO GUERRA
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
   Chave = STR(NUMDUP,6) + STR(PARDUP,1) + SUBSTR(EMIS,7,2)
   IF !seek(chave)
      APPEND BLANK
   ENDIF
   REPL NDUP     WITH NUMDUP   ,;
        PDUP     WITH PARDUP   ,;
        VDP      WITH MVDP     ,;
        EMIS     WITH MEMIS    ,;
        VENC     WITH MVENC    ,;
        CGC      WITH MCGC     ,;
        PAG      WITH MPAG     ,;
        JUROS    WITH MJUROS   ,;
        DESC     WITH MDESC    ,;
        CUSTAS   WITH MCUSTAS  ,;
        OBS      WITH MOBS     ,;
        NFCLI    WITH MNFCLI   ,;
        DTNFCLI  WITH MDTNFCLI ,;
        VALDEV   WITH MVALDEV  ,;
        DUPDEV   WITH MDUPDEV  ,;
        CORRECAO WITH MCORRECAO,;
        DTDESCTO WITH MDTDESCTO,;
        BXDESCTO WITH MBXDESCTO,;
        NBANCO   WITH MNBANCO  ,;
        AGENCIA  WITH MAGENCIA
ENDDO
CLOSE DATA
RETURN

