* AV07600 - EMISSAO DE NOTAS FISCAIS DIVERSAS (FORA DE VENDAS)
* SISTEMA ADMINISTRACAO DE VENDAS
* ADAO ELI PEREIRA - 6/5/90 - FOX 12/10/92
*
=INICPAG()
MENS = 'Impressora pronta para imprimir Notas Fiscais ?'
do pegresp with [SN]
if MC = 'N' .or. empty(mc)
   return
endif
close data
SELE 0
USE CABECNF ORDER P_NNF
sele 0
use SIGLAS INDEX SIGLAS.IDX
sele 0
use PARAM
sele 0
use TPNF    inde TPNF.IDX
sele 0
use CLIEN  ORDER X_GUE
sele 0
use ESTADOS inde ESTADOS.IDX
sele 0
use TRANS   inde TRANS.IDX, TRARAZ.IDX
sele 0
use NFDIVS
sele 0
use BANCOS index BANCOS.IDX
sele 0
use OBSNOTA index OBSNOTA.IDX
mped = 0
sele 0
use DUPL  order xdupano
sele 0
use CADNF ORDER X_NFREF
sele 0
use CLI_ENTR ORDER P_CGC
sele 0
use CLI_COBR ORDER P_CGC
sele 0
use VEICULOS index VEICULOS.IDX
sele 0
use PROD     index PROD.IDX

IF ! ChkDataNF()
  CLOSE DATA
  RETURN
ENDIF
  
IF ! GETSIGLA()
  CLOSE DATA
  RETURN 
ENDIF

cSigla = SIGLAS->SIGLA

do while .T.
   sele OBSNOTA
   zap
   sele PARAM
   if bof()
      MNNF = 1
      UNNF = 0
   else
      UNNF = NNF
      MNNF = NNF + 1
   endif
   =JANELA(16,37,21,78)
   =BORDA(16,37,21,78)
   @18,40 say 'Ultima Nota Fiscal emitida   '+PADL(UNNF,7,'0')
   @19,40 say 'Pr¢xima Nota Fiscal a emitir '+PADL(MNNF,7,'0')
   MENS = DADCORR
   do pegresp with [SN]
   if MC = 'N' .or. empty(mc)
      close data
      return
   endif
   stor 0 to MICM, MPLIQ
   =CABTELA(0,0,24,79)
   MENS = 'Emiss„o de Notas Fiscais Diversas'
   =MENSAGR(MENS)
   @01,POS say MENS
   @04,02 say 'Tipo N.Fisc'
   @04,49 say 'N.Guerra'
   @05,02 say 'R.Social'
   @06,02 say 'Endere‡o'
   @07,02 say 'Cidade'
   @07,41 say 'Estado'
   @07,61 say 'CEP'
   @08,02 say 'CGC'
   @08,41 say 'Inscri‡„o'
   @10,02 say 'End.Entr'
   @11,02 say 'Cidade'
   @11,41 say 'Estado'
   @11,60 say 'CEP'
   @12,02 say 'CGC'
   @12,41 say 'Inscri‡„o'
   @14,02 say 'Transpt'
   @15,02 say 'Endere‡o'
   @16,02 say 'Cidade'
   @16,41 say 'Estado'
   @17,02 say 'CGC'
   @18,02 say 'INSC.'
   stor space(48) TO MRAZ, MEND, ENDENT, RAZTRA, ENDTRA
   stor space(20) TO MCGC, CGCENT, CIDTRA, MUNIENT, MBAI,CIDENT
   stor space(02) TO MEST, ESTTRA, ESTENT
   stor space(09) TO MCEP, CEPENT, mfon1
   stor space(17) TO MINS, INSENT, instra
   stor space(14) to cgctra
   stor space(04) to mddd1
   stor 0.00      TO MIPI, VISSP, PBRUTO, VALISS, BASEIPI, BASEICM, BASEISS
   ESPECIE   = space(17)
   MVES      = space(17)
   MNOPE     = space(04)
   MCCL      = space(10)
   MVIA      = space(10)
   MTIP      = space(03)
   MEDUP     = space(01)
ENDCOB1   = space(48)
ENDCOB2   = space(48)
QTDVOL    = 0
MPLIQUIDO = 0.00

set deci to 4

do while .T.
   =MENSAG(ECODIGO)
   sele TPNF
   @04,15 get MTIP        picture '999'
   read
   if LASTKEY() = 27
      close data

      set deci to

      return
   endif
   if !SEEK(MTIP)
      =MENSAG('Tipo de Nota Fiscal n„o cadastrado - ENTER')
      =INKEY(0)
      LOOP
   else
      @04,20 say NOPE
      @04,26 say DESN
      MTIP  = TIPO
      MNOPE = NOPE
      MDESN = DESN
      MIPIT = IPIT
      MICMT = ICMT
      MISST = ISST
      MISSP = ISSP
      MCODT = CODT
      MEDUP = EDUP
      MBEST = BEST
      MMNF1 = MNF1
      MMNF2 = MNF2
   endif
   MGUE  = space(15)
   MCGCE = space(16)
   MCGCC = space(16)
   MLOJA = space(03)
   MTRA  = space(03)
   MMUNI = space(20)
   sele CLIEN
   @04,59 get MGUE        picture '@!'
   read
   if LASTKEY() = 27
      LOOP
   endif
   if MGUE = space(10)
      =MENSAG('Nome de Guerra do Cliente n„o pode ser brancos')
      =INKEY(0)
      LOOP
   endif
   if SEEK(MGUE)
      MRAZ = RAZ
      MEND = END
      MBAI = BAI
      MMUNI = CID
      MEST = EST
      MCEP = CEP
      mdd1 = ddd1
      mfon1 = fon1
      MINS = INS
      MGUE = GUE
      MTRA = TRA
      @04,73 say MLOJA
      @05,13 say MRAZ
      @06,13 say MEND
      @07,13 say MMUNI
      @07,49 say MEST
      @07,68 say MCEP
      @08,13 say CGC
      @08,58 say MINS
      MCGC = CGC
      MCGCE = ENT
      MCGCC = COB
   else
      MENS = 'Cliente n„o cadastrado. Informa os dados ?'
      do pegresp with [SN]
      if MC = 'N'
         LOOP
      else
         @05,13 get MRAZ
         @06,13 get MEND
         @07,13 get MMUNI
         read
         sele ESTADOS
         do while .T.
            @07,55 get MEST
            read
            if SEEK(MEST)
               MICM = ICM
               exit
            endif
            =MENSAG('Estado n„o cadastrado')
         enddo

         @07,68 get MCEP
         @08,13 get MCGC
         @08,58 get MINS
      endif
   endif
   sele CLI_ENTR
   if SEEK(MCGC)
      ENDENT = ENDE
      CIDENT = CIDE
      ESTENT  = ESTE
      CEPENT  = CEPE
      CGCENT  = CGC
      INSENT  = INSE
   else
      @10,13 get ENDENT
      @11,13 get CIDENT
      @11,49 get ESTENT
      @11,68 get CEPENT
      @12,13 get CGCENT
      @12,60 get INSENT
      read
   endif
   if MEDUP = 'S'
      sele CLI_COBR
      if SEEK(MCGC)
         MENDC = ENDC
         MCIDC = CIDC
         MBAIC = BAIC
         MESTC = ESTC
         MCEPC = CEPC
      else
         MENDC = ''
         MCIDC = ' '
         MBAIC = ' '
         MESTC = ' '
         MCEPC = ' '
      endif
   endif
   sele TRANS
   @14,13 get MTRA        picture '999'
   read
   if MTRA <> space(03)
      SEEK MTRA
      if !(eof() .OR. bof())
         @14,18 say RAZ
         RAZTRA = RAZ
         @15,18 say END
         ENDTRA = END
         @16,13 say CID
         CIDTRA = CID
         @16,49 say EST
         ESTTRA = EST
         MVIA    = VIA
         cgctra = cgc
         instra = ins
      else
         @14,18 get RAZTRA
         @15,13 get ENDTRA
         @16,13 get CIDTRA
         @16,49 get ESTTRA
         @17,13 get cgctra
         @18,13 get instra
      endif
   endif
   if MEDUP = 'S'
      @17,36 TO 20,75 DOUBLE
      stor 0 TO MCP1,MCP2,MCP3,MCP4
      stor space(3) TO MCPD
      stor 0 TO MPE1,MPE2,MPE3,MPE4
      =MENSAG('Informe DIAS e PERCENTUAIS para vencimento das parcelas')
      @18,38 say 'C.Pag.'
      @18,65 say 'Dias'
      @19,38 say 'Perc.'
      @18,46 get mcp1        picture '999'
      @19,46 get mpe1        picture '999'
      @18,50 get mcp2        picture '999'
      @19,50 get mpe2        picture '999'
      @18,54 get mcp3        picture '999'
      @19,54 get mpe3        picture '999'
      @18,58 get mcp4        picture '999'
      @19,58 get mpe4        picture '999'
      read
      =JANELA(04,50,14,75)
      =BORDA(04,50,14,75)
      @05,52 say 'Modos aceitos:'
      @06,52 say 'VIS - Pagto a Vista'
      @07,52 say 'APR - Contra Apresent.'
      @08,52 say 'DDL - Dias da Data Liq'
      @09,52 say 'DFM - Dias fora o Mes'
      @10,52 say 'DFQ - Dias fora Quinz.'
      @11,52 say 'SDB - Sem D‚bito'
      @12,52 say 'PER - Permuta'
      @13,52 say 'ANT - Pagto Antecipado'
      do while .t.
         @18,70 get mcpd        picture '!!!'
         read
         if mcpd = 'VIS' .or. mcpd = 'APR' .or. mcpd = 'DDL' .or. mcpd = 'DFM' ;
           .or. mcpd = 'DFQ' .OR. MCPD = 'SDB' .OR. MCPD = 'ANT' .OR. MCPD = 'PER'
            exit
         endif
      enddo
   endif
   MENS = DADCORR
   do pegresp with [SN]
   if MC = 'N'
      close data

      set deci to

      return
   else
      exit
   endif
enddo
=CABTELA(0,0,24,79)
MENS = 'Notas Fiscais Diversas - Inclus„o dos Itens'
=MENSAGR(MENS)
@01,POS say MENS
@04,10 say 'Quantidade'
@06,10 say 'Unidade'
@08,10 say 'Pˆso L¡quido'
@10,10 say 'Descri‡„o'
@14,10 say 'C¢d.Tribut.'
@14,30 say 'Trib.IPI'
@14,46 say 'Trib.ICM'
@14,62 say 'Trib.ISS'
@16,10 say 'Classifica‡„o Fiscal'
@18,10 say 'Pre‡o Unit rio'
@20,10 say 'Perc. IPI'
@20,30 say 'Perc. ICM'
@20,50 say 'Perc. ISS'
sele NFDIVS
zap
do while .T.
   MIPI = 0.0
   stor 0.00 TO ISSP, VISSP, MPREU, MQTD, MPES
   MUNI = space(03)
   MCCL = space(10)
   MREF = space(04)
   stor space(53) TO MDES, MDES1
   @04,35 get MQTD        picture '999999.999'
   @06,35 get MUNI        picture '@!'
   @08,35 get MPES        picture '99999.99'
   MPLIQ = ((MQTD * MPES) + MPLIQ)
   sele PROD
   @10,25 get MREF
   read
   if SEEK(MREF)
      MDES = DES
      MDES1 = DES2
   endif
   @11,25 get MDES
   @12,25 get MDES1
   read
   =MENSAG('Aceitos: Letra e N£mero correspondente')
   @14,23 get MCODT        picture '!!'
   read
   =MENSAG('(1) Debita, (2) Isento, (3) Outras Debita e (4) = Outras Isento')
   @14,40 get MIPIT        picture '9' VALID(MIPIT$'1234')
   read
   @14,56 get MICMT        picture '9' VALID(MICMT$'1234')
   read
   =MENSAG('(1) Debita e (2) isento')
   @14,72 get MISST        picture '9' VALID(MISST$'12')
   read
   @16,35 get MCCL         picture '!'
   **    @18,35 get MPREU        picture '9999999999.99'
   @18,35 get MPREU        picture '99999999.9999'
   read
   if MIPIT = '1' .OR. MIPIT = '3'
      do while .T.
         =MENSAG('IPI Tributado exige percentual de IPI')
         @20,22 get MIPI        picture '99.9'
         read
         if MIPI > 0.0
            exit
         endif
      enddo
   endif
   if MICMT = '1' .OR. MICMT = '3'
      sele ESTADOS
      if SEEK(MEST)
         MICM = ICM
      endif
      do while .T.
         =MENSAG('ICM Tributado exige percentual de ICM')
         @20,42 get MICM        picture '99.99'
         read
         if MICM > 0.00
            exit
         endif
      enddo
   endif
   if MISST = '1'
      do while .T.
         =MENSAG('ISS Tributado exige percentual de ISS')
         @20,62 get MISSP        picture '99.99'
         read
         if MISSP > 0.00
            exit
         endif
      enddo
   endif
   MENS = DADCORR
   do pegresp with [SN]
   if MC = 'S'
      sele NFDIVS
      append blank
      repl QTD     with MQTD     ,UNI     with MUNI     ,;
           PES     with MPES     ,REF     with MREF     ,;
           DES1    with MDES1    ,DES     with MDES     ,;
           CODT    with MCODT    ,CCL     with MCCL     ,;
           PREU    with MPREU    ,IPI     with MIPI     ,;
           ICM     with MICM     ,ISSP    with MISSP
   endif
   @20,22 say space(05)
   @20,42 say space(05)
   @20,62 say space(05)
   MENS = 'Inclui mais ¡tens na Nota Fiscal ?'
   do pegresp with [SN]
   if MC = 'N'
      exit
   endif
enddo
stor space(60) TO MOBSNF1, MOBSNF2, MOBSNF3, MOBSNF4
stor space(60) TO MOBSNF5, MOBSNF6, MOBSNF7, MOBSNF8
stor 0         TO MNBANCO, MAGENCIA, MQTDVOL, MPBRUTO
MESPECIE = space(17)
MVEICULO = space(08)
**       SAVE SCREEN TO TELA
**       =CABTELA(0,0,24,79)
MENS = 'Inclui Observa‡”es para este Pedido ?'
            do pegresp with 'NS'
DO INCLUIOB with 'AV07600'
**       REST SCREEN FROM TELA
MENS = DADCORR
do pegresp with [SN]
if MC = 'N'

   set deci to

   return
endif
**------------------------------------------------------------------
**                        IMPRESSAO DA NOTA FISCAL
**------------------------------------------------------------------
=MENSAG('Imprimindo a Nota Fiscal... Aguarde')
*SET PRINTER TO ARQ.TXT
sele NFDIVS
go bott
if bof()
   =MENSAG('N„o existem dados para emiss„o da Nota Fiscal - ENTER')
   =INKEY(0)
   close data

   set deci to

   return
endif
go top
stor 0.00 TO VALT, BASEISS, VALICM, VALIPI, BASEICM, BASEIPI
stor 0.00 TO TPES, VIPI, VICM
_PDSETUP = ""
_Peject  = "NONE"
_PAGENO  = 1
_PCOPIES = 1
_PBPAGE  = 1
_PEPAGE  = 1
_PSCODE  = ""
_PECODE  = ""
_PLINENO = 0
stor 0 to tabco    &&--- Tabula‡„o de linha e coluna
stor 2 to tabli    &&--- Tabula‡„o de linha e coluna

*do DEFSAIDA
??? CHR(27) + CHR(64) + CHR(30) + '4' + chr(27) + 'C' + chr(78)
set device to print

set deci to 4
do while .not. eof()
   **----------------------- Dados da Nota Fiscal -------------------------
   @tabli+02,tabco+118 say CHR(27)+'w'+CHR(1)
   @tabli+02,pcol() say mnnf pict '9 9 9 9 9 9'
   @tabli+02,pcol() say CHR(27)+'w'+CHR(0)      &&--- N£mero
   @tabli+03,tabco+083 say 'X'                   &&--- Nf. de sa¡da
   @tabli+07,tabco+002 say mdesn                 &&--- Natureza Opera‡„o
   @tabli+07,tabco+045 say mnope                 &&--- CFOP
   ** @tabli+07,tabco+058 say '*'                &&--- Ins. Estad. substituto tribut rio
   ** @tabli+07,tabco+120 say '*'                &&--- Data limite p/emissao
   **----------------------- Destinat rio ---------------------------------
   @tabli+10,tabco+002 say mraz                  &&--- Razao social
   @tabli+10,tabco+085 say mcgc                  &&--- Ins. CGC
   @tabli+10,tabco+123 say HOJE                &&--- Data de emiss„o
   @tabli+12,tabco+002 say mend                  &&--- Endere‡o
   @tabli+12,tabco+072 say mbai                  &&--- Bairro
   @tabli+12,tabco+103 say mcep                  &&--- CEP
   ** @tabli+12,tabco+120 say '*'                &&--- Data de Sa¡da
   @tabli+14,tabco+002 say mmuni
   if !empty(mddd1)                              &&--- DDD e Telefone
      @tabli+14,tabco+052 say '('+alltrim(mDDD1)+')'+ mFON1
   else
      @tabli+14,tabco+052 say mFON1
   endif
   @tabli+14,tabco+075 say mest                  &&--- Estado
   @tabli+14,tabco+086 say mins                  &&--- Inscricao
   ** @tabli+14,tabco+114 say time()             &&--- Hora da Sa¡da
   **  @10,018 say MRAZ
   **   @10,093 say MNOPE
   **  @10,123 say 'REAIS'
   **   @11,117 say MGUE
   **  @12,018 say MEND
   **  @12,081 say MDESN
   **  @13,018 say MBAI
   **  @13,064 say MCEP
   **  @14,018 say MMUNI
   **  @14,073 say MEST
   **  @15,018 say MCGC
   **----------------------- Fatura ----------------------------------------
   @tabli+17,tabco+002 say HOJE
   @tabli+17,tabco+025 say mnnf pict '999999'    &&--- N£mero
   @tabli+17,tabco+040 say 'Vide abaixo'         &&--- Valor
   @tabli+17,tabco+074 say 'Vide abaixo'         &&--- N§ da duplicata
   @tabli+17,tabco+095 say 'Vide abaixo'         &&--- Vencimento
   if medup = 'S'
      =seek(subs(mCGC,1,16),'cli_cobr')
      if !empty(cli_cobr->CIDC)
         @tabli+19,tabco+002 say alltrim(cli_cobr->endc)
         @tabli+19,pcol()+01 say ' '+alltrim(cli_cobr->cidc)+' '+cli_cobr->estc
      else
         @tabli+19,tabco+002 say alltrim(mend)
         @tabli+19,pcol()+01 say ' '+alltrim(mmuni)+' '+mest
      endif
   endif
   **   @15,092 say 'RODOVIARIO'
   if MEDUP = 'S'
      if MCPD = 'VIS' .OR. MCPD = 'APR' .OR. MCPD = 'SDB' .OR. MCPD = 'ANT'
         if MCPD = 'VIS'
            Y = 'A VISTA'
         endif
         if MCPD = 'APR'
            Y = 'CONTRA APRES'
         endif
         if MCPD = 'SDB'
            Y = 'SEM DEBITO'
         endif
         if MCPD = 'ANT'
            Y = 'ANTECIPADO'
         endif
      else
         Y = 'VER VENCTOS'
      endif
      **      @15,116 say Y
   endif
   **   @16,055 say MINS
   **  @16,093 say HOJE

   lin = 23
   **----------------------- Imprime os itens da Nf. -------------------------
   do while .not. eof()
      MREF = REF
      MQTD = QTD
      **     @lin,00 say MQTD        picture '99999.999'
      **     @lin,10 say UNI
      MPES = PES
      TPES = PES
      **     @lin,14 say MREF
      @tabli+lin,tabco+001 say mref              &&- Referencia
      if DES <> space(10)
         ** lin = lin + 1
         @tabli+lin,tabco+006 say des            &&- Descricao
         **        @lin,20 say DES
         if DES1 <> space(10)
            lin = lin + 1
            @tabli+lin,tabco+006 say des1        &&- Descricao
            **           @lin,20 say DES1
         endif
      endif
      **     @lin,82 say CODT+CCL
      MPREU = PREU
      **     @lin,83 say MPREU        picture "999,999.9999"
      @tabli+lin,tabco+069 say ccl                     &&- Class. Fiscal
      @tabli+lin,tabco+073 say codt                    &&- Class. Fiscal
      ** @tabli+lin,tabco+074 say '*'                   &&- Sit. Tribut ria
      @tabli+lin,tabco+077 say uni                     &&- Unidade
      @tabli+lin,tabco+083 say mqtd   pict '999999'    &&- Quantidade
      VTOTAAR = (MQTD * MPREU)
      VTOTA   = ROUND (VTOTAAR,2)
      VALT    = (VTOTA + VALT)
      if IPI > 0.00
         VIPI    = round(VTOTA * (IPI/100),2)
         VALIPI  = (VIPI + VALIPI)
         BASEIPI = (BASEIPI + VTOTA)
      endif
      if ICM > 0.00
         VICM    = (VTOTA * (ICM/100))
         VALICM  = (VICM + VALICM)
         BASEICM = (BASEICM + VTOTA)
      endif
      VISSP = 0.00
      CISSP = 0.00
      if ISSP > 0.00
         VISSP   = (VTOTA * (ISSP/100))
         VALISS  = (VISSP + VALISS)
         BASEISS = (BASEISS + VTOTA)
      endif
      @tabli+lin,tabco+091 say mpreu   pict '999999.9999'       &&- Vlr. Unit rio
      @tabli+lin,tabco+103 say vtota   pict '99,999,999.99'     &&- Vlr. Total
      if VALICM > 0
         @tabli+lin,tabco+119 say micm   pict '@z 99.9'         &&- % Icm
         **         @58,048 say MICM                 picture '99.99'
      endif
      if IPI > 0
         @tabli+lin,tabco+124 say ipi pict '@z 99.9'            &&- % ipi
         **        @lin,115 say IPI        picture '99.9'
         if MNOPE <> '5.31'
            @tabli+lin,tabco+128 say vipi  pict '@z 999.99'     &&- Vlr IPI
            **            @lin,120 say VIPI        picture "99999,999,999.99"
         endif
      endif
      **     @lin,98 say VTOTA        picture "99999,999,999.99"
      sele CADNF
      append blank
      repl CGC      with MCGC         ,TIP      with MTIP         ,;
           REF      with MREF         ,QTD      with MQTD         ,;
           VAL      with VTOTA        ,NNF      with MNNF         ,;
           NOPE     with MNOPE        ,DEMI     with DTOC(HOJE) ,;
           ENT      with DTOC(HOJE) ,CVEN     with '*'          ,;
           IPI      with VIPI         ,ICM      with VICM         ,;
           DES      with MDES         ,EDUP     with MEDUP        ,;
           ISS      with CISSP        ,VISS     with VISSP
      lin = lin + 1
      sele NFDIVS
      SKIP
   enddo
   **---------------------------------------------------------------------------
   **                          QUEBRA DE NOTA FISCAL
   **---------------------- Outros Dados no Corpo do Pedido -------------------
   if VALIPI > 0 .and. MNOPE = '5.31'
      CLIN = 32
   else
      CLIN = 34
   endif
   FVGIPI = 0.00
   FVVICM = 0.00
   if eof() .OR. lin > CLIN
      if VALIPI > 0 .and. MNOPE = '5.31'
         lin = lin + 2
         @tabli+lin,tabco+025 say 'IPI a Estornar de:   R$'
         @tabli+lin,tabco+052 say VALIPI        picture "99,999,999.99"
         FVGIPI = VALIPI
         lin = lin + 1
      endif
      =av07600ob(mobsnf1,mobsnf2)
      =av07600ob(mobsnf3,mobsnf4)
      if seek(mTIP,'tpnf')
         lin = lin + 1
         =av07600ob(tpnf->mnf1, tpnf->mnf2)
      endif
      lin = lin + 1
      if !empty(ENDENT)
         lin = lin + 1
         *@tabli+lin,pcol()+1 say alltrim(endent)
         *@tabli+lin,pcol()+1 say '  '+alltrim(cident)+' '+estent+'  '+cepent

         @tabli+lin,tabco+006 say 'Local Entrega:'
         @tabli+lin,pcol()+1 say alltrim(endent) +' '+ alltrim(cident) +' '+ alltrim(estent)
         @tabli+lin,pcol()+1 say alltrim(cepent)
      endif

      **------------------- Calculo do Imposto --------------------------------------
      if valicm > 0
         @tabli+49,tabco+002 say baseicm   picture '999,999,999.99'      && base icms
         fvvicm = round(valicm,2)
         @tabli+49,tabco+028 say valicm    picture '999,999,999.99'      && Vlr icms
         ** @tabli+49,tabco+054 say '*'                                  && Base Icmssubstituicao
         ** @tabli+49,tabco+082 say '*'                                  && Vlr. Icms substituicao
      endif
      FVGTOT = VALT
      **     @40,070 say VALT          picture "999,999,999.99"       &&-- Vlr. mercadorias
      @tabli+49,tabco+110 say valt      picture '999,999,999.99'      && Total Produtos
      @tabli+51,tabco+002 say 0 pict '@z 999999999.99'                && Vlr frete
      @tabli+51,tabco+028 say 0 pict '@z 999999999.99'                && Vlr Seguro
      @tabli+51,tabco+054 say 0 pict '@z 999999999.99'                && Outras despesas
      **      @40,091 say VALIPI        picture '999,999,999.99'      &&-- Vlr. IPI
      @tabli+51,tabco+082 say valIPI      picture '@Z 999,999,999.99' && Vlr IPI
      V = (VALT+VALIPI)
      **      @40,118 say V             picture "99,999,999,999.99" &&-- Vlr. Total NF.
      @tabli+51,tabco+110 say v     picture '999,999,999.99' && Total NF
      GIPI = VALIPI
      **------------------- Transportador / Volumes Transportados ------------------
      sele obsnota
      @tabli+54,tabco+002 say raztra                     && Razao social
      @tabli+54,tabco+077 say 2              pict '@z 9' && Frete por conta do emitente
      @tabli+54,tabco+087 say veiculo                    && Placa ve¡culo
      @tabli+54,tabco+100 say esttra                     && Estado
      @tabli+54,tabco+110 say cgctra                     && CGC
      @tabli+56,tabco+002 say endtra                     && Endereco
      @tabli+56,tabco+065 say cidtra                     && Cidade
      @tabli+56,tabco+100 say esttra                     && UF
      @tabli+56,tabco+110 say instra                     && Inscricao
      @tabli+58,tabco+010 say QTDVOL   pict '99,999'      && Qtd Volumes
      @tabli+58,tabco+023 say ESPECIE                     && Especie
      ** @tabli+58,tabco+045 say '*'                      && Marca
      ** @tabli+58,tabco+065 say '*'                      && Numero
      @tabli+58,tabco+095 say PBRUTO   pict '99,999.99'   && Peso Bruto
      @tabli+58,tabco+120 say PLIQUIDO pict '99,999.99'   && Peso Liquido
      if MEDUP = 'S'
         GTOT = VALT
         VDUP1 = 0.00
         VDUP2 = 0.00
         DO AV07422
         if VDUP1 > 0
            **            @44,006 say PADL(MNNF,6,'0')+'/1'
            **            @44,016 say VDUP1        picture "99999,999,999.99"
            **            @44,042 say VENDUP1
            @tabli+62,tabco+001 say padl(MNNF,6,'0')+'/1'
            @tabli+62,tabco+013 say VDUP1                  picture "99999,999,999.99"
            @tabli+62,tabco+035 say VENDUP1
            sele DUPL
            append blank
            repl CGC       with MCGC         ,NDUP     with MNNF      ,;
                 PDUP      with 1            ,VDP      with VDUP1     ,;
                 EMIS      with DTOC(HOJE) ,VENC     with VENDUP1   ,;
                 NBANCO    with MNBANCO      ,AGENCIA  with MAGENCIA
         endif
         ** @44,080 say ESTENT+'   '+CEPENT
         if VDUP2 > 0
            **            @45,006 say PADL(MNNF,6,'0')+'/2'
            **            @45,016 say VDUP2        picture "99999,999,999.99"
            **            @45,042 say VENDUP2
            @tabli+63,tabco+001 say padl(MNNF,6,'0')+'/2'
            @tabli+63,tabco+013 say VDUP2                  picture "99999,999,999.99"
            @tabli+63,tabco+035 say VENDUP2
            sele DUPL
            append blank
            repl CGC      with MCGC         ,NDUP     with MNNF     ,;
                 PDUP     with 2            ,VDP      with VDUP2    ,;
                 EMIS     with DTOC(HOJE)   ,VENC     with VENDUP2  ,;
                 NBANCO   with MNBANCO      ,AGENCIA  with MAGENCIA
         endif
         **         @45,080 say MENDC
         if VDUP3 > 0
            **            @46,006 say PADL(MNNF,6,'0')+'/3'
            **            @46,016 say VDUP3        picture "99999,999,999.99"
            **            @46,042 say VENDUP3
            @tabli+64,tabco+001 say padl(MNNF,6,'0')+'/3'
            @tabli+64,tabco+013 say VDUP3                  picture "99999,999,999.99"
            @tabli+64,tabco+035 say VENDUP3
            sele DUPL
            append blank
            repl CGC    with MCGC         ,NDUP     with MNNF        ,;
                 PDUP   with 3            ,VDP      with VDUP3       ,;
                 EMIS   with DTOC(HOJE) ,VENC     with VENDUP3     ,;
                 NBANCO with MNBANCO      ,AGENCIA  with MAGENCIA
         endif
         **         @46,080 say MCIDC+'   '+MBAIC
         if VDUP4 > 0
            **            @47,006 say PADL(MNNF,6,'0')+'/4'
            **            @47,016 say VDUP4        picture "99999,999,999.99"
            **            @47,042 say VENDUP4
            @tabli+65,tabco+001 say padl(MNNF,6,'0')+'/4'
            @tabli+65,tabco+013 say VDUP4                  picture "99999,999,999.99"
            @tabli+65,tabco+035 say VENDUP4
            sele DUPL
            append blank
            repl CGC      with MCGC          ,NDUP    with   MNNF     ,;
                 PDUP     with 4             ,VDP     with   VDUP4    ,;
                 EMIS     with DTOC(HOJE)  ,VENC    with   VENDUP4  ,;
                 NBANCO   with MNBANCO       ,AGENCIA with   MAGENCIA
         endif
         **         @47,080 say MESTC+'   '+MCEPC
      endif
      **      sele OBSNOTA
      **      @48,099 say QTDVOL          picture '99,999'
      **      @48,126 say PLIQUIDO        picture '99,999.99'
      **      @49,021 say RAZTRA
      **      @49,099 say ESPECIE
      **      @49,126 say PBRUTO          picture '99,999.99'
      **      MPLIQUIDO = 0.00
      VALT   = 0.00
      **      @50,021 say ENDTRA
      **      @51,021 say CIDTRA
      **      @51,060 say ESTTRA
      **      @51,070 say MMNF1
      **      @52,021 say VEICULO
      **      @52,070 say MMNF2
      **      @55,007 say DTOC(HOJE)
      **      @55,061 say MNOPE
      **      @55,073 say MCGC
      **      @55,107 say MINS
      **      @55,132 say MEST
      **      @58,013 say PADL(MNNF,6,'0')
      **      INNF = MNNF
      sele PARAM
      repl NNF       with MNNF
      repl DTEMISSAO with HOJE
   @tabli+67,tabco+002 say mgue
   @tabli+67,tabco+047 say cSigla      
   @tabli+71,tabco+118 say CHR(27)+'w'+CHR(1)
   @tabli+71,pcol() say mnnf pict '9 9 9 9 9 9'
   @tabli+71,pcol() say CHR(27)+'w'+CHR(0)      &&--- N£mero
   @78,00
   SELE CABECNF
   IF ! SEEK( MNNF )
     APPEND BLANK
   ENDIF
      
   repl CGC      WITH CLIEN->CGC,;
        DEMISSAO WITH HOJE,;
        VBASEICM with 0,;
        NNF      with MNNF,;
        VEN      with SPACE(04),;
        REGIAO   with SPACE(04),;
        IDREGIAO with 0,;
        PBRUTO   with OBSNOTA->PBRUTO ,;
        PLIQUIDO with OBSNOTA->PLIQUIDO,;
        VEICULO  with obsnota->VEICULO,;
        QTDVOL   with obsnota->QTDVOL ,;
        ESPECIE  with obsnota->ESPECIE,;
        VALPROD  with VALT,;
        VALIPI   with M->VALIPI,;
        VALICM   with M->VALICM,;
        CANCELADA with .F.,;
        DTCANCEL  WITH CTOD(''),;
        TRA       WITH MTRA,;
        TIPNF     WITH MTIP,;
        PED       WITH 0,;
        CP1       WITH 0,;
        CP2       WITH 0,; 
        CP3       WITH 0,;
        CP4       WITH 0,;
        PE1       WITH 0,;
        PE2       WITH 0,;
        PE3       WITH 0,;
        PE4       WITH 0
  
   =INICPAG()
      ** @tabli+67,tabco+024 say pedic->pcl
      ** @tabli+67,tabco+040 say str(pedic2->ped,6)
      ** @tabli+67,tabco+060 say ven
      **      if BASEIPI > 0
      **         @58,079 say BASEIPI              picture "99,999,999,999.99"
      **         @58,097 say (BASEIPI/VALIPI)     picture "99.9"
      **         @58,104 say VALIPI               picture "99,999,999,999.99"
      **      endif
      ** @64,125 say PADL(MNNF,6,'0')
      MNNF = MNNF + 1
      ** sele NFDIVS
      exit
   endif
enddo

*do IMPSAIDA
set print OFF
set device to screen
exit
enddo
close data
set deci to
return
*ßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßß
procedure AV07600OB
parameters vpar1, vpar2
if !empty(vpar1)
   lin = lin + 1
   @tabli+lin,tabco+011 say vpar1
   lin = lin + 1
   @tabli+lin,tabco+011 say vpar2
endif
return

*================
FUNCTION GETSIGLA
*================ 
  PRIVATE lRet
  
  lRet = .F.
  do while .t.
    sele siglas
    msigla = space(04)
    @23,01 say space(78)
    @23,10 say 'Informe sua Sigla 'get msigla pict '@!'
    read
    
    if seek(msigla)
      lRet = .T.
      exit
    else
      =mensag('Sigla n„o cadastrada - ENTER')
      =INKEY(0)
    endif
  enddo
  
RETURN lRet
