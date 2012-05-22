* AV060501  -  ATUALIZACAO DO CADASTRO DE PEDIDOS - 22/11/86
* SISTEMA DE ADMINISTRACAO DE VENDAS
* ADAO ELI PEREIRA - 24-04-90   -    fox 5/10/92
* UNIFICADO INCLUSAO/EXCLUSAO E ALTERACAO EM 12/09/93
*
vNOMEPRG = 'AV06050' 
set deci to 4
public vtela
close data
sele 0
use PROD  inde PROD.IDX
sele 0
use CLIEN ORDER P_CGC
sele 0
use VEND  order p_vend
sele 0
use TRANS inde TRANS.IDX, TRAGUE.IDX
sele 0
use TPNF  inde TPNF.IDX
sele 0
use NOMMOEDA inde NOMMOEDA.IDX
sele 0
use TABPRE   inde TABPRE.IDX
sele 0
use TABFIN2  inde TABFIN2.IDX
sele 0
use TABFIN   inde TABFIN.IDX
sele 0
use TABREF   INDEX TABREF.IDX
sele 0
use TABSEM   inde TABSEM.IDX
sele 0
use CTR_SENH inde CTR_SENH.IDX
sele 0
use PEDID ORDER P_PEDID
sele 0
use SIGLAS index SIGLAS.IDX
sele 0
use BANCOS index BANCOS.IDX
SELE 0
USE ATREGIAO ORDER P_ATREGIAO
SELE 0
USE REGIAO ORDER P_REGIAO
sele 0
use PEDIC ORDER P_PED
set relation to str(ped,6) into pedid,;
                cgc        into clien
set skip to clien,;
            pedid

stor 0    to MPED
stor 0.00 to TVALIQU, ATVALIQU, TVALIPI, ATVALIPI
MIDREPR   = 0
MIDREGIAO = 0
FINIPI = 0
SELE PEDIC
MDEM  = {}
MGUE  = space(15)
MCVR = SPACE(7)
mcgc  = space(20)
do while .t.
   =CABTELA(0,0,24,79)
   @01,MENSAGR('Manuten‡„o de Pedidos') say 'Manuten‡„o de Pedidos'
   @03,02 say 'Pedido-'
   @03,18 say 'Nome Guerra-'
   @03,47 say 'Repr-'
   @03,60 say 'Emiss„o-'
   @04,02 say 'Ped.Rep-'
   @04,20 say 'Data Ped.Rep-'
   @04,47 say 'P.Cli-'
   @04,60 say 'Dt.P.Cli-'
   @05,02 say 'Trans-'
   @05,47 say 'Entr-'
   @07,02 say 'Tipo NF-'
   @07,17 say 'Tab. Pre‡os-'
   @07,36 say 'Parc-'
   @07,44 say 'C.Pagt§ -'
   @08,15 say 'Dias-'
   @08,29 say 'Moeda-'
   @08,44 say 'Percent.-'
   @09,02 say 'D.Cliente-'
   @09,26 say 'D.Semanal-'
   @09,44 say 'C.Finan.-'
   @10,02 say 'N§ Banco-'
   @10,26 say 'N§ Agˆncia-'
   @10,50 say 'Situa‡„o-'
   @11,01 say replicate('-',78)
   @12,02 say 'It'
   @12,05 say 'Ref'
   @12,10 say 'T d P'
   @12,16 say 'Pre‡o Tabela'
   @12,30 say 'Quantid'
   @12,39 say 'D Gru'
   @12,45 say 'Promo'
   @12,51 say 'Bonus'
   @12,57 say 'Uni'
   @12,61 say '%IPI'
   @12,66 say 'Pr.Unit.L¡qu.'
   @13,01 CLEAR TO 20,78
   stor space(01) to maut, mcont
   stor space(03) to mtra, mtip, mcpd,mtra_opc
   stor space(04) to mref
   MVEND = EmptyRepr
   stor space(05) to mtabp
   stor space(06) to mpcl
   stor space(07) to ment, mordem
   stor {} to mden
   stor space(09) To mpint
   MDTPEDREP = {}
   MDTPEDCLI = {}
   stor space(15) to mguerra
   stor 0         to xmoeda, mdel, mcp1, mcp2, mcp3, mcp4, mpe1, mpe2, mpe3, mpe4, mparcelas
   stor 0.00      to mfre, memb, mdge, mpref, mprev, mdesconto
   stor 0.00      to mdescto1, mdescto2, mdescsem, mcfinanc, mnbanco, magencia, msituacao
   sele PEDIC
   go bott
   if !bof()
      MPED = PED + 1
   else
      MPED = 1
   endif
   @03,09 get mped pict '999999'
   read
   if lastkey() = 27
      if mdel = 1
         MENS = 'Houve exclus”es. Compacta arquivo de Pedidos ?'
         do PEGRESP with [SN]
         if MC = 'S'
            =MENSAG('Limpando as exclus”es do arquivo... Aguarde')
            sele PEDIC
            pack
            sele PEDID
            pack
         endif
      endif
      clos data
      set deci to
      return
   endif
   if  mped = 0
      =MENSAG(CNUMER)
      loop
   endif
   ALTERA = space(01)
   NOVOPED = .F.
   lVendaDir = .F.
   if !seek(MPED)
      MENS = 'Pedido n„o cadastrado - Inclui ?'
      do PEGRESP with [SN]
      if MC = 'N' .or. empty(mc)
         LOOP
      else
         ALTERA = 'S'
         NOVOPED = .T.
      endif
   else
      MCGC = CGC
      sele CLIEN
      SET ORDER TO P_CGC
      if seek(MCGC)
        MGUERRA = GUE
        MEST    = EST
      else
        =MENSAG('Cliente n„o cadastrado - ENTER')
        =INKEY(0)
        LOOP
      endif
      sele PEDIC
      MVEND     = FMTREPR(VEN,IDREPR)
      MCVR      = VEN
      MIDREPR   = IDREPR
      MDEM      = DEM
      MPINT     = PINT      
      MDTPEDREP = DTPEDREP      
      MPCL      = PCL      
      MDTPEDCLI = DTPEDCLI      
      MTRA      = TRA      
      @03,31 say mguerra color &corver
      @03,52 say MVEND    
      @03,69 say dem     color &corver
      @04,10 say pint    color &corver
      @04,33 say DTPEDREP    color &corver
      @04,53 say pcl     color &corver
      @04,69 say DTPEDCLI color &corver
      @05,08 say mtra    color &corver
      if seek(MTRA,'trans')
         @05,12 say trans->GUETRA 
      endif
      sele PEDIC
      @05,53 say den     color &corver
      MDEN = DEN
      @07,10 say tip pict '999' color &corver
      mtip = TIP
      @07,29 say tabp      color &corver
      mtabp = tabp
      @07,41 say parcelas pict '9' color &corver
      mparcelas = parcelas
      @07,53 say cp1 pict '999' color &corver
      MCP1 = CP1
      @08,53 say pe1 pict '999' color &corver
      MPE1 = PE1
      @07,57 say cp2 pict '999' color &corver
      MCP2 = CP2
      @08,57 say pe2 pict '999' color &corver
      MPE2 = PE2
      @07,61 say cp3 pict '999' color &corver
      MCP3 = CP3
      @08,61 say pe3 pict '999' color &corver
      MPE3 = PE3
      @07,65 say cp4 pict '999' color &corver
      MCP4 = CP4
      @08,65 say pe4 pict '999' color &corver
      MPE4 = PE4
      @08,21 say cpd pict '!!!' color &corver
      MCPD = CPD
      @08,36 say moeda   pict '999' color &corver
      xmoeda = MOEDA
      @09,12 say descto1 pict '99.99' color &corver
      *---- Ignorando o desconto padrao do cliente
      MDESCTO1 = 0
      @09,18 say descto2 pict '99.99' color &corver
      MDESCTO2 = 0
      @09,36 say descsem pict '99.99' color &corver
      MDESCSEM = DESCSEM
      @09,54 say cfinanc pict '99.99' color &corver
      MCFINANC = CFINANC
      @10,12 say nbanco pict '999'    color &corver
      Mnbanco = nbanco
      @10,38 say agencia pict '9999'  color &corver
      Magencia = agencia
      if situacao = 1
         @10,60 say '1-Pronto   ' 
      else
         if situacao = 2
            @10,60 say '2-Expedi‡„o' 
         else
            if situacao = 3
               @10,60 say '3-Cancelado' 
            else
               @10,60 say '0-Pendente ' 
            endif
         endif
      endif
      msituacao = situacao
      MO1 = O1
      MO2 = O2
      MO3 = O3
      MO4 = O4
      MO5 = O5
      MO6 = O6
      mens = testeinc
      do pegopcao with [123]
      if opcao = '1'
         loop
      endif
      if opcao = '3'
         do senha
         sele PEDIC
         delete
         sele PEDID
         if seek(STR(MPED,6))
            do while PED = MPED
               DELE
               SKIP
            enddo
         endif
         mdel = 1
         loop
      endif
      if opcao = '2'
         do senha
      endif
   endif
   if OPCAO = '2' .OR. ALTERA = 'S'
      SAVE SCRE TO TELAINI
      sele CLIEN
      SET ORDER TO X_GUE
      =MENSAG('Entre com o Nome de Guerra ou letras iniciais e pressione ENTER')
      @03,31 get MGUERRA pict '@!' valid av060501va()
      read
      if lastkey() = 27
         close data
         set deci to
         return
      endif
      
      SET NEAR ON
      IF ! SEEK( MGUERRA, 'CLIEN' )
        ? CHR(07)
        =MOSTRA(12,01,21,78)
        BROWSE FIELDS GUE:R:H='N. Guerra',CGC:R:H='CGC/Loja', ;
        CID:R:H='Cidade',END:R:H='Endere‡o' IN WINDOW TELA
        CLEAR WINDOW TELA
        mguerra = GUE
      ENDIF  
      SET NEAR OFF
      
      MCGC = CGC
      
*      IF ! FCGC(MCGC)
*        =MENSAG(MENSCGC)
*        ?? CHR(07)
*        =INKEY(0)
*        LOOP
*      ENDIF  
      
      MEST      = EST
      
      IF NOVOPED
        lVendaDir = VEN = '0199'
        IF lVendaDir
          MVEND = FMTREPR('0199',7)
          MCVR    = '0199'
          MIDREPR = 7
        ELSE
          * Usa o Representante do Cliente se houver        
          IF CLIEN->IDREPR <> 0       
            MVEND    = FMTREPR(CLIEN->VEN,CLIEN->IDREPR)
            MCVR     = CLIEN->VEN  
            MIDREPR  = CLIEN->IDREPR
          ELSE
            MVEND    = '0000.00'
            MCVR     = '0000'
            MIDREPR  = '00'
          ENDIF  
        ENDIF  
      ELSE
        MVEND   = FMTREPR(PEDIC->VEN,PEDIC->IDREPR)
        MCVR    = PEDIC->VEN 
        MIDREPR = PEDIC->IDREPR
      ENDIF    
        
      MIDREGIAO = CLIEN->IDREGIAO
      =SEEK( MIDREGIAO, 'REGIAO' )
      MTRA      = TRA
     
      MTRA_OPC = TRA_OPC
      *---- Ignorando o desconto padrao
      MDESCTO1 = 0
      MDESCTO2 = 0
      mnbanco  = nbanco
      magencia = agencia
      do ESCONDE
      @03,31 say mguerra  color &corver
      MENS = 'Cliente selecionado est  correto ?'
      do PEGRESP with [SN]
      if MC = 'N' .or. empty(mc)
        LOOP
      endif
      =MENSAG('')
      MDEM = DATE()
      stor 0 to mcfin2, mcfin3, mcfin4
      save screen to vtela
      * Seleciona o Vendedor da regiao
      @03,52 get MVEND  valid ValidRepr()
      @03,69 get mdem                                                   pict '99/99/99'
      @04,10 get mpint
      @04,33 get mdtpedrep
      @04,53 get mpcl
      @04,69 get mdtpedcli
      @05,08 get mtra      when av060501wh() valid av060501va()         pict '999'
      @05,53 get mden                                                   pict '99/99/99'
      @07,10 get mtip      when av060501wh() valid av060501va()         pict '999'
      @07,29 get mtabp     when av060501wh() valid av060501va()         pict '99/99'
      @07,41 get mparcelas when av060501wh() valid av060501va()         pict '9'
      @07,53 get mcp1      when av060501wh() valid av060501va()         pict '999'
      @07,57 get mcp2      when av060501wh() valid av060501va()         pict '999'
      @07,61 get mcp3      when av060501wh() valid av060501va()         pict '999'
      @07,65 get mcp4      when av060501wh() valid av060501va()         pict '999'
      @08,53 get mpe1      when av060501wh() valid av060501va()         pict '999'
      @08,57 get mpe2      when av060501wh() valid av060501va()         pict '999'
      @08,61 get mpe3      when av060501wh() valid av060501va()         pict '999'
      @08,65 get mpe4      when av060501wh() valid av060501va()         pict '999'
      @09,54 get mcfinanc  when av060501wh() valid av060501va(mcfinanc) pict '99.99'
      @08,21 get mcpd      when av060501wh() valid av060501va()         pict '!!!'
      @08,36 get xmoeda    when av060501wh() valid av060501va()         pict '999'
      @09,12 get mdescto1  when av060501wh() valid av060501va()         pict '99.99'
      @09,18 get mdescto2  when av060501wh() valid av060501va()         pict '99.99'
      @09,36 get MDESCSEM  when av060501wh() valid av060501va()         pict '99.99'
      @10,12 get mnbanco   when av060501wh() valid av060501va()         pict '999'
      @10,38 get magencia  when av060501wh() valid av060501va()         pict '9999'
      @10,60 get msituacao when av060501wh() valid av060501va()         pict '9' range 0,3
      read
      rest screen from vtela
      if lastkey() # 13 .and. lastkey() # 19
         close data
         set deci to
         return
      endif
      MENS = DADCORR
      do PEGRESP with [SN]
      if mc = 'N' .or. empty(mc)
         loop
      else
         do while .t.
            sele siglas
            msigla = space(04)
            @23,01 say space(78)
            @23,10 say 'Informe sua Sigla 'get msigla pict '@!'
            read
            if seek(msigla)
               exit
            else
               =mensag('Sigla n„o cadastrada - ENTER')
               =INKEY(0)
            endif
         enddo
      endif
 
      sele CLIEN
      SET ORDER TO X_GUE
      if seek(mguerra)
         cRegiao = CLIEN->REGIAO
         repl ulc with date(), alt with '*'
      endif
      * Gravar os dados no Cabe‡alho de Pedido
      sele PEDIC
      MIDREPR = VAL(SUBSTR(MVEND,6,2))
      MCVR    = SUBSTR(MVEND,1,4)
      if !seek(mped)
        appe blan
      endif
      repl cgc      with mcgc,;
           ven      with MCVR,;
           IDREPR   WITH MIDREPR,;
           dem      with mdem,;
           den      with mden,;
           pcl      with mpcl,;
           dtpedcli with mdtpedcli
      repl ped      with mped,;
           pint     with mpint,;
           dtpedrep with mdtpedrep,;
           moeda    with xmoeda
      repl tra with mtra,descto1 with mdescto1,descto2 with mdescto2
      repl aut with maut,cp1 with mcp1,tip with mtip
      repl cp2 with mcp2,cp3 with mcp3,cp4 with mcp4,cpd with mcpd
      repl pe1 with mpe1,pe2 with mpe2,pe3 with mpe3,pe4 with mpe4
      repl descsem with mdescsem,cfinanc with mcfinanc,cfin2 with mcfin2
      repl cfin3 with mcfin3, cfin4 with mcfin4
      repl tabp   with mtabp
      repl parcelas with mparcelas
      repl sigla with msigla
      repl nbanco with mnbanco, agencia with magencia, situacao with msituacao
      REPL REGIAO WITH cRegiao
      repl IDREGIAO WITH MIDREGIAO
   endif
   ** --------------------------------------------------------------------------
   **                            ITENS do PEDIDO
   ** -------------------------------------------------------------------------
   =MENSAG(ECODIGO)
   @13,01 CLEAR TO 21,78
   lin  = 13
   mtabref = space(05)
   mdescgru = 0.00
   @04,02 clear to 10,77
   LINTELA = PAR4_P1+'/'+PAR1_P2
   @04,02 to 10,77 DOUBLE color &LINTELA
   @05,04 say 'Descontos'
   @05,40 say 'Resumo'
   @05,62 say 'Liq + IPI = Tot'
   @06,04 say 'Grupo'
   @06,31 say 'Descto Semanal'
   @07,04 say 'Cliente'
   @07,31 say 'Custo Financ'
   @08,04 say 'Promoc'
   @08,31 say 'Valor do Item'
   @08,63 say '-------------'
   @09,04 say 'Bonus'
   @09,31 say 'Valor do IPI'
   stor 0.00 to  ADFINANC, ADFIN2, ADGRUPO, ADCLIENTE1, ADCLIENTE2, ADMDIT
   stor 0.00 to  ADSEMANA, ADBONUS, AVALIPI, AVALIQU, AVALTOTAL, ATVALIQU
   stor 0.00 to  ATVALIPI, DFINANC, DGRUPO, DCLIENTE1, DCLIENTE2, DMDIT
   stor 0.00 to  DSEMANA, DBONUS, VALIPI, TVALIPI, VALIQU, VALTOTAL, TVALIQU
   do while .t.
      stor 0 to mite, mqtd, qtdped
      mref   = space(04)
      mipi   = 0.0
      stor 0.00 to mdit, mbon, mpre
      mdescricao = space(53)
      sele PEDID
      if mped > 3500
         @lin,02 get mite pict '99' valid mite>=0 .and.  mite <= 20 message 'Informe N£mero do item inferior a 20'
      else
         @lin,02 get mite pict '99'
      endif
      read
      SAVE SCRE TO TELA
      SAVE SCRE TO TELAINI
      if LASTKEY() = 27
         do PEDOBS
         REST SCRE FROM TELAINI
         exit
      endif
      if seek(str(mped,6) + str(mite,2))
         @lin,05 say ref color &corver
         mref = ref
         sele PROD
         if seek(mref)
            mgrupo = grupo
            muni   = uni
            IF TPNF->IPIT = '2'
              mipi = 0
            ELSE
              mipi = ipi  
            ENDIF  
            mccl   = ccl
            @lin+1,05 say embalagem color &corver
            @lin+1,10 say des color &corver
            @lin,57 say muni  color &corver
            @lin,61 say mipi pict '99.9' color &corver
         endif
         sele PEDID
         @LIN,10 say mtabp                color &corver
         mtabref = mtabp
         @lin,16 say prebru pict '999,999.9999' color &corver
         mprebru = prebru
         @lin,30 say QTD pict '99999999' color &corver
         mqtd = qtd
         qtdped = qtd
         @lin,39 say descgru pict '99.99' color &corver
         mdescgru = descgru
         @LIN,45 say DIT pict '99.99' color &corver
         mdit = dit
         @LIN,51 say BON pict '99.99' color &corver
         mbon = bon
         stor 0.00 to ADFINANC, ADFIN2, ADGRUPO, ADCLIENTE1, ADCLIENTE2
         stor 0.00 to  ADMDIT, ADSEMANA, ADBONUS, AVALIPI, AVALIQU
         stor 0.00 to  AVALTOTAL, ATVALIQU, ATVALIPI, TADCLIENTE1
         AVALIQU = (MQTD * MPREBRU)
         do FORA_JACAD
         MENS = 'Cadastrado. [1] Retorna, [2] Altera item, [3] Exclui item do Pedido '
         do PEGRESP with [123]
         if MC = '3'
            DELE
            LOOP
         endif
         if MC = '1' .or. empty(mc)
            LIN = LIN + 2
            if lin > 20
               @13,01 CLEAR TO 21,78
               lin = 13
            endif
            LOOP
         endif
      else
         MENS = 'N„o cadastrado no Pedido. Inclui ?'
         do PEGRESP with [SN]
         if MC = 'N' .or. empty(mc)
            LOOP
         endif
      endif
      sele PROD
      =MENSAG(ECODIGO)
      @lin,05 get mref
      read
      if LASTKEY() = 27
         do PEDOBS
         REST SCRE FROM TELAINI
         exit
      endif
      if EMPTY(mref)
         =MENSAG(CALPHA)
         loop
      endif
      if !seek(mref)
         =MENSAG(CORRIJA)
         =INKEY(0)
         LOOP
      else
         mgrupo = grupo
         muni   = uni
         IF TPNF->IPIT = '2'
           mipi = 0 
         ELSE  
           mipi   = ipi
         ENDIF  
         mccl   = ccl
         @lin+1,05 say embalagem color &corver
         @lin+1,10 say des color &corver
         @lin,57 say muni  color &corver
         @lin,61 say mipi pict '99.9' color &corver
      endif
      mtabref = mtabp
      @LIN,10 say mtabref color &corver
      sele TABREF
      if seek(mtabref + mref)
         mpre = preco
         mprebru = preco
         @lin,16 say mpre pict '999,999.9999' color &corver
      else
         =mensag('Sem tabela de Pre‡os cadastrada - ENTER')
         =INKEY(0)
         loop
      endif
      =mensag('Informe a quantidade pedida')
      do while .t.
         @lin,30 get MQTD pict '99999999'
         read
         if mqtd > 0
            exit
         else
            =mensag(cnumer)
         endif
      enddo
      dqtd = (mqtd - qtdped)
      MMES = SUBSTR(DTOC(MDEM),4,2)
      MANO = SUBSTR(DTOC(MDEM),7,2)
      sele TABPRE
      if seek(MGRUPO+MANO)
         KEYPRE = 'PER'+MMES
         mdescgru = &KEYPRE
         @lin,39 get mdescgru pict '99.99' color &corver
      else
         =mensag('Sem Desconto de Grupo cadastrado - ENTER')
         =INKEY(0)
         loop
      endif
      =MENSAG('Informe o percentual de desconto do ¡tem, se concedido')
      @LIN,45 get MDIT pict '99.99'
      read
      =MENSAG('Informe o percentual de bonifica‡„o do ¡tem, se concedido')
      @LIN,51 get MBON pict '99.99'
      read
      stor 0.00 to DFINANC, DGRUPO, DCLIENTE1, DCLIENTE2, DMDIT, DSEMANA
      stor 0.00 to DBONUS, VALIPI, VALIQU, VALTOTAL, TDCLIENTE1
      VALIQU = (MQTD * MPREBRU)
      if MDESCGRU > 0
         DGRUPO   = round((VALIQU * MDESCGRU) / 100,2)
         @06,14 say DGRUPO pict '99,999,999.99' color &corver
      endif
      VALIQU   = (VALIQU - DGRUPO)
      do FORA_NAOCAD
      MPRE = round(VALIQU / MQTD,4)
      MPRER = MPRE

      @LIN,66 say mprer pict '9999,999.9999' color &corver
      MENS = DADCORR
      do PEGRESP with [SN]
      if MC = 'N' .or. empty(mc)
         stor 0.00 to VALIQU, VALIPI, VALTOTAL, DGRUPO, DCLIENTE, DMDIT
         stor 0.00 to DBONUS, DSEMANA, DFINANC
         if LIN > 13
            LIN = LIN - 2
         endif
         LOOP
      endif
      sele PROD
      if seek(MREF)
         repl vam with (vam + dqtd)
         repl vaa with (vaa + dqtd)
         repl pea with (pea + dqtd)
      endif
      sele PEDID
      if !seek(STR(MPED,6) + STR(MITE,2))
         appe blan
      endif
      repl ped       with mped           ,      cgc       with mcgc
      repl ite       with mite           ,      ref       with mref
      repl ccl       with mccl           ,      uni       with muni
      repl ipi       with mipi           ,      qtd       with mqtd
      repl sal       with mqtd           ,      dit       with mdit
      repl bon       with mbon           ,      pre       with mprer
      repl prebru    with mprebru        ,      descgru   with mdescgru
      repl descricao with mdescricao     ,      tabref    with mtabref
      lin = LIN + 2
      if lin > 20
         @13,01 CLEAR TO 21,78
         lin = 13
      endif
      =MENSAG(ECODIGO)
   enddo
enddo
clos data
set deci to
return

******************
PROCEDURE PEDOBS
*****************
MENS = 'Tem valores para Desconto ?'
do PEGRESP with [NS]
if MC = 'S'
   do while .t.
      @07,08 CLEAR TO 11,44
      @07,08 to 11,44
      @09,10 say 'Valor do Desconto'
      =MENSAG('Informe o Valor do Desconto Fixo, se concedido')
      @09,30 get mdesconto pict '99,999,999.99'
      read
      MENS = DADCORR
      do PEGRESP with [SN]
      if MC = 'S'
         exit
      else
         LOOP
      endif
   enddo
   sele PEDIC
   if seek(mped)
      repl desconto with mdesconto
   endif
endif
sele PEDIC
if seek(mped)
   MO1 = O1
   MO2 = O2
   MO3 = O3
   MO4 = O4
   MO5 = O5
   MO6 = O6
else
   stor "" to mo1,mo2,mo3,mo4,mo5,mo6
endif
REST SCRE FROM TELA
@13,14 CLEAR TO 20,70
@13,14 TO 20,70 DOUBLE
@13,16 say 'Observa‡”es a imprimir no Pedido:'
@14,16 say MO1
@15,16 say MO2
@16,16 say MO3
@17,16 say MO4
@18,16 say MO5
@19,16 say MO6
MENS = 'Pedido com observa‡”es especiais ?'
do PEGRESP with [NS]
if MC = 'S'
   @12,14 CLEAR TO 20,70
   @12,14 TO 20,70 DOUBLE
   @13,16 say 'Observa‡”es a imprimir no Pedido:'
   do while .t.
      @14,16 get MO1
      @15,16 get MO2
      @16,16 get MO3
      @17,16 get MO4
      @18,16 get MO5
      @19,16 get MO6
      read
      MENS = DADCORR
      do PEGRESP with [SN]
      if MC = 'S'
         sele PEDIC
         repl O1 with MO1,O2 with MO2,O3 with MO3
         repl O4 with MO4,O5 with MO5,O6 with MO6
         exit
      endif
   enddo
endif
return


* Nenhum modulo usa as funcoes SP_JACAD, SP_NAOCAD !?

*---------- CLIENTES DE SAO PAULO - ALTERACAO DE ITENS
******************
PROCEDURE SP_JACAD
******************

return

*---------- CLIENTES DE SAO PAULO - INCLUSAO DE ITENS
*******************
PROCEDURE SP_NAOCAD
*******************

return


*----------- CLIENTES FORA DE SAO PAULO - ALTERACAO DE ITENS
********************
PROCEDURE FORA_JACAD
********************
  
 if MDESCGRU > 0
   ADGRUPO   = round((AVALIQU * MDESCGRU) / 100,2)
   @06,14 say ADGRUPO pict '99,999,999.99' color &corver
 endif
  
 AVALIQU   = (AVALIQU - ADGRUPO)
 if MDESCTO1 > 0
    ADCLIENTE1 = round((AVALIQU * MDESCTO1) / 100,2)
 endif
 AVALIQU   = (AVALIQU - ADCLIENTE1)
 if MDESCTO2 > 0
   ADCLIENTE2 = round((AVALIQU * MDESCTO2) / 100,2)
 endif
 AVALIQU   = (AVALIQU - ADCLIENTE2)
 if (ADCLIENTE1 + ADCLIENTE2) > 0
   @07,14 say (ADCLIENTE1 + ADCLIENTE2) pict '99,999,999.99' color &corver
 endif
 if MDIT > 0
   ADMDIT    = round((AVALIQU * MDIT) / 100,2)
   @08,14 say ADMDIT pict '99,999,999.99' color &corver
 endif
 AVALIQU   = (AVALIQU - ADMDIT)
 if MBON > 0
   ADBONUS   = round((AVALIQU * MBON) / 100,2)
   @09,14 say ADBONUS pict '99,999,999.99' color &corver
 endif
 AVALIQU   = (AVALIQU - ADBONUS)
 if MDESCSEM > 0
   ADSEMANA  = round((AVALIQU * MDESCSEM) / 100,2)
   @06,47 say ADSEMANA pict '99,999,999.99' color &corver
 endif
 AVALIQU   = (AVALIQU - ADSEMANA)
 
 * Calculo da Despesa Financeira
 
 nDesp = DespFinanc( AVALIQU, MCFINANC, MCP1, MPE1,; 
                                        MCP2, MPE2,;
                                        MCP3, MPE3,;
                                        MCP4, MPE4 )
 Fator = ((AVALIQU + nDesp) / AVALIQU) - 1

 if MIPI > 0
   AVALIPI = round(((AVALIQU * MIPI) / 100),2) 
   FINIPI  = AVALIPI * FATOR
   AVALIPI = AVALIPI + FINIPI
 endif
 
 FINLIQ  = AVALIQU * FATOR
 AVALIQU = AVALIQU + FINLIQ

 @09,47 say AVALIPI       pict '99,999,999.99' color &corver 
 @07,47 say FINLIQ+FINIPI pict '99,999,999.99' color &corver
 @08,47 say AVALIQU       pict '99,999,999.99' color &corver
 
 ATVALIPI = (ATVALIPI + AVALIPI)

 *--- Provisoriamente
 REPLACE PEDIC->VALPED WITH ROUND(ATVALIQU,2)
 REPLACE PEDIC->VALIPI WITH ROUND(ATVALIPI,2)
return


*------------- CLIENTES FORA DE SAO PAULO - INCLUSAO DE ITEM
*********************
PROCEDURE FORA_NAOCAD
*********************
  
  if MDESCTO1 > 0
     DCLIENTE1 = round(((VALIQU * MDESCTO1) / 100),2)
  endif
  VALIQU   = (VALIQU - DCLIENTE1)
  if MDESCTO2 > 0
    DCLIENTE2 = round(((VALIQU * MDESCTO2) / 100),2)
  endif
  VALIQU   = (VALIQU - DCLIENTE2)
  if (DCLIENTE1 + DCLIENTE2) > 0
    @07,14 say (DCLIENTE1 + DCLIENTE2) pict '99,999,999.99' color &corver
  endif
  if MDIT > 0
    DMDIT    = round(((VALIQU * MDIT) / 100),2)
    @08,14 say DMDIT pict '99,999,999.99' color &corver
  endif
  VALIQU   = (VALIQU - DMDIT)
  if MBON > 0
    DBONUS   = round(((VALIQU * MBON) / 100),2)
    @09,14 say DBONUS pict '99,999,999.99' color &corver
  endif
  VALIQU   = (VALIQU - DBONUS)
  if MDESCSEM > 0
    DSEMANA  = round(((VALIQU * MDESCSEM) / 100),2)
    @06,47 say DSEMANA pict '99,999,999.99' color &corver
  endif
  VALIQU   = (VALIQU - DSEMANA)

  * Calculo da Despesa Financeira
 
  nDesp = DespFinanc(VALIQU, PEDIC->CFINANC, PEDIC->CP1, PEDIC->PE1,;
                                             PEDIC->CP2, PEDIC->PE2,;
                                             PEDIC->CP3, PEDIC->PE3,;
                                             PEDIC->CP4, PEDIC->PE4 )
  FATOR = ((VALIQU + nDesp) / VALIQU) - 1

  if MIPI > 0
    VALIPI = round(((VALIQU * MIPI) / 100),2)
    FINIPI = VALIPI * FATOR
    VALIPI = VALIPI + FINIPI
  endif

  FINLIQ = VALIQU * FATOR
  VALIQU = VALIQU + FINLIQ  
 
  TVALIQU = (TVALIQU + VALIQU)
  TVALIPI = (TVALIPI + VALIPI)
  
  @06,63 say ROUN(TVALIQU,2) pict '99,999,999.99' color &corver
  @07,63 say ROUN(TVALIPI,2) pict '99,999,999.99' color &corver
  @07,47 say FINLIQ+FINIPI   pict '99,999,999.99' color &corver
  @08,47 say VALIQU          pict '99,999,999.99' color &corver
  @09,47 say VALIPI          pict '99,999,999.99' color &corver
  @09,63 say (ROUN(TVALIQU,2) + ROUND(TVALIPI,2)) pict '99,999,999.99' color &corver

  *--- Provisoriamente
  REPLACE PEDIC->VALPED WITH ROUND(TVALIQU,2)
  REPLACE PEDIC->VALIPI WITH ROUND(TVALIPI,2)
  
return

*******************
function av060501wh
*******************
*  if varread() = 'MVEND'
*    =MENSAG('Se C¢digo do Representante n„o for o cadastrado, substitua-o')
*    return .t.
*  endif
  if varread() = 'MDEM'
     =MENSAG('Informe a data da emiss„o do Pedido no formato (DD/MM/AA)')
     return .t.
  endif
  if varread() = 'MPINT' .or. varread() = 'MPC'
     =MENSAG('Informe os n£meros dos Pedidos de Controle Interno e do Cliente')
     return .t.
  endif
  if varread() = 'MTRA'
    @05,08 say mtra
    if !empty(MTRA_OPC)
      @06,09 say MTRA_OPC color &corver
      if seek(MTRA_OPC,'trans')
         @06,13 say trans->GUETRA    
      else
         @06,13 say 'NAO CADASTRADA'
      endif
    endif
    =seek(MTRA,'trans')
    @05,12 say trans->GUETRA 
    if !empty(MTRA_OPC)
      MENS = 'Transportadora Normal (N) ou Opcional (O) ?'
      do PEGRESP with [NO]
      if MC = 'O'
         MTRA = MTRA_OPC
         @05,08 say spac(len(mtra_opc))
         @05,12 say spac(len(trans->guetra))
      endif
    endif
    =MENSAG('Se C¢digo da Transportadora n„o for o do cadastro, substitua-o')
    return .t.
  endif
  if varread() = 'MTIP'
    =MENSAG('Informe o Tipo de Nota Fiscal a ser emitida - [ESC]) encerra')
    return .t. 
  endif
  if varread() = 'MTABP'
     if ! empty( clien->ves )
        =mensag( 'O Cliente tem vencimento especial definido' )
        MCP1 = VAL(SUBSTR( VES,  1, 2 ))
        MCP2 = VAL(SUBSTR( VES,  4, 2 ))
        MCP3 = VAL(SUBSTR( VES,  7, 2 ))
        MCP4 = VAL(SUBSTR( VES, 10, 2 ))
     else    
        =mensag('Entre com uma tabela de pre‡os v lida - F1 para ajuda')
     endif
     return .t.
  endif
  if varread() = 'MCP2' 
    IF MCP2 = 0
       MCP3 = 0
       MPE2 = 0
       MPE3 = 0
    ENDIF
    RETURN .T.
  endif
  if varread() = 'MCP3'
    IF MCP3 = 0
      MCP4 = 0
      MPE4 = 0
    ENDIF
    RETURN .T.
  endif
  if varread() = 'MCP4' 
    return .T.
  endif
  if varread() = 'MPE1'
    if MCP1 = 0 
       MPE1 = 100
       MPE2 = 0
       MPE3 = 0
       MPE4 = 0
       @08,53 say MPE1 pict '999' color &corver
       RETURN .F.
    endif   
    return .T.
  endif
  if varread() = 'MPE2' .and. mcp2 = 0
    MPE2 = 0  
    return .f.
  endif
  if varread() = 'MPE3' .and. mcp3 = 0
    MPE3 = 0
    return .f.
  endif
  if varread() = 'MPE4' .and. mcp4 = 0
    MPE4 = 0
    return .f.
  endif
  if varread() = 'MCFINANC'
  *  save screen to vtela
  endif
  if varread() = 'MCFIN2'
*    save screen to vtela
    if mpe2 = 0
      return .f.
    endif
    IF MCFIN2 = 0
       MCFIN2 = MCFINANC
    ENDIF   
  endif
  if varread() = 'MCFIN3'
*    save screen to vtela
    if mpe3 = 0
      return .f.
    endif
    IF MCFIN3 = 0
      MCFIN3 = MCFIN2
    ENDIF   
  endif
  if varread() = 'MCFIN4'
*    save screen to vtela
    if mpe4 = 0
      return .f.
    endif
    IF MCFIN4 = 0
      MCFIN4 = MCFIN3
    ENDIF   
  endif
  if varread() = 'MCPD'
    save screen to vtela
    =janela(12,15,22,40)
    =borda( 12,15,22,40)
    @13,17 say 'Modos aceitos:'
    @14,17 say 'VIS - Pagto a Vista'
    @15,17 say 'APR - Contra Apresent.'
    @16,17 say 'DDL - Dias da Data Liq'
    @17,17 say 'DFM - Dias fora o Mes'
    @18,17 say 'DFQ - Dias fora Quinz.'
    @19,17 say 'SDB - Sem D‚bito'
    @20,17 say 'PER - Permuta'
    @21,17 say 'ANT - Pagto Antecipado'
  endif
  if varread() = 'XMOEDA'
    restore screen from vtela
    @08, 21 SAY MCPD 
  endif
  if varread() = 'MDESCTO1' .or. varread() = 'MDESCTO2'
    =MENSAG('Informe os percentuais de Desconto ao Cliente, se concedidos')
    return .t.
  endif
  if varread() = 'MNBANCO'
    save screen to vtela
  endif
  if varread() = 'MAGENCIA'
    rest screen from vtela
  endif
  if varread() = 'MSITUACAO'
    save screen to vtela
    =janela(12,50,17,64)
    =borda( 12,50,17,64)
    @13,52 say '0-Pendente '
    @14,52 say '1-Pronto   '
    @15,52 say '2-Expedi‡„o'
    @16,52 say '3-Cancelado'
  endif
  =mensag(' ')
return .t.

*******************
function av060501va
*******************
  para vpar
  
  *if varread() = 'MVEND'
  *  if !empty(MIDREPR) .and. !seek(MIDREPR,'vend')
  *    =MENSAG(CORRIJA)
  *    =INKEY(0)
  *    return .f.
  *  endif
  *endif
  if varread() = 'MSITUACAO'
    if msituacao = 1
      @10,60 say '1-Pronto   ' color &corver
    else
      if msituacao = 2
         @10,60 say '2-Expedi‡„o' color &corver
      else
         if msituacao = 3
            @10,60 say '3-Cancelado' color &corver
         else
            @10,60 say '0-Pendente ' color &corver
         endif
      endif
    endif
    return .t.
  endif
  if lastkey() = 19 .or. lastkey() = 5
     if varread() = 'MCPD' .or. varread() = 'MSITUACAO'
       rest screen from vtela
     endif
     return .t.
  endif
  if varread() = 'MNBANCO'
    if !seek(str(mnbanco,3),'bancos')
      =MENSAG('BANCO INEXISTENTE  -  FAVOR  VERIFICAR')
      =INKEY(0)
      return .f.
    endif
  endif
  if varread() = 'MAGENCIA'
    if !seek(str(mnbanco,3)+str(magencia,4),'bancos')
      =MENSAG('AGENCIA INEXISTENTE NO BANCO ACIMA  -  FAVOR  VERIFICAR')
      =INKEY(0)
      return .f.
    endif
  endif
  if varread() = 'MGUERRA'
    if empty(mguerra)
      =MENSAG('Este campo deve existir - ENTER P/CONTINUAR')
      =INKEY(0)
      return .f.
    endif
    if !seek(alltrim(MGUERRA))
      =MENSAG('Nome de Guerra n„o cadastrado - ENTER')
      =INKEY(0)
      return .f.
    endif
  endif
  if varread() = 'MDEM'
    do FDATA with DTOC(MDEM)
    if TDATA # ' '
      =mensag('Data de emiss„o est  incorreta')
      =INKEY(0)
      return .f.
    endif
  endif
  if varread() = 'MTRA'
    if !seek(mtra,'trans')
      =MENSAG(CORRIJA)
      =INKEY(0)
      return .f.
    else
      @05,12 say trans->GUETRA 
    endif
  endif
  if varread() = 'MTIP'
    if empty(mtip)
      =MENSAG(CALPHA)
      =INKEY(0)
      return .f.
    endif
    if !seek(mtip,'tpnf')
      =MENSAG(CORRIJA)
      =INKEY(0)
      return .f.
    endif
  endif
  if varread() = 'MTABP'
     if !seek(mtabp,'TABFIN2')
        =mensag('Tabela de Pre‡os n„o cadastrada - ENTER')
        =INKEY(0)
        return .f.
     endif
  endif
  if varread() = 'MPARCELAS'
    if !seek(mtabp + str(mparcelas,1),'tabfin2')
      =mensag('Tabela de Pre‡os n„o cadastrada - ENTER')
      =INKEY(0)
      return .f.
    endif
  endif
  If varread() = 'MCP1'
     if !seek(mtabp + str(mparcelas,1) + str(mcp1,3),'tabfin2')
        =mensag('Tabela de Pre‡os n„o cadastrada - ENTER')
        =INKEY(0)
        return .f.
     endif
  endif  
  if varread() = 'MCP2'
    IF mcp2 = 0
      MCP3 = 0
      MCP4 = 0
      mpe2 = 0
      mpe3 = 0
      MPE4 = 0
      mcfin2 = 0
    ELSE  
      @08,57 say mpe2    pict '999'   color 'w+/bg'
      *@09,60 get mcfin2  pict '99.99' color 'w+/bg'
    ENDIF
  endif
  if varread() = 'MCP3' 
    IF mcp3 = 0
      MCP4 = 0
      mpe3 = 0
      MPE4 = 0
      mcfin3 = 0
    ELSE
      @08,61 say mpe3 pict '999' color 'w+/bg'
      *@09,66 get mcfin3  pict '99.99' color 'w+/bg'
    ENDIF    
  endif
  if varread() = 'MCP4' 
    IF mcp4 = 0
      mpe4 = 0
      mcfin4 = 0
    ELSE  
      @08,65 say mpe4 pict '999' color 'w+/bg'
     * @09,72 get mcfin4  pict '99.99' color 'w+/bg'
    ENDIF  
  endif
  if varread()='MPE1'
    if mcp2=0
      MPE2 = 0
      if MPE1 # 100
        =MENSAG('Percentuais para emiss„o das Duplicatas <> 100% - ENTER')
        =INKEY(0)
        return .f.
      endif
    else
      if mpe1 = 0
         =MENSAG('Percentual n„o pode ser 0 (zero) - ENTER')
         =INKEY(0)
         return .f.
      endif
    endif
  endif
  if varread()='MPE2'
    if mcp3=0
      MPE3 = 0   
      if MPE1+MPE2 # 100
         =MENSAG('Percentuais para emiss„o das Duplicatas <> 100% - ENTER')
         =INKEY(0)
         return .f.
      endif
    else
      if mpe2 = 0
         =MENSAG('Percentual n„o pode ser 0 (zero) - ENTER')
         =INKEY(0)
         return .f.
      endif
    endif
  endif
  if varread()='MPE3'
    if mcp4=0
      MPE4 = 0
      if MPE1+MPE2+MPE3 # 100
         =MENSAG('Percentuais para emiss„o das Duplicatas <> 100% - ENTER')
         =INKEY(0)
         return .f.
      endif
    else
      if mpe3 = 0
         =MENSAG('Percentual n„o pode ser 0 (zero) - ENTER')
         =INKEY(0)
         return .f.
      endif
    endif
  endif
  if varread()='MPE4'
    if mpe4 = 0
      =MENSAG('Percentual n„o pode ser 0 (zero) - ENTER')
      =INKEY(0)
      return .f.
    endif
    if MPE1+MPE2+MPE3+MPE4 # 100
      =MENSAG('Percentuais para emiss„o das Duplicatas <> 100% - ENTER')
      =INKEY(0)
      return .f.
    endif
  endif
  if varread() = 'MCFINANC' .or. varread()='MCFIN2' .or. varread()='MCFIN3' .or. varread()='MCFIN4'
    if vpar < 0
      =MENSAG('Taxa financeira est  incorreta - ENTER')
      =INKEY(0)
      return .f.
    endif
  endif
  if varread() = 'MCPD'
    if mcpd = 'VIS' .or. mcpd = 'APR' .or. mcpd = 'DDL' .or. mcpd = 'DFM' ;
      .or. mcpd = 'DFQ' .OR. MCPD = 'SDB' .OR. MCPD = 'ANT'
    else
      =mensag('Dados Est„o Incorretos')
      =INKEY(0)
      return .f.
    endif
  endif
  if varread() = 'XMOEDA'
    if xmoeda = 0
      =MENSAG(CNUMER)
      =INKEY(0)
      return .f.
    endif
    if !seek(str(xmoeda,3),'nommoeda')
      =MENSAG('Moeda n„o cadastrada   -   ENTER')
      =INKEY(0)
      return .f.
    endif
  endif
return .t.

******************
FUNCTION VALIDREPR
******************
  PRIVATE Valido, nDbf, cVend, nIdRepr
  
  cVend   = SUBSTR(MVEND,1,4)
  nIdRepr = VAL(SUBSTR(MVEND,6,2))
  nDbf = SELECT()
  * Verifica se o Representante existe
  Valido = ChkRepr(MVEND, .T.)
  IF Valido 
    * Verifica se e um representante do Cliente
    IF (CLIEN->IDREPR <> 0) AND (nIdRepr <> CLIEN->IDREPR) 
      IF ! SEEK( STR(nIdRepr,5)+STR(CLIEN->IDREGIAO,5), 'ATREGIAO') 
        =MENSAG('Este representante nÆo ‚ v lido para a RegiÆo')
        =INKEY(0)
      ENDIF
    ENDIF
  ENDIF    
  SELECT(nDbf)

RETURN Valido