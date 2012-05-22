* --------------------------------------------------------------------
*   ADMVFUNC - FUNCOES DO SISTEMA ADMINISTRACAO DE VENDAS
* --------------------------------------------------------------------
*================
PROCEDURE pegresp
*================
PARAMETER x_tudo

  @ 23,01 SAY SPAC(78)
  x_aqui = (80-(LEN(mens)+(LEN(x_tudo)*2)+1))/2
  x_lax = x_aqui+LEN(mens)
  SET MESS TO
  x_N = 1
  DO WHIL x_N <= LEN(x_tudo)
    x_sy = LTRI(STR(x_N))
    op&x_sy = SUBS(x_tudo,x_N,1)
    x_N = x_N + 1
  ENDD
  x_N = 1
  @ 23,x_aqui SAY mens+[ (]+REPL([ /],LEN(x_tudo)-1)+[ )]
  DO WHIL x_N <= LEN(x_tudo)
    x_sy = LTRI(STR(x_N))
    @ 23,x_lax+(x_N*2) PROM op&x_sy
    x_N = x_N + 1
  ENDD
  MENU TO x_peg
  if x_peg = 0
    MC = " "
  else
    MC = SUBS(x_tudo,x_peg,1)
  endif
RETU

*==================
PROCEDURE OBTEMRESP
*=================
PARAMETER cTexto, cOpcao
  private cRet, nOpcao

  @ 23,01 SAY SPAC(78)
  x_aqui = (80-(LEN(cTexto)+(LEN(cOpcao)*2)+1))/2
  x_lax = x_aqui+LEN(cTexto)
  SET MESS TO
  x_N = 1
  DO WHIL x_N <= LEN(cOpcao)
    x_sy = LTRI(STR(x_N))
    op&x_sy = SUBS(cOpcao,x_N,1)
    x_N = x_N + 1
  ENDD
  x_N = 1
  @ 23,x_aqui SAY cTexto+[ (]+REPL([ /],LEN(cOpcao)-1)+[ )]
  DO WHIL x_N <= LEN(cOpcao)
    x_sy = LTRI(STR(x_N))
    @ 23,x_lax+(x_N*2) PROM op&x_sy
    x_N = x_N + 1
  ENDD
  MENU TO nOpcao
  if nOpcao = 0
    cRet = ''
  else
    cRet = SUBS(cOpcao,nOpcao,1)
  endif
  @ 23,01 SAY SPAC(78)

RETU cRet

*==================
PROCEDURE pegresp49
*==================
PARA x_tudo
@ 48,01 SAY SPAC(78)
x_aqui = (80-(LEN(mens)+(LEN(x_tudo)*2)+1))/2
x_lax = x_aqui+LEN(mens)
SET MESS TO
x_N = 1
DO WHIL x_N <= LEN(x_tudo)
   x_sy = LTRI(STR(x_N))
   op&x_sy = SUBS(x_tudo,x_N,1)
   x_N = x_N + 1
ENDD
x_N = 1
@ 48,x_aqui SAY mens+[ (]+REPL([ /],LEN(x_tudo)-1)+[ )]
DO WHIL x_N <= LEN(x_tudo)
   x_sy = LTRI(STR(x_N))
   @ 48,x_lax+(x_N*2) PROM op&x_sy
   x_N = x_N + 1
ENDD
MENU TO x_peg
MC = SUBS(x_tudo,x_peg,1)
RETU
* --------------------------------------------------------------
PROC pegopcao
PARA x_tudo
@ 23,01 SAY SPAC(78)
x_aqui = (80-(LEN(mens)+(LEN(x_tudo)*2)+1))/2
x_lax = x_aqui+LEN(mens)
SET MESS TO
x_N = 1
DO WHIL x_N <= LEN(x_tudo)
   x_sy = LTRI(STR(x_N))
   op&x_sy = SUBS(x_tudo,x_N,1)
   x_N = x_N + 1
ENDD
x_N = 1
@ 23,x_aqui SAY mens+[ (]+REPL([ /],LEN(x_tudo)-1)+[ )]
DO WHIL x_N <= LEN(x_tudo)
   x_sy = LTRI(STR(x_N))
   @ 23,x_lax+(x_N*2) PROM op&x_sy
   x_N = x_N + 1
ENDD
MENU TO x_peg
OPCAO = SUBS(x_tudo,x_peg,1)
RETU
* -------------------------------------------------------------
PROC mensmeio
PARA x_meio
LINTELA = PAR4_P1+'/'+PAR1_P2
@ 23,01 SAY SPAC(78) color &lintela
@ 23,INT((80-LEN(mens))/2) SAY mens color &LINTELA
@ 23,39-(LEN(mens)/2) + AT(x_meio,mens) SAY x_meio color r+*/b
RETU
* ------------------------------------------------------------
PROC menspisc
PARAMETERS MENS
@ 23,01 SAY SPAC(78) color n/n
@ 23,INT((80-LEN(mens))/2) SAY mens color r+*/n
RETU
* ------------------------------------------------------------
PROC mensrep
PARAMETERS MENS
@ 23,01 SAY SPAC(78)
@ 23,INT((80-LEN(mens))/2) SAY mens color w+*/n

* do som
* DO Base

I = INKE(2)
RETU
* --------------------------------------------------------------
* FUNCAO DE BROWSE DE ARQUIVOS
*================
PROCEDURE BROWARQ
*================
    PRIVATE CAMPO, XAREA
    CAMPO  = VARREAD()
    XAREA  = SELECT()
    ON KEY LABEL ENTER DEAC WINDOW TELA
    ON KEY LABEL ESCAPE DEAC WINDOW TELA

    DO CASE
      CASE CAMPO = "MCPD" 
        nDbf = SELECT()
        SELECT TIPOPGTO
        SEEK(MCPD)
        ? MOSTRA(10,10,21,40)
        BROWSE FIELDS TIPO:H='Tipo',DESCRICAO:R:H='Descri‡„o' IN WINDOW TELA
        MCPD = TIPO 
        SELECT(nDbf)
    
      CASE CAMPO = "MSITUACAO" 
        nDbf = SELECT()
        SELECT SITPED
        SEEK(MSITUACAO)
        ? MOSTRA(10,10,21,40)
        BROWSE FIELDS SITUACAO:H='Situacao',DESCRICAO:R:H='Descri‡„o' IN WINDOW TELA
        MSITUACAO = SITUACAO
        SELECT(nDbf)

      CASE CAMPO = "MTIPOCART"
        nDbf = SELECT()
        SELECT TPCART
        IF ! EMPTY(MTIPOCART)
          SEEK(MTIPOCART)
        ENDIF
        ? MOSTRA(10,47,21,78)
        BROWSE FIELDS TIPO:H='Sigla',DESCRICAO:R:H='Descri‡„o' IN WINDOW TELA
        MTIPOCART = TIPO
        SELECT(nDbf)
      CASE CAMPO = "MSTATUS"
        nDbf = SELECT()
        SELECT STATUS
        IF MSTATUS <> SPACE(02)
          SEEK(MSTATUS)
        ENDIF
        ? MOSTRA(03,47,21,78)
        BROWSE FIELDS STATUS:H='Status',DSTATUS:R:H='Descri‡„o' IN WINDOW TELA
        MSTATUS = STATUS
        SELECT( nDbf )

      CASE CAMPO = "MNNF"
        sele cadnf
        seek str(mnnf,6)
        =mostra(10,02,21,78)
        browse fields nnf:H='N§ Nf.',demi:H='Dt. Emiss„o', ref:H='Referˆncia',des:H='Descri‡„o' IN WINDOW TELA
        MNNF = NNF

      CASE CAMPO = "MDESCSEM"
        sele TABSEM
        ? MOSTRA(10,47,21,78)
        BROWSE FIELDS TABSEM:R:H='Desc.Sem.',DIAS_DE:R:H='De',DIAS_ATE:R:H='Ate',PERDIAS:R:H='Percent' IN WINDOW TELA
        MTABSEM = TABSEM
        MDIAS_DE = DIAS_DE
        MDESCSEM = PERDIAS

      CASE CAMPO = "MCFINANC"
        sele TABFIN
        ? MOSTRA(10,47,21,78)
        BROWSE FIELDS TABFIN:R:H='C.Financ.',DIAS_DE:R:H='De',DIAS_ATE:R:H='Ate',PERFIN:R:H='Percent' IN WINDOW TELA
        MTABFIN = TABFIN
        MDIAS_DE = DIAS_DE
        MDIAS_ATE = DIAS_ATE
        MCFINANC = PERFIN

      CASE CAMPO = "MCFIN2"
        sele TABFIN2
        ? MOSTRA(10,47,21,78)
        BROWSE FIELDS TABFIN:R:H='C.Financ.',DIAS_DE:R:H='De',DIAS_ATE:R:H='Ate',PERFIN:R:H='Percent' IN WINDOW TELA
        MTABFIN = TABFIN
        MDIAS_DE = DIAS_DE
        MDIAS_ATE = DIAS_ATE
        MCFIN2 = PERFIN

      CASE CAMPO = "MTABFIN"
        IF MTABFIN <> SPACE(02)
          SEEK(MTABFIN)
        ENDIF
        ? MOSTRA(10,24,21,78)
        BROWSE FIELDS TABFIN:H='Tabela',PARCELAS:H='P', ;
            DIAS_DE:H='Dias',VENCTOS:H='Vencimentos',PERFIN:R:H='Acr‚sc' ;
             NOEDIT IN WINDOW TELA
        MTABFIN   = TABFIN
        MCFINANC  = PERFIN
        MPERFIN   = PERFIN
        MDIAS_DE  = DIAS_DE
        MPARCELAS = PARCELAS
        MTABP     = TABFIN
        MCP1      = DIAS_DE

      CASE CAMPO = "MTABP"
        sele TABFIN2
        IF MTABP <> SPACE(02)
          SEEK(MTABP)
        ENDIF
        nOrdem = ORDER()
        SET ORDER TO X_DATA
        GO TOP
        ? MOSTRA(10,24,21,78)
        BROWSE FIELDS TABFIN:H='Tabela',PARCELAS:H='P', ;
            DIAS_DE:H='Dias',VENCTOS:H='Vencimentos',PERFIN:R:H='Acr‚sc' ;
             NOEDIT IN WINDOW TELA
        MTABFIN   = TABFIN
        MCFINANC  = PERFIN
        MPERFIN   = PERFIN
        MDIAS_DE  = DIAS_DE
        MPARCELAS = PARCELAS
        MTABP     = TABFIN
        MCP1      = DIAS_DE
        SET ORDER TO nOrdem
        
      CASE CAMPO = "MTABPARC"
        IF MTABPARC <> SPACE(02)
          SEEK(MTABPARC)
        ENDIF
        ? MOSTRA(04,50,21,78)
        BROWSE FIELDS TABPARC:H='Tabela',PRAZMED:H='Prazo', ;
               DESPARC:H='% Descto' NOEDIT IN WINDOW TELA
        MTABPARC = TABPARC
        MPRAZMED = PRAZMED
        MDESPARC = DESPARC

      CASE CAMPO = "MTABUNIC"
        IF MTABUNIC <> SPACE(02)
          SEEK(MTABUNIC)
        ENDIF
        ? MOSTRA(04,50,21,78)
        BROWSE FIELDS TABUNIC:H='Tabela',QTDDIAS:H='Prazo', ;
               DESUNIC:H='% Descto' NOEDIT IN WINDOW TELA
        MTABUNIC = TABUNIC
        MQTDDIAS = QTDDIAS
        MDESUNIC = DESUNIC

      CASE CAMPO = "MCONCEITO" .OR. CAMPO = "SCONCEITO"
        cConceito = &CAMPO
        IF cCONCEITO <> SPACE(01) 
          SEEK(cCONCEITO)
        ENDIF

        ? MOSTRA(03,42,21,78)
        BROWSE FIELDS CONCEITO:R:H='C¢d',DCONCEITO:R:H='Descri‡„o' IN WINDOW TELA
        &CAMPO = CONCEITO

      CASE CAMPO = "MREGIAO"
        sele regiao
        nIndice = ORDER()
        SET ORDER TO X_REGIAO
        IF MREGIAO <> EmptyRegiao
          MCHAVE = SUBSTR(MREGIAO,1,4) + STR(VAL(SUBSTR(MREGIAO,6,2)),2)
          SEEK MCHAVE
        ENDIF
        =MOSTRA(13,07,21,78)
        BROWSE FIELDS REGIAO:H='C¢digo',IDREGIAO:H='Seq.',DREGIAO:H='Descri‡„o';
               NOEDIT IN WINDOW TELA
        MREGIAO   = FmtRegiao(REGIAO,IDREGIAO)
        MIDREGIAO = IDREGIAO
        SET ORDER TO nIndice

      CASE CAMPO = "MATIVIDADE"
        IF MATIVIDADE <> SPACE(02)
          SEEK(MATIVIDADE)
        ENDIF
        ? MOSTRA(03,47,21,78)
        BROWSE FIELDS ATIVIDADE:H='Ativ',DATIVIDADE:R:H='Descri‡„o' IN WINDOW TELA
        MATIVIDADE = ATIVIDADE

      CASE CAMPO = "MVEND"
        IF lVendaDir 
          =MENSAG('Cliente esta definido como Venda Direta')
          =INKEY(0)
          =MENSAG('')
        ELSE  
          nDbf = SELECT()
          MNUM = CLIEN->IDREGIAO
          ? MOSTRA(03,05,21,78)
          SELECT;
            V.CVR, V.RAZ, AT.IDREPRES, AT.IDREGIAO, AT.DTINICIO, AT.DTTERMINO,;
            R.ESTADO;
          FROM;
            VEND V, ATREGIAO AT, REGIAO R;
           WHERE;
            AT.IDREGIAO = MNUM AND AT.IDREPRES = V.IDREPR AND;
            AT.IDREGIAO = R.IDREGIAO;
          ORDER BY V.CVR, V.IDREPR;
          INTO CURSOR BROWSE
        
          BROWSE FIELDS BROWSE.CVR:H='REPR.',;
                        BROWSE.IDREPRES:H='Seq.',;
                        BROWSE.ESTADO:H='UF',;                      
                        BROWSE.RAZ:R:H='Razao Social',;
                        BROWSE.DTINICIO:H='Inicio',;
                        BROWSE.DTTERMINO:H='Termino';
                        IN WINDOW TELA
          IF LASTKEY() <> 27              
            MCVR      = BROWSE.CVR
            MVEND     = FMTREPR(BROWSE.CVR, BROWSE.IDREPRES)
            MIDREPR   = BROWSE.IDREPRES
            MIDREGIAO = BROWSE.IDREGIAO
          ENDIF  
          USE
          SELECT(nDbf)
        ENDIF   

      CASE CAMPO = "MSIGLA"
        sele siglas
        IF MSIGLA <> SPACE(03)
          SEEK(MSIGLA)
        ENDIF
        ? MOSTRA(03,47,21,78)
        BROWSE FIELDS SIGLA:H='Sigla',NOME:H='Nome do Respons.' NOEDIT IN WINDOW TELA
        MSIGLA = SIGLA

      CASE CAMPO = "MCARGO"
        IF MCARGO <> SPACE(02)
          SEEK(MCARGO)
        ENDIF
        ? MOSTRA(03,47,21,78)
        BROWSE FIELDS CARGO:H='Cargo',DCARGO:H='Cargo do Contato' NOEDIT IN WINDOW TELA
        MCARGO = CARGO

      CASE CAMPO = "MCONDCPAG"
        ? MOSTRA(10,01,21,78)
        BROWSE FIELDS CONDCPAG:R:H='C¢d',DESCCPA1:R:H='Descri‡„o', ;
              TIPOCPAG:R:H='Tip',VENC01:R:H='V 1',PERC01:R:H=' % 1', ;
              VENC02:R:H='V 2',PERC02:R:H=' % 2',VENC03:R:H='V 3',PERC03:R:H=' % 3' ;
             IN WINDOW TELA
        MCONDCPAG = CONDCPAG

      CASE CAMPO = "M_CARGO1"
        IF M_CARGO1 <> SPACE(02)
          SEEK(M_CARGO1)
        ENDIF
        ? MOSTRA(03,47,21,78)
        BROWSE FIELDS CARGO:H='Cargo',DCARGO:H='Cargo do Contato' NOEDIT IN WINDOW TELA
        M_CARGO1 = CARGO

      CASE CAMPO = "M_CARGO2"
        IF M_CARGO2 <> SPACE(02)
          SEEK(M_CARGO2)
        ENDIF
        ? MOSTRA(03,47,21,78)
        BROWSE FIELDS DCARGO:H='Cargo do Contato' NOEDIT IN WINDOW TELA
        M_CARGO2 = CARGO

      CASE CAMPO = "MLPR"
        IF MLPR <> SPACE(1)
          SEEK(MLPR)
        ENDIF
        ? MOSTRA(03,40,21,78)
        BROWSE FIELDS LPR:H='C¢d',DLPR:H='Linha de Produto' NOEDIT IN WINDOW TELA
        MLPR = LPR
      CASE CAMPO = "MCEST"
        sele ESTADOS
        IF MCEST <> SPACE(02)
          SEEK MCEST
        ENDIF
        ? MOSTRA(03,51,21,78)
        BROWSE FIELDS CEST:H='UF',DEST:H='Descri‡„o' NOEDIT IN WINDOW TELA
        MCEST = CEST

      CASE CAMPO = "MTRA" .OR. CAMPO = "MGUETRA"
        sele TRANS
        MCHAVE = ''
        IF CAMPO = "MTRA" 
          IF MTRA <> SPACE(03)
            MCHAVE = MTRA
          ENDIF
        ELSE
          IF MGUETRA <> SPACE(15)
            MCHAVE = MGUETRA  
          ENDIF  
        ENDIF
        SEEK MCHAVE  
        ? MOSTRA(03,32,21,78)
        BROWSE FIELDS TRA:H='C¢d',;
        		      GUETRA:H='N.Guerra',;
        			  RAZ:H='Raz„o Social',;
        			  COLETA=IIF(TRANS->COLETA,'S','N'):H='Coleta';
        			  NOEDIT IN WINDOW TELA
        IF CAMPO = "MTRA"			  
          MTRA = TRA
        ELSE  
          MGUETRA = GUETRA
        ENDIF   
 *       USE
        
     CASE CAMPO = "TTRA_OPC"
        IF TTRA_OPC <> SPACE(03)
          SEEK(TTRA_OPC)
        ENDIF
        ? MOSTRA(03,32,21,78)
        BROWSE FIELDS TRA:R:H='C¢d',RAZ:R:H='Raz„o Social' IN WINDOW TELA
        TTRA_OPC = TRA

     CASE CAMPO = "MCVR" 
        sele vend
        cOrdem = ORDER()
        SET ORDER TO X_VEND
        ? MOSTRA(03,50,21,78)
        BROWSE FIELDS CVR:R:H='C¢d',IDREPR:R:H='Seq.',RAZ:R:H='Raz„o Social/Nome' IN WINDOW TELA
        MVEN    = CVR
        MIDREPR = IDREPR
        MCVR    = FmtRepr(CVR,MIDREPR)
        SET ORDER TO cOrdem

     CASE CAMPO = "MVENGUE" 
        sele vend
        IF MVENGUE <> SPACE(03)
          SEEK(MVENGUE)
        ENDIF

        ? MOSTRA(03,50,21,78)
        BROWSE FIELDS CVR:R:H='C¢d',RAZ:R:H='Raz„o Social/Nome' IN WINDOW TELA
        MVENGUE = VENGUE

     CASE CAMPO = "MREF"   
       nDbf = SELECT()
       SELE PROD
       IF MREF <> SPACE(04)
         SEEK(MREF)
       ENDIF
       ? MOSTRA(06,20,21,78)
       BROWSE FIELDS REF:R:H='C¢digo',DES:R:H='Descri‡„o' IN WINDOW TELA
       MREF = REF
       SELECT(nDbf)

    CASE CAMPO = "MCPR"
      IF MCPR <> SPACE(01)
        SEEK(MCPR)
      ENDIF

      ? MOSTRA(13,60,21,78)
      BROWSE FIELDS CPR:R:H='C¢d',PRE:R:H='   Pre‡o' IN WINDOW TELA
      MCPR = CPR

    CASE CAMPO = "MNBANCO"
      varea = select()
      sele BANCOS
      IF MNBANCO > 0
        SEEK(STR(MNBANCO,3))
      ENDIF
      ? MOSTRA(06,50,21,78)
      BROWSE FIELDS NBANCO:R:H='Bco',AGENCIA:R:H='Agˆn',DBANCO:R:H= ;
            'R.Social' IN WINDOW TELA
      MNBANCO = NBANCO
      MAGENCIA = AGENCIA
      select(varea)
    CASE CAMPO = "MMOEDA"
      IF MMOEDA > 0
        SEEK(STR(MMOEDA,3))
      ENDIF
      ? MOSTRA(06,10,21,78)
      BROWSE FIELDS MOEDA:H='C¢d',NOMMOEDA->DMOEDA:H='Descri‡„o', ;
             CADMOEDA->DATA:H=' Data',CADMOEDA->CDOLAR:H='P.Dolar', ;
             CADMOEDA->CCZ:H='P.Cruzeiro' NOEDIT IN WINDOW TELA
      MMOEDA = MOEDA
      MDATA  = CADMOEDA->DATA

    CASE CAMPO = "XMOEDA"
      sele NOMMOEDA
      IF XMOEDA > 0
        SEEK(STR(XMOEDA,3))
      ENDIF
      ? MOSTRA(08,30,21,78)
      BROWSE FIELDS MOEDA:H='C¢d',AMOEDA:H='Abrev',DMOEDA:H='Descri‡„o' NOEDIT IN WINDOW TELA
      XMOEDA = MOEDA

      CASE CAMPO = "MCEST"  .OR. CAMPO = "ESTENT"
        ? MOSTRA(03,51,21,78)
        BROWSE FIELDS CEST:H='UF',DEST:H='Descri‡„o' NOEDIT IN WINDOW TELA
        MCEST = CEST

    CASE CAMPO = "MCGC"
      IF MCGC <> SPACE(03)
        SEEK(MCGC)
      ENDIF
      ? MOSTRA(12,01,21,78)
      BROWSE FIELDS GUE:R:H='N. Guerra',CGC:R:H='CGC/Loja', ;
                    CID:R:H='Cidade',END:R:H='Endere‡o' IN WINDOW TELA
      MCGC  = SUBS(CGC,1,16)
      MLOJA = SUBS(CGC,18,3)
      KEYCGC = MCGC+'/'+MLOJA
      MFIJU = FIJU
      MGUE = GUE
      MVEN = VEN
      MTRA = TRA
      MTRA_OPC = TRA_OPC

    CASE CAMPO = "MMGUE"
      IF MMGUE <> SPACE(10)
        SEEK RTRIM(MMGUE)
      ENDIF
        ? MOSTRA(12,01,21,78)
        BROWSE FIELDS GUE:R:H='N. Guerra',CGC:R:H='CGC/Loja', ;
            CID:R:H='Cidade',END:R:H='Endere‡o' IN WINDOW TELA
        MCGC  = SUBS(CGC,1,16)
        MMGUE = GUE
        @ 04,20 SAY MMGUE COLOR &CORVER

    CASE CAMPO = "MGUE"  .OR. CAMPO = "MGUERRA"
        SELE CLIEN
        SEEK( RTRIM( &CAMPO ) )   
        *IF VARREAD() = "MGUERRA" .AND. MGUERRA <> SPACE(10)
        *  SEEK RTRIM(MGUERRA)
        *ELSE
        *  IF MGUE <> SPACE(10)
        *    SEEK RTRIM(MGUE)
        *  ENDIF
        *ENDIF
        ? MOSTRA(12,01,21,78)
        BROWSE FIELDS GUE:R:H='N. Guerra',CGC:R:H='CGC/Loja', ;
                    CID:R:H='Cidade',END:R:H='Endere‡o' IN WINDOW TELA
        MCGC  = SUBS(CGC,1,16)
        MLOJA = SUBS(CGC,18,3)
        KEYCGC = MCGC+'/'+MLOJA
        MFIJU = FIJU
        MGUE = GUE
        MVEN = VEN
        MCVR = VEN
        MTRA = TRA
        MTRA_OPC = TRA_OPC
        mguerra = GUE
        MTRA_OPC = TRA_OPC
        MDESCTO1 = DESCTO1
        MDESCTO2 = DESCTO2

    CASE CAMPO = "MFON1" .OR. CAMPO = "MFON2" .OR. CAMPO = "MNUM_CELU"
        SELE CLIEN
        cOrder = ORDER()
        DO CASE
          CASE CAMPO = "MFON1"
            SET ORDER TO X_FON1
            MATUAL = "FON1"
          CASE CAMPO = "MFON2"
            SET ORDER TO X_FON2
            MATUAL = "FON2"
          CASE CAMPO = "MNUM_CELU"
            SET ORDER TO X_CELU
            MATUAL = "NUM_CELU"
        ENDCASE        
        SEEK( ALLTRIM( &CAMPO ) )   
        =MOSTRA(12,01,21,78)
         BROWSE FIELDS FON1:R:H='Fone 1',;
                       FON2:R:H='Fone 2',;
                       NUM_CELU:R:H='Celular',;
                       GUE:R:H='N. Guerra',;
                       CGC:R:H='CGC/Loja',;
                       CID:R:H='Cidade',;
                       END:R:H='Endere‡o';
                       IN WINDOW TELA
        IF LASTKEY() = 13            
          &CAMPO = &MATUAL
        ENDIF  
        SET ORDER TO cOrder
        
    CASE CAMPO = "MTIP"
      sele TPNF 
      IF CAMPO = "MTIP"
        SEEK MTIP
      ENDIF
      ? MOSTRA(10,33,22,78)
      BROWSE FIELDS TIPO:R:H='Tipo',NOPE:R:H='N.Op',DESN:R:H='Descri‡„o', ;
              IPIT:R:H='IPI',ICMT:R:H='ICM',ISST:R:H='ISS' IN WINDOW TELA
      MTIPO = TIPO
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

    CASE CAMPO = "MPED"

      sele pedid
      IF MPED > 0
        SEEK(STR(MPED,6))
      ENDIF
      ? MOSTRA(04,01,23,78)
      BROWSE FIELDS PED:H='Pedido',clien->GUE:H='Nome de Guerra',PEDID->ITE:H='It', ;
                    PEDID->REF:H='Ref.',PEDID->QTD:H=' Quantid',PEDID->SAL:H='   Saldo', ;
                    PEDID->PRE:H='   Pre‡o Unit rio' IN WINDOW TELA NOEDIT
* FOR PEDID->SAL > 0
      MITE = PEDID->ITE
      MREF = PEDID->REF
      MCGC = CGC
      MPED = PED
*      MGUE = GUE
      MPED = PED

    CASE CAMPO = "MITE"
      IF MITE > 0
        SEEK(STR(MITE,2))
      ENDIF

      ? MOSTRA(03,01,10,79)
      BROWSE FIELDS ITE:R:H='It',REF:R:H='Referˆncia',IPI:R:H='%IPI', ;
                   QTD:R:H='Quantidade',DIT:R:H='%Ite',BON:R:H='%Bon', ;
                   PRE:R:H='Pre‡o Unit' FOR PED = MPED IN WINDOW TELA
      MITE = ITE
      MREF = REF
      MPED = PED
*      DO ESCONDE
      @ LIN,02 SAY MITE PICT '99' color &corver
      @ LIN,05 SAY MREF color &corver
      @ LIN,10 say tabref pict '99/99' color &corver
      @ lin,16 say prebru pict '99,999,999.99' color &corver
      @ lin,30 say descgru pict '99.99' color &corver
      @ LIN,36 SAY QTD PICT '99999999.99' color &corver
      @ LIN,45 SAY DIT PICT '99.99'  color &corver
      @ LIN,51 SAY BON PICT '99.99'  color &corver
      @ LIN,67 SAY PRE PICT '99999999.99'  color &corver
      @ LIN+1,05 SAY DESCRICAO    color &corver
      @ lin,61 say ipi pict '99.9' color &corver

     CASE CAMPO = "MUNIDADE"
      IF MUNIDADE <> SPACE(02)
        SEEK(MUNIDADE)
      ENDIF

       ? MOSTRA(03,50,21,78)
       BROWSE FIELDS UNIDADE:R:H='Unid',DUNIDADE:R:H='Descri‡„o' IN WINDOW TELA
       MUNIDADE = UNIDADE

     CASE CAMPO = "MEMBALAGEM"
       IF MEMBALAGEM <> SPACE(02)
         SEEK(MEMBALAGEM)
       ENDIF
       ? MOSTRA(03,50,21,78)
       BROWSE FIELDS EMBALAGEM:R:H='Tipo',DEMBALAGEM:R:H='Descri‡„o' IN WINDOW TELA
       MEMBALAGEM = EMBALAGEM

     CASE CAMPO = "MCLASFISC"
       IF MCLASFISC <> SPACE(01)
         SEEK(MCLASFISC)
       ENDIF
       ? MOSTRA(03,50,21,78)
       BROWSE FIELDS CLASFISC:R:H='C¢digo',DCLASFISC:R:H='Class. do IPI' IN WINDOW TELA
       MCLASFISC = CLASFISC

     CASE CAMPO = "MGRUPO"
       sele GRUPOS
       IF MGRUPO <> SPACE(02)
         SEEK(MGRUPO)
       ENDIF
       ? MOSTRA(03,45,21,78)
       BROWSE FIELDS GRUPO:H='C¢d',DGRUPO:H='Descri‡„o',COMISSAO:H= ;
             '% Com' NOEDIT IN WINDOW TELA
       MGRUPO = GRUPO

     CASE CAMPO = "MVEICULO"
       IF MVEICULO <> SPACE(08)
         SEEK(RTRIM(MVEICULO))
       ENDIF
       ? MOSTRA(03,45,21,78)
       BROWSE FIELDS VEICULO:H='Chapas',DVEICULO:H='Descri‡„o' NOEDIT IN WINDOW TELA
       MVEICULO = VEICULO

    CASE CAMPO = "MTABREF"
      IF MTABREF <> SPACE(02)
        SEEK(MTABREF)
      ENDIF
      ? MOSTRA(04,50,21,78)
      BROWSE FIELDS TABREF:R:H='Tab',REF:R:H='Ref',PRECO:R:H='Pre‡o' IN WINDOW TELA
      MTABREF = TABREF
      MPRECO = PRECO
      MREF   = REF
    CASE CAMPO = "MTABSEM"
      IF MTABSEM <> SPACE(02)
        SEEK(MTABSEM)
      ENDIF
      ? MOSTRA(04,48,21,78)
      BROWSE FIELDS TABSEM:R:H='Desc.Sem.',DIAS_DE:R:H='De',DIAS_ATE:R:H='Ate',PERDIAS:R:H='Percent' IN WINDOW TELA
      MTABSEM = TABSEM
      MDIAS_DE = DIAS_DE
      MDIAS_ATE= DIAS_ATE
      MPERDIAS = PERDIAS

    CASE CAMPO = "MTABFIN"
      IF MTABFIN <> SPACE(02)
        SEEK(MTABFIN)
      ENDIF
      ? MOSTRA(04,48,21,78)
      BROWSE FIELDS TABFIN:R:H='C.Financ.',DIAS_DE:R:H='De',DIAS_ATE:R:H='Ate',PERFIN:R:H='Percent' IN WINDOW TELA
      MTABFIN = TABFIN
      MDIAS_DE = DIAS_DE
      MDIAS_ATE = DIAS_ATE
      MPERFIN = PERFIN

    ENDCASE

    ON KEY LABEL ENTER
    ON KEY LABEL ESCAPE

* DO ESCONDE
  SELE (XAREA)
RETURN .T.

* ---------------------------------------------------------------
FUNCTION MOSTRA
PARAMETERS B1,B2,B3,B4
  DEFINE WINDOW TELA FROM B1,B2 TO B3,B4;
         COLOR +W/N,+N/W,GR+/N,+GR/N,+GR/N ;
         FLOAT;
         TITLE ' <ENTER> para selecionar '
  ACTIVATE WINDOW TELA
RETURN
* ---------------------------------------------------------------
FUNCTION TESTIMP2
* Define a impressao em tela ou na impressora
mens = 'Relat¢rio selecionado est  correto para impress„o ?'
do pegresp with [SN]
cp = mc
if cp = 'S'
   if file("GRAREL80.TXT")
      erase GRAREL80.TXT
   endif
   if file("GRAREL10.TXT")
      erase GRAREL10.TXT
   endif
   stor space(01) to cp, srel, defrel
   stor space(08) to trel
   mens = 'Relat¢rio em Tela ( T ) ou Impresso ( I ) ?'
   do pegresp with [TI]
   defrel = mc
   if defrel = 'I'
      mens = 'Impressora est  ligada e com Papel dispon¡vel ?'
      do pegresp with [SN]
      cp = mc
*      if cp = 'N' .or. empty(cp)
*         return
*      endif
   endif
endif
return
* --------------------------------------------------------------------------
PROCEDURE COMPIMPR      && COMPACTA CARACTERES POR POLEGADA

  _PLENGHT = 1
  _PLINENO = 0
  _PAGENO  = 1
  _PCOPIES = 1
  _PBPAGE  = 1
  _PEPAGE  = 1
  _PSCODE  = ""
  _PECODE  = ""
  _PEJECT  = "NONE"

*  SET PRINT ON
  IF TIPOIMPR = 'R'
    ??? CHR(30) + '4'
  ELSE
    IF TIPOIMPR = 'E'
      ??? CHR(15)
    ELSE
      ??? CHR(27)+"&l7.27C"+chr(27)+"(s0p16.66H"
    ENDIF
  ENDIF
*  SET PRINT OFF
  RETURN
* -------------------------------------------------------------------------
PROCEDURE DEFSAIDA
* DEFINE SE SAIDA EM 80 OU 132 COLUNAS
parameters vrotina
=INICPAG()  &&-- Reseta _PLINENO/_PCOLNO 
if defrel = 'I'
   if parameters() # 1
      do reseimpr
   endif
   if defrel = 'I' .and. tamrel = 2
      mens = 'Papel de 80 (1) ou de 132 (2) colunas? '
      do pegresp with [12]
      ci = mc
      if ci = '1'
         do compimpr
      endif
   endif
   _PLENGHT = 1
   _PLINENO = 0
   _PAGENO  = 1
   _PCOPIES = 1
   _PBPAGE  = 1
   _PEPAGE  = 1
   _PSCODE  = ""
   _PECODE  = ""
   _PEJECT  = "NONE"
   set devi to print
else
   if tamrel = 1
      set devi to file GRAREL80.TXT
   else
      set devi to file GRAREL10.TXT
   endif
endif
return
* -------------------------------------------------------------------------
PROCEDURE IMPSAIDA && - DEFINE SE SAIDA NA IMPRESSORA OU NA TELA
private xx
if type('MCOPIAS') # 'N'
   mcopias = 1
endif
  SET DEVI TO SCRE
  IF DEFREL = 'T'
     SAVE SCREEN TO TELA
     CLEAR
     SET DISPLAY TO VGA50
     CLOSE DATABASES
     IF TAMREL = 1
        use arqrel80
        if flock()
           zap
           unlock
        endif
        APPE FROM GRAREL80.TXT SDF
        GO TOP
     ELSE
        use arqrel10
        if flock()
           zap
           unlock
        endif
        APPE FROM GRAREL10.TXT SDF
        GO TOP
     ENDIF

     DEFINE WINDOW CELULA FROM 0,0 TO 49,79 && GROW FLOAT ZOOM CLOSE NONE
     BROWSE WINDOW CELULA NOMODIFY NODELETE NOMENU COLOR +W/N,+N/W,GR+/N,+GR/N,+GR/N
     SET DISPLAY TO VGA25
     SET MESSAGE TO 23 CENTER
     REST SCREEN FROM TELA

     MENS = 'Quer imprimir o Relat¢rio ?'
     DO PEGRESP WITH [NS]
     IF MC = 'S'
        DO RESEIMPR
        SET PRINT ON
        IF TAMREL = 2
           MENS = 'Papel de 80 (1) ou de 132 (2) colunas? '
           DO PEGRESP WITH [12]
        ENDIF
        CI = MC
        for xx = 1 to mcopias
          IF CI = '1'
            DO COMPIMPR
            RUN TYPE GRAREL10.TXT > PRN
          ELSE
             RUN TYPE GRAREL80.TXT > PRN
          ENDIF
          EJECT
        next xx
     ENDIF
  ELSE
     IF DEFREL = 'I'
        EJECT
     ENDIF
  ENDIF
  CLOSE DATABASES
  SET PRINT OFF
  SET DEVI TO SCRE
RETURN

* -----------------------------------------------------------
* RESEIMPR - RESETA A IMPRESSORA E VOLTA PARA 132 COLUNAS

  PROCEDURE RESEIMPR

  _PLENGHT = 1
  _PLINENO = 0
  _PAGENO  = 1
  _PCOPIES = 1
  _PBPAGE  = 1
  _PEPAGE  = 1
  _PSCODE  = ""
  _PECODE  = ""

*  SET PRINT ON
  IF TIPOIMPR = 'R'
    ??? CHR(27) + CHR(64) + CHR(30) + '0'
  ELSE
    IF TIPOIMPR = 'E'
      ??? CHR(18)
    ELSE
      ??? CHR(27)+"&l7.27C"+chr(27)+"(s0p10H"
    ENDIF
  ENDIF
*  SET PRINT OFF
  RETURN
* -------------------------------------------------------
PROCEDURE ABREVGA       && TELA PARA 45 LINHAS
  SET DISPLAY TO VGA50
  RETURN
* -------------------------------------------------------
PROCEDURE FECHAVGA      && VOLTA TELA PARA 25 LINHAS
  SET DISPLAY TO VGA25
  RETURN
* -------------------------------------------------------
* CABTELA - TELA DE INICIALIZACAO DAS VARIAVEIS DO SISTEMA
FUNCTION CABTELA
  PARAMETERS LI,CI,LF,CF
  PRIVATE LI,CI,LF,CF
  LINTELA = PAR0_P1+'/'+PAR1_P2
  @ LI,CI FILL TO LF,CF
  @ LI,CI TO LF,CF DOUBLE COLOR &LINTELA
  @ LI+1,CI+2 SAY DTOC(HOJE) COLOR &LINTELA
  @ LI+2,CI TO LI+2,CF COLOR &LINTELA
  @ LI+2,CI SAY CHR(199) COLOR &LINTELA
  @ LI+2,CF SAY CHR(182) COLOR &LINTELA
  @ LI+(22-LI),CI TO LI+(22-LI),CF COLOR &LINTELA
  @ LI+(22-LI),CI SAY CHR(199) COLOR &LINTELA
  @ LI+(22-LI),CF SAY CHR(182) COLOR &LINTELA
RETURN.T.
* CABTELA - TELA DE INICIALIZACAO DAS VARIAVEIS DO SISTEMA
FUNCTION CABTELAC
  PARAMETERS LI,CI,LF,CF
  PRIVATE LI,CI,LF,CF
  LINTELA = PAR1_P1+'/'+PAR1_P2
  @ LI,CI FILL TO LF,CF
  @ LI,CI TO LF,CF DOUBLE COLOR &LINTELA
  @ LI+1,CI+2 SAY DTOC(HOJE) COLOR &LINTELA
  @ LI+2,CI TO LI+2,CF COLOR &LINTELA
  @ LI+2,CI SAY CHR(199) COLOR &LINTELA
  @ LI+2,CF SAY CHR(182) COLOR &LINTELA
  @ LI+(22-LI),CI TO LI+(22-LI),CF COLOR &LINTELA
  @ LI+(22-LI),CI SAY CHR(199) COLOR &LINTELA
  @ LI+(22-LI),CF SAY CHR(182) COLOR &LINTELA
RETURN.T.
* --------------------------------------------------------
* CABTELA45 - TELA DE INICIALIZACAO DAS VARIAVEIS DO SISTEMA
FUNCTION CABTELA45
  PARAMETERS LI,CI,LF,CF
  PRIVATE LI,CI,LF,CF
  LINTELA = PAR0_P1+'/'+PAR1_P2
  @ LI,CI FILL TO LF,CF
  @ LI,CI TO LF,CF DOUBLE COLOR &LINTELA
  @ LI+1,CI+2 SAY DTOC(HOJE) COLOR &LINTELA
  @ LI+2,CI TO LI+2,CF COLOR &LINTELA
  @ LI+2,CI SAY CHR(199) COLOR &LINTELA
  @ LI+2,CF SAY CHR(182) COLOR &LINTELA
  @ LI+47,CI TO LI+47,CF COLOR &LINTELA
  @ LI+47,CI SAY CHR(199) COLOR &LINTELA
  @ LI+47,CF SAY CHR(182) COLOR &LINTELA
RETURN.T.
* -------------------------------------------------------------------------
PROCEDURE TELAI
CLEAR ALL
SET DEVI TO SCRE
*
PUBLIC VOLMENU,IMPRCODM,IMPRALPHAM,TITULO,NAOCAD,MC,TESTPRIN, MENSCGC
PUBLIC MAIN_SCR,MAIN2_SCR,BOXFRAME,RELALPHA,RELCOD,ATUCAD,ENCMENU,INCALTEXC
PUBLIC MREL,MIMPRES,MMVAR,MFATURA,MSIS,CORRIJA,ENCERRAR,IMPRALPHA,IMPRCOD
PUBLIC CNUMER,CALPHA,CALPHAN,ECODIGO,EMENS,TESTEINC,TESTECAD,DADCORR,USESETAS
PUBLIC MTPRG,MTATR,MIMPOR,MINTERNO,MINTERNA,MEMBALAG,MVARVTOT,MVALTOT
PUBLIC MVARVAL,LIN,CFRETE,RLINHA,V,POS,IMPRESS,PAG,PAGINA,TIMP,CP
PUBLIC SELCOR,TDATA,TIPOIMPR,TREL,SREL,CORSCHE,DEFREL,TAMREL,MPLIQUIDO,MICM
PUBLIC VDUP1,VDUP2,VDUP3,VDUP4,VDUP5,VDUP6,MEDUP
PUBLIC VENDUP1,VENDUP2,VENDUP3,VENDUP4
PUBLIC MCP1,MCP2,MCP3,MCP4,MPE1,MPE2,MPE3,MPE4,MCPD,INICIO,MEST
PUBLIC CORARQ,CORVER,OPCAO,MC,RET,LINTELA,VNOMEPRG
PUBLIC ADIAMES, MESBASE, EmptyRepr, EmptyRegiao
PUBLIC MRAZAOSOC

*
* Nomes dos Meses

DIMENSION ADIAMES(12)

* Numero de dias do mes para calculos financeiros
MESBASE = 28

DO GETDADOSEMP

ADIAMES(1)  = 31
ADIAMES(2)  = 28
ADIAMES(3)  = 31
ADIAMES(4)  = 30
ADIAMES(5)  = 31
ADIAMES(6)  = 30 
ADIAMES(7)  = 31
ADIAMES(8)  = 31
ADIAMES(9)  = 30 
ADIAMES(10) = 31
ADIAMES(11) = 30
ADIAMES(12) = 31
OPCAO = SPACE(01)
MC    = SPACE(01)
MENSCGC  = 'C.G.C. ‚ Inv lido'
TESTPRIN = 'Impressora desligada ou falta papel... Retorna sem imprimir ?'
SELCOR   = 'Relat¢rio selecionado est  correto ?'
ATUCAD   = 'Atualiza‡„o do Cadastro      '
RELCOD   = 'Relat¢rio em ordem de C¢digo '
RELALPHA = 'Relat¢rio em ordem Alfab‚tica'
VOLMENU  = 'Volta ao Menu anterior       '
ENCMENU  = 'Encerra Menu atual e volta ao Menu anterior'
INCALTEXC= 'Inclui, Altera, Consulta ou Exclui dados do Cadastro'
CNUMER   = 'Campo n„o pode ser zeros'
CALPHA   = 'Campo n„o pode ser brancos'
CALPHAN  = 'Campo n„o pode ser brancos ou zeros'
ECODIGO  = '[ESC] para encerrar/sair e [F1] para consultar Arquivos'
EMENS    = 'encerra a atualiza‡„o'
TESTEINC = 'J  Cadastrado - Retorna (1), Altera (2) ou Exclui (3)? '
TESTECAD = 'N„o cadastrado - Inclui ?'
DADCORR  = 'Dados est„o corretos ?'
CORRIJA  = 'N„o cadastrado. ENTER para corrigir ou informar novamente'
ENCERRAR = 'p/encerrar'
IMPRALPHA= 'Imprimindo Relat¢rio em ordem Alfab‚tica... Aguarde'
IMPRCOD  = 'Imprimindo Relat¢rio em ordem de C¢digo... Aguarde'
IMPRALPHAM='Imprime Relat¢rio do Cadastro em ordem Alfab‚tica'
IMPRCODM = 'Imprime Relat¢rio do Cadastro em ordem de C¢digo'
NAOCAD   = 'N„o cadastrado... Confira e re-entre com a informa‡„o...'
TIMP     = 'I'
_PDSETUP = ""
_PEJECT  = "AFTER"
EmptyRepr   = '    .  '
EmptyRegiao = '    .  '
*
*************** IDENTIFICACAO  DA  EMPRESA  USUARIA / SISTEMA / VERSAO ******
PUBLIC Versao, CalcDifDup

MSIS     = 'SISTEMA ADMINISTRA€ŽO VENDAS' 
*
* MREL = 'CENTROFIX Ind.Com. Ltda'
MREL = MRAZAOSOC

RETURN

*******************
FUNCTION GETNOMEMES
*******************
  PARAMETER MES
  PRIVATE ANOMEMES
  
  DIMENSION ANOMEMES(12)

  ANOMEMES(1)  = 'Janeiro'
  ANOMEMES(2)  = 'Fevereiro'
  ANOMEMES(3)  = 'Marco'
  ANOMEMES(4)  = 'Abril'
  ANOMEMES(5)  = 'Maio'
  ANOMEMES(6)  = 'Junho'
  ANOMEMES(7)  = 'Julho'
  ANOMEMES(8)  = 'Agosto'
  ANOMEMES(9)  = 'Setembro'
  ANOMEMES(10) = 'Outubro'
  ANOMEMES(11) = 'Novembro'
  ANOMEMES(12) = 'Dezembro'
  
  M = ''
  IF MES >= 1 .AND. MES <= 12
    M = ANOMEMES[MES]
  ENDIF  

RETURN M

*-----------------------------------------------------------------------
function TELAG
clear

 TESTE1="ÉÍ»º¼ÍÈº"

pos = int(13 + (((65 - 13) - LEN(MRAZAOSOC)) / 2))
@ 00,00,03,11 BOX TESTE1
@ 00,13,03,65 BOX TESTE1
@ 00,67,03,79 BOX TESTE1
@ 22,00 to 22,79 double
@ 24,00 to 24,79 double
*
=VERSAOSYS()
@ 01,14 SAY SPACE(10) + MSIS 
*@ 02,14 SAY '            CENTROFIX Ind. e Com. Ltda'
@ 02, pos SAY MRAZAOSOC
*@ 02,14 say '                 Ad†o Eli Pereira'
=NOMEMOD( 'AV' )
@ 02, 69 SAY HOJE
RETURN .T.
* -------------------------------------------------------------------------
function janela
parameters li,ci,lf,cf
PUBLIC i

  @ li,ci clear to lf,cf
  LINTELA = PAR4_P1+'/'+PAR1_P2
  @ li,ci to lf,cf double  COLOR &LINTELA
  *@ li,ci + 2 say chr(001) COLOR &LINTELA   && c rosto
  *@ li,ci say chr(024)     COLOR &LINTELA   && seta cima
  *@ li,cf say chr(025)     COLOR &LINTELA   && seta baixo
  *@ li,ci say chr(024)     COLOR &LINTELA   && seta baixo
  *@ li,cf say chr(025)     COLOR &LINTELA   && seta baixo
   * for i = (li + 1) to (lf -1)
   *   @ i,ci say chr(186) COLOR &LINTELA   && quadrado cheio
   *   @ i,cf say chr(186) COLOR &LINTELA  && quadrado cheio
   * endfor
  return .T.

*=====================
function borda
parameters li,ci,lf,cf
*=====================  
  *private c1,c2,l1,l2
  *SET COLOR OF SCHEME 1 TO &CORSCHE
  *@ li-1,ci+1 say replicate(chr(223),cf-ci)
  *  for l1=li-1 to lf-1
  *    @ l1,cf+1 say chr(222)
  *  endfor
  *@ li,ci TO lf, cf DOUBLE
   
return(.t.)

*---------------------------------------------------------------------------
* CENTRALIZA E IMPRIME MENSAGENS - LINHA 23
FUNCTION MENSAG
  PARAMETERS MENS
    @ 23,01 SAY SPACE(78)
    POS = 40 - INT(LEN(MENS) / 2)
    LINTELA = PAR4_P1+'/'+PAR1_P2
    @ 23,POS SAY MENS color &LINTELA
  RETURN POS
*-------------------------------------------------------------
* CENTRALIZA E IMPRIME MENSAGENS - LINHA 49
FUNCTION MENSAG49
  PARAMETERS MENS
    @ 48,01 SAY SPACE(78)
    POS = 40 - INT(LEN(MENS) / 2)
    @ 48,POS SAY MENS color &LINTELA
  RETURN POS
*-------------------------------------------------------------
* CENTRALIZA E IMPRIME MENSAGENS - LINHA 22
FUNCTION MENSAG22
  PARAMETERS MENS22
  @ 22,01 SAY SPACE(78)
    POS = 40 - INT(LEN(MENS22) / 2)
  @ 22,POS SAY MENS22 COLOR &LINTELA
*--------------------------------------------------------------
* LIMPA A LINHA 23
FUNCTION LIMPA23
  @ 23,01 SAY SPACE(78)
*---------------------------------------------------------------------------
* CABEC - IMPRIME LINHA BASICA DO CABECALHO DOS RELATORIOS
*
FUNCTION CABEC
PARAMETERS PR,TIT,HIF
  PAGCTR = PAGCTR + 1
  @ 01,01 SAY MREL
  @ 01,30 SAY PR
  @ 01,42 SAY TIT
  @ 01,112 SAY HOJE
  @ 01,124 SAY 'PAG '+STR(PAGCTR,3)
  @ 02,01 SAY REPLICATE('-',HIF)
RETURN .T.
* --------------------------------------------------------------------------
* CABEC80 - IMPRIME LINHA BASICA DO CABECALHO DOS RELATORIOS (80 COLUNAS)
*
FUNCTION CABEC80
PARAMETERS PR,TIT,HIF
  PAGCTR = PAGCTR + 1
  @ 01,01 SAY TIT
  @ 01,69 SAY 'PAG '+STR(PAGCTR,3)
  @ 02,01 SAY MREL
  @ 02,49 SAY PR
  @ 02,59 SAY TIME()
  @ 02,69 SAY HOJE
  @ 03,01 SAY REPLICATE('-',HIF)
RETURN
* --------------------------------------------------------------------------

* CABEC132 - IMPRIME LINHA BASICA DO CABECALHO DOS RELATORIOS (132 COLUNAS)
*
*--------------------
FUNCTION CABEC132
PARAMETERS Modulo,Titulo
*--------------------
  PAGCTR = PAGCTR + 1
  @ 00, 00 SAY CHR(15)
  @ 01, 00 SAY REPLICATE('=',133)
  @ 02, 00 SAY MREL
  @ 02,116 SAY TIME()
  @ 02,125 SAY HOJE
  @ 03, 00 SAY Modulo
  @ 03, CentraTxt(Titulo,132) SAY Titulo
  @ 03,124 SAY 'PAG: ' + STR(PAGCTR,3)
  @ 04, 00 SAY REPLICATE('-',133)
 
RETURN

* CABEC219 - IMPRIME LINHA BASICA DO CABECALHO DOS RELATORIOS (219 COLUNAS)
*
*--------------------
FUNCTION CABEC219
PARAMETERS Modulo,Titulo
*--------------------
  PRIVATE nCol
  
  nCol = 219
  
  PAGCTR = PAGCTR + 1
  @ 00, 00 SAY CHR(15)
  @ 01, 00 SAY REPLICATE('=',nCol)
  @ 02, 00 SAY MREL
  @ 02,202 SAY TIME()
  @ 02,211 SAY HOJE
  @ 03, 00 SAY Modulo
  @ 03, CentraTxt(Titulo,nCol) SAY Titulo
  @ 03, 212 SAY 'PAG: ' + STR(PAGCTR,3)
  @ 04, 00 SAY REPLICATE('-',nCol)
 
RETURN

* --------------------------------------------------------------------------
* CENTRALIZA E IMPRIME MENSAGENS EM LINHAS ESPECIFICADAS
FUNCTION MENSAGR
  PARAMETERS MENS
  POS = 40 - INT(LEN(MENS) / 2)
  RETURN POS
* --------------------------------------------------------------------------
* FCGC - CALCULA OS DIGITOS DE CONTROLE DO CGC
*
FUNCTION FCGC
PARAMETERS CGCT
PRIVATE cTmp, i, cNum, nTam
* Remove caracteres nao Numericos

cTmp = ''
nTam = LEN(CGCT)
FOR i = 1 TO nTam
  cNum = SUBSTR(CGCT,i,1)
  IF cNum $ '0123456789' 
    cTmp = cTmp + cNum
  ENDIF  
NEXT i

CGCT = SUBSTR(cTmp, 1, 14)
CONTADOR = 1
  DO WHILE CONTADOR <= 12
    D=IIF(CONTADOR < 10, '0'+STR(CONTADOR,1),STR(CONTADOR,2))
    D1&D = VAL(SUBS(CGCT,CONTADOR,1))
    CONTADOR = CONTADOR + 1
  ENDDO
*
DF1=5*D101+4*D102+3*D103+2*D104+9*D105+8*D106+7*D107+6*D108+5*D109+4*D110+3*D111+2*D112
DF2 = DF1 / 11
DF3 = INT(DF2) * 11
RESTO1 = DF1 - DF3
*
  IF RESTO1 = 0 .OR. RESTO1 = 1
    PRIDIG = 0
  ELSE
    PRIDIG = 11 - RESTO1
  ENDIF
* ----------------------------------------------------------------------------
*               SEGUNDO DIGITO
* ----------------------------------------------------------------------------
CONTADOR = 1
  DO WHILE CONTADOR <= 12
    D=IIF(CONTADOR < 10, '0'+STR(CONTADOR,1),STR(CONTADOR,2))
    D2&D = VAL(SUBS(CGCT,CONTADOR,1))
    CONTADOR = CONTADOR + 1
  ENDDO
*
DF4=6*D101+5*D102+4*D103+3*D104+2*D105+9*D106+8*D107+7*D108+6*D109+5*D110+4*D111+3*D112+2*PRIDIG
DF5 = DF4 / 11
DF6 = INT(DF5) * 11
RESTO2 = DF4 - DF6
*
  IF RESTO2 = 0 .OR. RESTO2 = 1
    SEGDIG = 0
  ELSE
    SEGDIG = 11 - RESTO2
  ENDIF
*
IF PRIDIG <> VAL(SUBS(CGCT,13,1)) .OR. SEGDIG <> VAL(SUBS(CGCT,14,1))
  MC = 'N'
ELSE
  MC = 'S'
ENDIF
IF PRIDIG <> VAL(SUBS(CGCT,13,1)) .OR. SEGDIG <> VAL(SUBS(CGCT,14,1))
  return .f.
ENDIF
RETURN .t.
* --------------------------------------------------------------------------
* FCPF - CALCULA O DIGITO DE CONTROLE DO "C P F"
*
FUNCTION FCPF
PARAMETERS CPFT
*
* ----------------------------------------------------------------------------
*                PRIMEIRO DIGITO
* ----------------------------------------------------------------------------
               CONTADOR = 1
                 DO WHILE CONTADOR <= 9
                   D = '0' + STR(CONTADOR,1)
                   D2&D = VAL(SUBS(CPFT,CONTADOR,1))
                   CONTADOR = CONTADOR + 1
                 ENDDO
*
                   DF4=10*D201+9*D202+8*D203+7*D204+6*D205+5*D206+4*D207+3*D208+2*D209
                   DF5 = DF4 / 11
                   DF6 = INT(DF5) * 11
                   RESTO1 = DF4 - DF6
*
                 IF RESTO1 = 0 .OR. RESTO1 = 1
                   PRIDIG = 0
                 ELSE
                   PRIDIG = 11 - RESTO1
                 ENDIF
* ----------------------------------------------------------------------------
*               SEGUNDO DIGITO
* ----------------------------------------------------------------------------
              CONTADOR = 1
                DO WHILE CONTADOR <= 9
                  D = '0' + STR(CONTADOR,1)
                  D2&D = VAL(SUBS(CPFT,CONTADOR,1))
                  CONTADOR = CONTADOR + 1
                ENDDO
*
                  DF4=11*D201+10*D202+9*D203+8*D204+7*D205+6*D206+5*D207+4*D208+3*D209+2*PRIDIG
                  DF5 = DF4 / 11
                  DF6 = INT(DF5) * 11
                  RESTO1 = DF4 - DF6
*
                IF RESTO1 = 0 .OR. RESTO1 = 1
                  SEGDIG = 0
                ELSE
                  SEGDIG = 11 - RESTO1
                ENDIF
*
                  PRIDIG = STR(PRIDIG,1)
                  SEGDIG = STR(SEGDIG,1)
*
                IF PRIDIG <> SUBS(CPFT,10,1) .OR. SEGDIG <> SUBS(CPFT,11,1)
                  MC = 'N'
                ELSE
                  MC = 'S'
                ENDIF
*
RETURN

**************
FUNCTION FDATA
**************
PARAMETER MDATA

 TDATA  = ' '
 
RETURN .T.


* --------------------------------------------------------------------------
FUNCTION EXTENSOP
*           - ESCREVE VALORES POR EXTENSO E PASSA AO PROGRAMA SOLICITANTE
*             PARA DETERMINAR TAMANHO DE LINHA E IMPRIMIR
*
NUMERO = M.VDP
N01 = 'UM '
N02 = 'DOIS '
N03 = 'TRES '
N04 = 'QUATRO '
N05 = 'CINCO '
N06 = 'SEIS '
N07 = 'SETE '
N08 = 'OITO '
N09 = 'NOVE '
N00 = ' '
N10 = 'DEZ '
N11 = 'ONZE '
N12 = 'DOZE '
N13 = 'TREZE '
N14 = 'QUATORZE '
N15 = 'QUINZE '
N16 = 'DEZESSEIS '
N17 = 'DEZESSETE '
N18 = 'DEZOITO '
N19 = 'DEZENOVE '
N20 = 'VINTE '
N30 = 'TRINTA '
N40 = 'QUARENTA '
N50 = 'CINCOENTA '
N60 = 'SESSENTA '
N70 = 'SETENTA '
N80 = 'OITENTA '
N90 = 'NOVENTA '
N000 = ' '
N100 = 'CENTO '
N200 = 'DUZENTOS '
N300 = 'TREZENTOS '
N400 = 'QUATROCENTOS '
N500 = 'QUINHENTOS '
N600 = 'SEISCENTOS '
N700 = 'SETECENTOS '
N800 = 'OITOCENTOS '
N900 = 'NOVECENTOS '
MIL = 'MIL '
MILHAO = 'MILHAO '
MILHOES = 'MILHOES '
MOEDA    = 'CRUZEIRO '
MOEDA1  = 'CRUZEIROS '
CENTAVO = 'CENTAVO '
CENTAVOS = 'CENTAVOS '
*
NUMERO1 = STR(NUMERO,12,2)
CENTS   = SUBS(NUMERO1,11,2)
NUMERO  = INT(NUMERO)
CONTADOR = 1
P1 = SUBS(NUMERO1,1,3)
P2 = SUBS(NUMERO1,4,3)
P3 = SUBS(NUMERO1,7,3)
P4 = CENTS
TEXTO1 = ''
TEXTO2 = ''
TEXTO3 = ''
TEXTO4 = ''
LINHA1 = ''
LINHA2 = ''
LINHA3 = ''
*
IF VAL(P1) > 0
  STATUS1 = IIF(VAL(P1) > 1, '2','1')
ELSE
  STATUS1 = ' '
ENDIF
*
DO WHILE CONTADOR <= 4
  ITEM = STR(CONTADOR,1)
  GRUPO = 'P'+STR(CONTADOR,1)
  EXTENSO = IIF(&GRUPO='000','',LTRIM(&GRUPO))
    DO CASE
      CASE LEN(EXTENSO) = 3

        IF EXTENSO = '100'
          TEXTO&ITEM = TEXTO&ITEM + 'CEM '
          CONTADOR = CONTADOR + 1
          LOOP
        ENDIF

        IF SUBS(EXTENSO,2,2) = '00'
          TAB = 'N' + SUBS(EXTENSO,1,1) + '00'
          TEXTO&ITEM = TEXTO&ITEM + &TAB
          CONTADOR = CONTADOR + 1
          LOOP
        ENDIF

        TAB = 'N' + SUBS(EXTENSO,1,1) + '00'
        TEXTO&ITEM = &TAB + 'E '
        DEZENA = VAL(SUBS(EXTENSO,2,2))

          IF DEZENA < 20
            TAB = 'N' + SUBS(EXTENSO,2,2)
            TEXTO&ITEM = TEXTO&ITEM + &TAB
            CONTADOR = CONTADOR + 1
            LOOP
          ELSE
            TAB = 'N' + SUBS(EXTENSO,2,1) + '0'
            TEXTO&ITEM = TEXTO&ITEM + &TAB
            TAB = 'N0' + SUBS(EXTENSO,3,1)
            UNIDADE = VAL(SUBS(EXTENSO,3,1))

              IF UNIDADE > 0
                TEXTO&ITEM = TEXTO&ITEM + IIF(VAL(EXTENSO) > 10,'E ','')
              ENDIF

            TEXTO&ITEM = TEXTO&ITEM + IIF(TAB = 'N00',' ',&TAB)
          ENDIF

    CASE LEN(EXTENSO) = 2
      DEZENA = VAL(SUBS(EXTENSO,1,2))

        IF DEZENA < 20
          TAB = 'N' + SUBS(EXTENSO,1,2)
          TEXTO&ITEM = TEXTO&ITEM + &TAB
          CONTADOR = CONTADOR + 1
          LOOP
        ELSE
          TAB = 'N' +SUBS(EXTENSO,1,1) + '0'
          TEXTO&ITEM = TEXTO&ITEM + &TAB
          TAB = 'N0' + SUBS(EXTENSO,2,1)
          UNIDADE1 = VAL(SUBS(EXTENSO,2,1))

            IF UNIDADE1 > 0
              TEXTO&ITEM = TEXTO&ITEM + 'E '
            ENDIF

          TEXTO&ITEM = TEXTO&ITEM + IIF(TAB = 'N00',' ',&TAB)
        ENDIF

    CASE LEN(EXTENSO) = 1
      TAB = 'N0' + SUBS(EXTENSO,1,1)
      TEXTO&ITEM = TEXTO&ITEM + IIF(TAB = 'N00',' ',&TAB)
    OTHERWISE
      TEXTO&ITEM = ''
    ENDCASE

      CONTADOR = CONTADOR + 1

ENDDO
*
IF VAL(P1 + P2 + P3) = 0 .AND. VAL(P4) # 0
  FINAL = TEXTO4 + (IIF(VAL(P4) = 1,CENTAVO,CENTAVOS))
*  LOOP
ENDIF
*
FINAL=IIF(LEN(TEXTO1)=0,'',TEXTO1+IIF(STATUS1='1',MILHAO,MILHOES))+IIF (LEN(TEXTO2)=0,'', TEXTO2+MIL)+TEXTO3+IIF(VAL(P2+P3)=0,'DE ','')

FINAL = FINAL + IIF(VAL(P1+P2+P3)=0,'',IIF(VAL(P1+P2+P3)=1,MOEDA,MOEDA1))

FINAL=FINAL+IIF(VAL(P4)=0,'',IIF(VAL(P1+P2+P3)=0,'','E ') +TEXTO4+(IIF(VAL(P4)=1,CENTAVO,CENTAVOS)))

*
* PASSA VARIAVEL EXTENSO PARA IMPRESSAO
*
VREXT = FINAL
*
RETURN
* --------------------------------------------------------------------------
* INCLUIOB - INCLUSAO DE OBSERVACOES, BANCO E BASE DE ICM REDUZIDO EM N.F.
*
PROCEDURE INCLUIOB
parameters vproc
  STOR SPACE(60) TO MOBSNF1, MOBSNF2, MOBSNF3, MOBSNF4
  STOR SPACE(60) TO MOBSNF5, MOBSNF6, MOBSNF7, MOBSNF8
*  MESPECIE = SPACE(17)
  MESPECIE = 'VOLUMES          '
  MVEICULO = SPACE(08)
  MQTDVOL  = 0
  MPBRUTO  = 0.00
  MPLIQ    = 0.00
*  MENS = 'Inclui Observa‡”es para imprimir na Nota Fiscal ?'
*  DO PEGRESP WITH [NS]
        IF MC = 'S'
          =CABTELA(0,0,24,79)
          MENS = 'Dados complementares para imprimir na Nota Fiscal'
          =MENSAGR(MENS)
          @ 01,POS SAY MENS
*          @ 04,01 SAY '                        A t e n ‡ „ o'
*          @ 05,01 say '  As observa‡”es informadas destinam-se a um Cliente, representado pelo seu'
*          @ 06,01 say '  C¢digo e ser„o impressas na primeira Nota Fiscal do Cliente, caso  houver'
*          @ 07,01 say '  mais de uma. Portanto, se necess rio imprimir observa‡”es diferentes para'
*          @ 08,01 say '  observa‡”es para cada Nota Fiscal, selecionar os Pedidos para cada Nota e'
*          @ 09,01 say '  imprimi-la antes de continuar a sele‡„o de Pedidos'

*          MENS = 'Continua com a inclus„o das Observa‡”es ?'
*          DO PEGRESP WITH [SN]
*            IF MC = 'S'
              @ 04,09 CLEAR TO 21,72
              @ 04,09 TO 15,72 double
              @ 06,11 GET MOBSNF1
              @ 07,11 GET MOBSNF2
              @ 09,11 GET MOBSNF3
              @ 10,11 GET MOBSNF4
              @ 12,11 GET MOBSNF5
              @ 13,11 GET MOBSNF6
*             @ 12,05 GET MOBSNF7
*             @ 13,05 GET MOBSNF8
                READ
*            ENDIF
        ENDIF

          SELE BANCOS
          @ 17,01 CLEAR TO 22,78
          @ 17,01 TO 22,78
          @ 18,07 SAY 'Qtd.Vol.'
          @ 18,17 SAY 'Especie'
          @ 18,36 say '  P.Bruto'
          @ 18,46 say 'P.L¡quido'
          @ 20,09 GET MQTDVOL PICT '99999' VALID MQTDVOL > 0
          @ 20,17 GET MESPECIE
          @ 20,36 GET MPBRUTO PICT '99999.99' VALID ChkPesoBruto()
          @ 20,46 GET MPLIQ   PICT '99999.99' VALID ChkPesoLiq()
          READ
          
          IF LASTKEY() = 27
            RETURN
          ENDIF     
          IF MEDUP = 'S'
            @ 18,56 SAY 'Bco/Agˆn'
         *if vproc = 'AV07600'   &&---- Emiss„o de N. Fiscal sem Pedido
            DO WHILE .T.
              @ 20,56 GET MNBANCO PICT '999'
              @ 20,60 GET MAGENCIA PICT '9999'
              READ
              ? MENSAG(CORRIJA)
              IF SEEK(STR(MNBANCO,3) + STR(MAGENCIA,4))
                EXIT
              ENDIF
            ENDDO
*else
*   @ 20,56 say MNBANCO  PICT '999'    color &corver
*   @ 20,60 say MAGENCIA PICT '9999'   color &corver
*endif
            ENDIF

            SELE VEICULOS
            @ 18,66 SAY 'Ve¡culo'
            @ 20,66 GET MVEICULO PICT '@!'
            READ

            MVALORICM = 0.00
            MBASEICM  = 0.00

            MENS = 'Destaca ICM reduzido ?'
            DO PEGRESP WITH [NS]
            IF MC = 'S'
              @ 21,05 SAY 'Valor do ICM reduzido 'GET MVALORICM PICT '999999999.99'
              @ 21,45 SAY 'Base do ICM 'GET MBASEICM PICT '9999999999.99'
                READ
            ENDIF
            SELE OBSNOTA
*            SEEK MCGC
            SEEK MCGC + str(MPED,6)
            IF (EOF() .OR. BOF())
              APPE BLAN
            ENDIF
            REPL CGC      WITH MCGC     , ped      with mped    ,;
                 OBSNF1   WITH MOBSNF1  , OBSNF2   WITH MOBSNF2 ,;
                 OBSNF3   WITH MOBSNF3  , OBSNF4   WITH MOBSNF4 ,;
                 OBSNF5   WITH MOBSNF5  , OBSNF6   WITH MOBSNF6
*                OBSNF7   WITH MOBSNF7  , OBSNF8   WITH MOBSNF8
            REPL QTDVOL   WITH MQTDVOL  , ESPECIE  WITH MESPECIE,;
                 PBRUTO   WITH MPBRUTO  , PLIQUIDO WITH MPLIQ   ,;
                 BANCO    WITH MNBANCO  , AGENCIA  WITH MAGENCIA,;
                 VALORICM WITH MVALORICM, BASEICM  WITH MBASEICM,;
                 VEICULO  WITH MVEICULO

    RETURN
* -------------------------------------------------------
PROCEDURE ESCONDE
  DEACTIVATE WINDOW TELA
  SET ESCA OFF
RETURN

* ------------------------------------------------------
FUNCTION CENTRATXT
PARAMETER Texto, TamLinha
  PRIVATE Coluna, TamTexto

  TamTexto = LEN(Texto)

  Coluna = (TamLinha - TamTexto) / 2

RETURN Coluna

*-----------------------------------------
* Esta funcao restorna o numero de dias do
* mes da data 
*===============
FUNCTION NDIAMES
*===============
PARAMETER dDataMes
  PRIVATE NDias, Mes, Ano
  
  Mes = MONTH( dDataMes )
  Ano = YEAR( dDataMes )
  nDias = 0
  IF Mes >= 1 .AND. Mes <= 12
     NDias = ADIAMES(Mes)
     IF Mes == 2 .AND. Bissexto(Ano)
        NDias = NDias + 1
     ENDIF   
  ENDIF   
              
RETURN NDias

*----------------------------------
* Retorna .T. se o ano for bissexto
*=================
FUNCTION BISSEXTO
*=================
PARAMETER Ano
  PRIVATE lRet

  lRet = MOD(Ano, 4) == 0 .AND. MOD(Ano, 100) != 0 .OR. MOD(Ano, 400) == 0
  
RETURN lRet

*========================================
FUNCTION ACUMDIFDUPL
PARAMETER TOT, Porc1, Porc2, Porc3, Porc4
*========================================
 PRIVATE TotInteiro, Dif
 
 Dif = 0
 TotInteiro = INT(TOT*(Porc1/100))+INT(TOT*(Porc2/100))+INT(TOT*(Porc3/100))+INT(TOT*(porc4/100))
 IF Tot <> TotInteiro
    Dif = Tot - TotInteiro
 ENDIF
 
RETURN Dif    
    
*------------------------------------------------
* Esta rotina apresenta o Nome do Modulo corrente
*------------------------------------------------
*****************
PROCEDURE NOMEMOD
PARAMETER Nome
*****************
  PRIVATE Col

  Col = INT((10 - LEN(NOME)) / 2) + 1
  @ 01, Col SAY Nome
  
RETURN  

*------------------------------------------------
* Esta rotina apresenta a Versao do Sistema
*------------------------------------------------
*******************
PROCEDURE VERSAOSYS
*******************

 @ 02, 02 SAY Versao
    
RETURN  

* Nova Versao, nao fecha arquivos ja abertos
*******************
PROCEDURE IMPSAIDA1 && - DEFINE SE SAIDA NA IMPRESSORA OU NA TELA 
*******************
  private xx, nArea
  if type('MCOPIAS') # 'N'
     mcopias = 1
  endif
  SET DEVI TO SCRE
  IF DEFREL = 'T'
     SAVE SCREEN TO TELA
     CLEAR
     SET DISPLAY TO VGA50
     SELE 0
     IF TAMREL = 1
        use arqrel80
        if flock()
           zap
           unlock
        endif
        APPE FROM GRAREL80.TXT SDF
        GO TOP
     ELSE
        use arqrel10
        if flock()
           zap
           unlock
        endif
        APPE FROM GRAREL10.TXT SDF
        GO TOP
     ENDIF
     nArea = SELECT()         
     DEFINE WINDOW CELULA FROM 0,0 TO 49,79 && GROW FLOAT ZOOM CLOSE NONE
     BROWSE WINDOW CELULA NOMODIFY NODELETE NOMENU COLOR +W/N,+N/W,GR+/N,+GR/N,+GR/N
     SET DISPLAY TO VGA25
     SET MESSAGE TO 23 CENTER
     REST SCREEN FROM TELA

     MENS = 'Quer imprimir o Relat¢rio ?'
     DO PEGRESP WITH [NS]
     IF MC = 'S'
        DO RESEIMPR
        SET PRINT ON
        IF TAMREL = 2
           MENS = 'Papel de 80 (1) ou de 132 (2) colunas? '
           DO PEGRESP WITH [12]
        ENDIF
        CI = MC
        for xx = 1 to mcopias
          IF CI = '1'
            DO COMPIMPR
            RUN TYPE GRAREL10.TXT > PRN
          ELSE
             RUN TYPE GRAREL80.TXT > PRN
          ENDIF
          EJECT
        next xx
     ENDIF
     SELECT( nArea )
  ELSE
     IF DEFREL = 'I'
        EJECT
     ENDIF
  ENDIF

  USE
  SET PRINT OFF
  SET DEVI TO SCRE
RETURN

****************************************************************
* Funcoes Auxiliares 
* Autor: Jair Gon‡alves
*

*====================
FUNCTION CHKPESOBRUTO
*====================
  PRIVATE lPesoValido
  
  lPesoValido = MPBRUTO > 0
  IF .NOT. lPesoValido
    =MENSAG( '<< Peso Bruto deve ser maior que Zero >>'  )
  ENDIF
      
RETURN lPesoValido

*==================
FUNCTION CHKPESOLIQ
*==================
  PRIVATE lPesoValido
  
  lPesoValido = MPLIQ < MPBRUTO 
  IF .NOT. lPesoValido
    =MENSAG( '<< Peso Liquido deve ser menor que o Peso Bruto >>'  )
  ENDIF
      
RETURN lPesoValido

* Esta Procedure calcula a despesa financeira 
* Parametros
* ValorLiq      -> Valor a Calcular
* CPag1..CPag4  -> Dias da Data
*==================
FUNCTION DESPFINANC
*==================
PARAMETER ValorLiq, Taxa, CPag1, nPe1, CPag2, nPe2, CPag3, nPe3, Cpag4, nPe4
  PRIVATE  nDespesa, APe, ACOND, AFATOR, i, SomaFt
 
  DIMENSION APe(4), ACOND(4), AFATOR(4)
 
  nDespesa = 0
  *--- Calcula o Valor Presente Inicial
  APe[1] = nPe1 / 100
  APe[2] = nPe2 / 100
  APe[3] = nPe3 / 100
  APe[4] = nPe4 / 100
  ACond[1] = CPag1
  ACond[2] = CPag2
  ACond[3] = CPag3 
  ACond[4] = CPag4 
  SOMAFT = SomaFator( Taxa, aCond[1], aCond[2], aCond[3], aCond[4],;
                            aPe[1],   aPe[2],   aPe[3],   aPe[4] )

  FOR i = 1 TO 4
    AFATOR[i] = APe[i] / SOMAFT
  NEXT i
                    
  SOMA = 0
  FOR i = 1 TO 4
    SOMA = SOMA + (ValorLiq * AFATOR[i])
  NEXT i  
       
  nDespesa = SOMA - ValorLiq
       
RETURN ROUND( nDespesa, 2)


****************************
* Calcula o Valor Presente 
* de uma serie de pagamentos
*
*=================
FUNCTION SOMAFATOR
*=================
PARAMETER nTaxa, Cond1, Cond2, Cond3, Cond4, Pe1, Pe2, Pe3, Pe4 
  PRIVATE SOMAFT, I, aPe, aCond

  DIMENSION aPe(4), aCond(4)
  
  aPe[1] = Pe1 
  aPe[2] = Pe2 
  aPe[3] = Pe3 
  aPe[4] = Pe4       
  aCond[1] = Cond1
  aCond[2] = Cond2
  aCond[3] = Cond3
  aCond[4] = Cond4    
  SOMAFT = 0
  FOR I = 1 TO 4
    SOMAFT = SOMAFT + aPe[i] * FAC( nTaxa, ACond[i]/MESBASE )
  NEXT i

RETURN SOMAFT


* Fator de Atualizacao de Capital
*===========
FUNCTION FAC
*===========
PARAMETER i, n  

 PRIVATE nFator
 
 nFator = 1 / (1 + (i / 100))^n

RETURN nFator

*==================
FUNCTION NDIASUTEIS
*==================
PARAMETER dInicio, dFim
  PRIVATE nDias, d, DiaSem
  
  nDias = 0
  d = dInicio
  DO WHILE d <= dFim
    DiaSem = DOW(d)
    IF DiaSem >= 2 .AND. DiaSem <= 6
      nDias = nDias + 1
    ENDIF  
    d = d + 1
  ENDDO
        
RETURN nDias

*******************************************************************************

* Restaura os Settings do Ambiente FOXPRO
*================
PROCEDURE RESTAMB
*================
  
  SET BELL ON
  SET BLINK ON
  SET CONF ON
  SET CONS OFF
  SET DATE BRIT
  SET TALK ON
  SET DELE ON
  SET DELI OFF
  SET DEVI TO SCRE
  SET ESCA ON
  SET EXCL ON
  SET HEAD OFF
  SET MENU ON
  SET NOTI OFF
  SET OPTI ON
  SET PRIN OFF
  SET RESO ON
  SET SAFE OFF
  SET SCOR OFF
  SET STAT ON
  SET CLOCK TO 00,70
  *SET DISPLAY TO VGA50
  
RETURN 

*=================
FUNCTION CHKREGIAO
*=================
  PARAMETER MREGIAO, MEXIBMENS
  PRIVATE lAchou, nAlias, cOrdem, cRegiao, cIdRegiao
  
  nAlias = SELECT()
  cOrdem = ORDER()
  SELECT REGIAO
  SET ORDER TO X_REGIAO
  cRegiao = SUBSTR(MREGIAO,1,4)
  cIdRegiao = STR(VAL(SUBSTR(MREGIAO, 6, 2)), 2)
  lAchou = SEEK(cRegiao + cIdRegiao, 'REGIAO') 
  
  IF ! lAchou .AND. MEXIBMENS
    =MENSAG( 'RegiÆo nÆo cadastrada, tente novamente' )
  ENDIF  
  SELECT( nAlias )
  SET ORDER TO cOrdem
  
RETURN lAchou

*===============
FUNCTION CHKREPR
*===============
  PARAMETER MVEND, MEXIBMENS
  PRIVATE lAchou, cCodigo, cCvr, cIdRepr, cOrdem, nDbf
  
  * Salva o Alias Atual e o Indice Atual
  nDbf    = SELECT()
  cOrdem  = ORDER()
  
  cCvr    = SUBSTR(MVEND,1,4) 
  cIdRepr = STR(VAL(SUBSTR(MVEND, 6, 2)), 2)
  cCodigo = cCvr + cIdRepr
  SELECT VEND
  SET ORDER TO X_VEND
  lAchou  = SEEK(cCodigo, 'VEND') 
  
  IF ! lAchou .AND. MEXIBMENS
    =MENSAG( 'Representante nÆo cadastrado, tecle algo para voltar...' )
    =INKEY(0)
    =MENSAG('')
  ENDIF  
  
  * Restaura o Alias e Indice Atuais
  SELECT(nDbf)
  SET ORDER TO cOrdem
  
RETURN lAchou

*===============
FUNCTION INICPAG
*===============
  PRIVATE cLpt
  
  SET PRINTER TO NUL
  SET DEVI TO PRINTER
  EJECT
  SET DEVI TO SCREEN
  SET PRINTER TO 

RETURN  

*=================
FUNCTION CHKDATANF
*=================
  PRIVATE lDataValida
  
  lDataValida = HOJE >= PARAM->DTEMISSAO
  if .NOT. lDataValida
    =MENSAG('Data de EmissÆo da Nota Fiscal nÆo pode ser retroativa')
    =INKEY(0)
  endif
  
RETURN lDataValida

*------------------------------------------
* Esta Funcao Obtem os Dados do Backup pelo seu Volume
*
*==================
FUNCTION GETVOLDATA
PARAMETER cArquivo, dDataGrav
*=================
  PRIVATE nVol, dDataGrav, nHandle, cLinha
 
  nVol      = 0
  dDataGrav = {}
  nHandle = FOPEN( cArquivo )
  DO WHILE ! FEOF(nHandle)
    cLinha = FGETS( nHandle )
    *--- Localiza a Linha com a Data e Nome do Volume
    n = AT( 'AV.A', cLinha )
    IF n > 0
      nVol = nVol + 1
      IF nVol = 1
        dDataGrav = CTOD( SUBSTR( cLinha, 9, 2 ) + '/' +;
                          SUBSTR( cLinha, 6, 2 ) + '/' +;
                          SUBSTR( cLinha, 1, 4 ) )
      ENDIF                    
    ENDIF  
  ENDDO
  =FCLOSE(nHandle)

RETURN nVol  

********************
PROCEDURE CALCVENCIM
********************

  PRIVATE MES, DIA, MDIA, MMES, AVencEsp, NumVenc, ACondPag, NumPag,;
          APePag, AVencDup, AValDup, ValorNota, QPARC

  NumPag   = 4
  NVencEsp = 6
  DIMENSION AVencEsp(NVencEsp)
  DIMENSION ACondPag(NumPag), APePag(NumPag), AVencDup(NumPag), AValDup(NumPag)

  STOR 0 TO DPAR, MVDP, VDUP1, VDUP2, VDUP3, VDUP4, VPAR, AVPAR

  QDIA = 0
  FORM = SPACE(02)
  STOR SPACE(08) TO VENC1, VENC2, VENC3, VENC4
  STOR SPACE(08) TO VENDUP1, VENDUP2, VENDUP3, VENDUP4

  DIA = DAY(HOJE)
  MES = MONTH(HOJE)

  INICIO = HOJE

  * Copia as Condicoes do pedido para um array
  ACondPag(1) = pedic2->cp1
  ACondPag(2) = pedic2->cp2
  ACondPag(3) = pedic2->cp3
  ACondPag(4) = pedic2->cp4

  * Copia para um array os percentuais por duplicatas do pedido
  APePag(1) = pedic2->pe1
  APePag(2) = pedic2->pe2
  APePag(3) = pedic2->pe3
  APePag(4) = pedic2->pe4

  IF pedic2->cpd = 'VIS' .OR. pedic2->cp1 = 0
    INICIO = HOJE
  ENDIF

  IF pedic2->cpd = 'DFD' .OR. pedic2->cpd = 'DFQ' .OR. pedic2->cpd = 'DFM'
    nDiasFora = 0
    DO CASE
      CASE pedic2->cpd = 'DFD' 
        nDiasFora = 10
      CASE pedic2->cpd = 'DFQ' 
        nDiasFora = 15
      CASE pedic2->cpd = 'DFM' 
        nDiasFora = 30
    ENDCASE
    IF DIA >= nDiasFora 
      nDiasFora = IIF( MES = 2, 28, 30 ) 
    ENDIF
    INICIO = HOJE + nDiasFora - DIA      
  ENDIF   

  MAIOR_DIA = 0

  * Calcula o Total de Parcelas
  QPARC = 0
  FOR i = 1 TO NumPag
    IF APePag(i) > 0 
      QPARC = QPARC + 1
    ENDIF
  NEXT i     

  * ValorNota = GTOT + GIPI
  ValorNota = GTOT 
  DPAR = AcumDifDupl( ValorNota, MPE1, MPE2, MPE3, MPE4 )

  * Transfere para um array os dias de vencimento especial
  FOR i = 1 TO NVencEsp
    AVencEsp(i) = VAL(SUBSTR(MVES, i + ((i * 2) - 2), 2))
  NEXT i

  * Procura pelo ultimo de dia de vencimento especial
  FOR i = 1 TO NVencEsp
    IF AVencEsp(i) > 0
      MAIOR_DIA = AVencEsp(i)
    ENDIF
  NEXT i

  DIA = SPACE(2)
  MES = SPACE(2)

  * Zera as datas de vencimento e valores de duplicatas
  FOR i = 1 TO NumPag
    AVencDup(i) = SPACE(10)
    AValDup(i) = 0
  NEXT i

  FOR NPAG = 1 TO QPARC
    CPA = STR(NPAG,2)
    VENC = INICIO + ACondPag(NPAG)
    MDIA = DAY(VENC)
    MMES = MONTH(VENC)
    AValDup(NPAG) = INT(ValorNota * (APePag(NPAG) / 100))
    IF NPAG = 1
      AValDup(NPAG) = AValDup(NPAG) + DPAR
      IF pedic2->cpd = 'ANT' .OR. pedic2->cpd = 'VIS'
        AVencDup(NPAG) = DTOC(HOJE)
      ELSE
        AVencDup(NPAG) = DTOC(VENC)
      ENDIF
    ELSE
      IF pedic2->cpd = 'ANT'
        AVencDup(NPAG) = 'ANTECIPADO'
      ELSE
        AVencDup(NPAG) = DTOC(VENC)
      ENDIF
    ENDIF
  NEXT I

  VDUP1   = AValDup(1)
  VDUP2   = AValDup(2) 
  VDUP3   = AValDup(3)
  VDUP4   = AValDup(4)
  VENDUP1 = AVencDup(1)
  VENDUP2 = AVencDup(2)
  VENDUP3 = AVencDup(3)
  VENDUP4 = AVencDup(4)
return

***************
PROCEDURE MAIOR
***************
  PRIVATE i, c
  
  IF MAIOR_DIA > 0
     * Se o dia de Vencimento > que o maior dia venc.especial
     IF MDIA > MAIOR_DIA
        DIF  = NDIAMES(venc)
        VENC = VENC + ((DIF + AVencEsp(1)) - MDIA)
     ELSE
        FOR i = 1 TO 6
           IF AVencEsp(i) >= MDIA
              VENC = VENC + (AVencEsp(i) - MDIA)
              FOR c = i+1 TO 6
                 AVencEsp(c) = 0
              NEXT c
              EXIT  
           ENDIF   
        NEXT i
     ENDIF
  ENDIF
RETURN

********************
PROCEDURE USEWINDOWS
********************
  PRIVATE cTela, Li, Ci, Lf, Cf, cCor, cMens, nTam
  
  cMens = 'Use a versao WINDOWS deste Modulo' 
  Li = 20
  Lf = Li + 2
  Ci = 20
  Cf = Ci + LEN(cMens) + 1
  nTam = Cf - Ci + 1
  cCor = SET('COLOR')
  SET COLOR TO &CORAVISO
  SAVE SCREEN TO cTela
  @Li, Ci CLEAR TO Lf, Cf 
  @Li, Ci TO Lf, Cf DOUBLE
  @Li+1, Ci+CentraTxt( cMens,nTam) SAY cMens 
  ?? CHR(07)
  =INKEY(0)
  SET COLOR TO &cCor
  RESTORE SCREEN FROM cTela

RETURN

****************
FUNCTION FMTREPR
****************
  PARAMETER cCVR, nIdRepr
  PRIVATE cTexto
  
  IF EMPTY(cCVR)
    cTexto = EmptyRepr
  ELSE  
    cTexto = cCVR+'.'+PADL(LTRIM(STR(nIdRepr)),2,'0')
  ENDIF  
  
RETURN cTexto  

******************
FUNCTION FMTREGIAO
******************
  PARAMETER cRegiao, nIdRegiao
  PRIVATE cTexto
  
  IF EMPTY(cRegiao)
    cTexto = EmptyRegiao
  ELSE  
    cTexto = cRegiao+'.'+PADL(LTRIM(STR(nIdRegiao)),2,'0')
  ENDIF  
  
RETURN cTexto  

*******************
FUNCTION EXTRACTVEN
*******************
PARAMETER MCVR

RETURN SUBSTR( MCVR, 1, 4 )
    
********************
FUNCTION EXTRACTREPR
********************
PARAMETER MCVR

RETURN VAL(SUBSTR( MCVR, 6, 2 ))


************************
PROCEDURE ATUALPEDABERTO
************************
  PRIVATE MMES, MANO, MDATA
  
  =MENSAG( 'Atualizando Pedidos em Aberto do Ultimo Mes...' )
  MMES = MONTH(DTLANC)
  MANO = YEAR(DTLANC)
  MFIM = CTOD('01/'+STR(MMES,2)+'/'+STR(MANO,4)) - 1
  MINICIO = CTOD('01/'+STR(MONTH(MFIM),2)+'/'+STR(YEAR(MFIM),4))
  MTOTAL = 0
  MIPI   = 0
  
  USE PEDIC ORDER P_PED IN 1
  USE PEDID IN 2
  
  SELECT PEDID
  DO WHILE ! EOF() 
    SELECT PEDIC 
    IF PEDID->SAL > 0 .AND. SEEK(PEDID->PED) .AND. PEDIC->DEM < MFIM THEN
      MVALOR = PEDID->QTD * PEDID->PRE
      MTOTAL = MTOTAL + MVALOR
      MIPI   = MIPI   + (MVALOR * (PEDID->IPI / 100))
    ENDIF  
    SELECT PEDID
    SKIP 
  ENDDO  
  SELECT PEDIC
  USE
  SELECT PEDID 
  USE

  *SELECT;
  *  SUM(QTD * PRE) AS TOTAL, SUM(QTD * PRE * IPI / 100) AS IPI;
  *FROM;
  * CLIEN CLI, PEDIC PED, PEDID IT;
  *WHERE;
  *  PED.DEM <= MFIM;
  *  AND IT.SAL > 0;
  *  AND CLI.CGC = PED.CGC 
  *  AND PED.PED = IT.PED;
  *INTO CURSOR TMP
  
  SELE PEDABERT 
  IF ! SEEK( MFIM )
    APPEND BLANK
    REPLACE DATA  WITH MFIM
  ENDIF
  REPLACE VALOR WITH MTOTAL
  REPLACE IPI   WITH MIPI
  SELECT PEDABERT
  USE
RETURN

*******************
FUNCTION GETPROXMES
*******************
PARAMETER dData
  PRIVATE Dia, Mes, Ano, NovoMes, NovaData
  
  Dia = DAY( dData )
  Mes = MONTH( dData )
  Ano = YEAR( dData )
  NovoMes = Mes + 1
  Mes = NovoMes % 12
  if Mes = 0 
    Mes = 12
  endif  
  if NovoMes > 12 
    Ano = Ano + 1
  endif
  
  NovaData = CTOD( STR( Dia, 2 ) + '/' + STR( Mes, 2 ) + '/' + Str(Ano,4) )

RETURN NovaData

********************
PROCEDURE ADICFILTRO
********************
PARAMETER cFiltro, cCond

  IF cFiltro = ''
    cFiltro = cCond
  ELSE
    cFiltro = cFiltro + ' AND ' + cCond
  ENDIF    

RETURN cFiltro

*************
PROCEDURE Y2K
*************
PARAMETER MDATA
  PRIVATE ANO, CDATA
 
RETURN MDATA 

***************
PROCEDURE PIVOT
***************
PARAMETER nAno
  
  nAnoNovo = 1900 + nAno 
  
  IF nAno <= AnoPivot
    nAnoNovo = 2000 + nAno
  ENDIF
  
RETURN nAnoNovo

 
*********************
PROCEDURE GETDADOSEMP
*********************
   USE DadosEmp
   
   MRazaoSoc = TRIM(RAZAOSOC)
   MCNPJ     = TRIM(CNPJ)
   MIEST	 = IESTADUAL
   MFONE1    = DDD1 + ' - ' + FONE1
   MFAX	     = DDD1 + ' - ' + FAX
   MENDER    = TRIM(ENDERECO)
   MBAIRRO   = TRIM(BAIRRO)
   MCEP      = CEP
   MCIDADE   = TRIM(CIDADE)
   MESTADO   = ESTADO
   
RETURN 

  
  
  
