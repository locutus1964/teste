* AV09100 - INDEXACAO DOS CADASTROS
* SISTEMA ADMINISTRACAO DE VENDAS
* ADAO ELI PEREIRA - 06/04/90
*
MENS = 'Excluindo os Indices anteriores... Aguarde'
do mensmeio with 'Excluindo'

* TIPOS DE PAGAMENTO

mens = 'Classificando Tipo de Pagamento... Aguarde'
do mensmeio with 'Classificando'
  ERASE TIPOPGTO.IDX
  ERASE DTIPPGTO.IDX
  USE TIPOPGTO
    INDEX ON TIPO      TO TIPOPGTO.IDX COMPACT
    INDEX ON DESCRICAO TO DTIPPGTO.IDX COMPACT
*----------------------------------------------------------------------

* SITUACAO DE PEDIDO

mens = 'Classificando Situacao de Pedidos... Aguarde'
do mensmeio with 'Classificando'
  ERASE SITPED.IDX
  ERASE DSITPED.IDX
  USE SITPED
    INDEX ON SITUACAO  TO SITPED.IDX COMPACT
    INDEX ON DESCRICAO TO DSITPED.IDX COMPACT
*----------------------------------------------------------------------

* TIPO DE CARTEIRA BANCARIA

mens = 'Classificando os Tipos de Carteira Bancaria... Aguarde'
do mensmeio with 'Classificando'
  USE TPCART
  REINDEX
  
*----------------------------------------------------------------------

*   E S T A D O S
mens = 'Classificando os Estados... Aguarde'
do mensmeio with 'Classificando'
  ERASE ESTADOS.IDX
  USE ESTADOS
    INDEX ON CEST TO ESTADOS.IDX COMPACT
*----------------------------------------------------------------------
*   O B S E R V A C O E S     P A R A     N O T A S    F I C A I S
MENS = 'Classificando Observa‡”es para Notas Fiscais... Aguarde'
do mensmeio with 'Classificando'
  ERASE OBSNOTA.IDX
  USE OBSNOTA
*    INDEX ON CGC TO OBSNOTA COMPACT
INDEX ON CGC + str(ped,6) TO OBSNOTA.IDX COMPACT
*----------------------------------------------------------------------
*   T R A N S P O R T A D O R A S
MENS = 'Classificando as Transportadoras... Aguarde'
do mensmeio with 'Classificando'
  ERASE TRANS.IDX
  ERASE TRARAZ.IDX
  ERASE TRAGUE.IDX
  USE TRANS
    INDEX ON TRA TO TRANS.IDX COMPACT
    INDEX ON RAZ TO TRARAZ.IDX COMPACT
    INDEX ON GUETRA TO TRAGUE.IDX COMPACT
*----------------------------------------------------------------------
*   V E N D E D O R E S
MENS =  'Classificando Vendedores... Aguarde'
do mensmeio with 'Classificando'
 ERASE VEND.IDX
 ERASE VENGUE.IDX
 ERASE VENRAZ.IDX
 USE VEND
   INDEX ON CVR    TO VEND.IDX COMPACT
   INDEX ON VENGUE TO VENGUE.IDX COMPACT
   INDEX ON RAZ    TO VENRAZ.IDX COMPACT
*---------------------------------------------------------------------
*   C L I E N T E S
MENS = 'Classificando Clientes... Aguarde'
do mensmeio with 'Classificando'
*  erase clien.idx
*  erase guerra.idx
*  erase razao.idx
*  erase clitrans.idx
*  erase clitraop.idx
*  erase clifon1.idx
*  ERASE CLIFON2.IDX
*  ERASE CLINCEL.IDX
*  USE CLIEN
*    INDEX ON CGC               TO CLIEN.IDX    COMPACT
*    INDEX ON GUE               TO GUERRA.IDX   COMPACT
*    INDEX ON RAZ               TO RAZAO.IDX    COMPACT
*    INDEX ON TRA               TO CLITRANS.IDX COMPACT
*    INDEX ON TRA_OPC           TO CLITRAOP.IDX COMPACT
*    INDEX ON ALLTRIM(FON1)     TO CLIFON1.IDX  COMPACT
*    INDEX ON ALLTRIM(FON2)     TO CLIFON2.IDX  COMPACT
*    INDEX ON ALLTRIM(NUM_CELU) TO CLINCEL.IDX  COMPACT
  erase cli_entr.idx
  USE CLI_ENTR
    INDEX ON CGC + CGCE TO CLI_ENTR.IDX COMPACT

  erase cli_cobr.idx
  USE CLI_COBR
    INDEX ON CGCC TO CLI_COBR.IDX COMPACT
  
  ERASE CLI_MATR.IDX
  USE CLI_MATR
    INDEX ON CGCM TO CLI_MATR.IDX COMPACT
*---------------------------------------------------------------------
* MOEDAS
MENS = 'Classificando Convers„o de Moedas... Aguarde'
do mensmeio with 'Classificando'
  ERASE CADMOEDA.IDX
  use CADMOEDA
  INDEX ON STR(MOEDA,3)+DTOS(DATA) TO CADMOEDA.IDX COMPACT
  ERASE NOMMOEDA.IDX           
  use NOMMOEDA
  INDEX ON STR(MOEDA,3) TO NOMMOEDA.IDX COMPACT
*---------------------------------------------------------------------
*   B A N C O S
MENS = 'Classificando Bancos e Agˆncias... Aguarde'
do mensmeio with 'Classificando'
  ERASE BANCOS.IDX
  USE BANCOS
    INDEX ON STR(NBANCO,3)+STR(AGENCIA,4) TO BANCOS.IDX COMPACT
*---------------------------------------------------------------------
*   P R O D U T O S
MENS =  'Classificando Produtos... Aguarde'
do mensmeio with 'Classificando'
  ERASE PROD.IDX
  USE PROD
    INDEX ON REF TO PROD.IDX COMPACT
*---------------------------------------------------------------------
MENS =  'Classificando Tabela de Pre‡os dos Produtos... Aguarde'
do mensmeio with 'Classificando'
  ERASE TABPRE.IDX
  USE TABPRE
    INDEX ON GRUPO + ANO TO TABPRE.IDX COMPACT
    INDEX ON GRUPO + ANO TAG XGRUPOANO
*---------------------------------------------------------------------
*   SALDOS
MENS = 'Classificando Saldos do mes anterior... Aguarde'
do mensmeio with 'Classificando'
  ERASE SALDOS.IDX
  USE SALDOS
    INDEX ON REF TO SALDOS.IDX COMPACT
*---------------------------------------------------------------------
*   P E D I D O S    -    C A B E C A L H O
MENS =  'Classificando Cabe‡alhos dos Pedidos... Aguarde'
do mensmeio with 'Classificando'
*  ERASE PEDIC.IDX
  ERASE PEDIC2.IDX  
*  USE PEDIC
*    INDEX ON STR(PED,6) TO PEDIC.IDX COMPACT

  USE PEDIC2
    INDEX ON STR(PED,6) TO PEDIC2.IDX COMPACT
*----------------------------------------------------------------------
*   P E D I D O S    -    I T E N S
MENS =  'Classificando Itens dos Pedidos... Aguarde'
do mensmeio with 'Classificando'
*  ERASE PEDID.IDX
*  ERASE PEDIDM.IDX
*  ERASE PEDIDNF.IDX
  USE PEDID
    PACK
*    INDEX ON STR(PED,6) + STR(ITE,2) TO PEDID.IDX COMPACT
*    INDEX ON STR(PED,6) + REF TO PEDIDM.IDX COMPACT
*    INDEX ON CGC + STR(PED,6) + REF TO PEDIDNF.IDX COMPACT
*   A P R O V A C A O   D E   P E D I D O S
  ERASE APROVP.IDX
  USE APROVP
    INDEX ON CGC + TIP + CCL TO APROVP.IDX COMPACT
*----------------------------------------------------------------------
*   T I P O S   D E   N O T A S    F I S C A I S
MENS =  'Classificando Tipos de Notas Fiscais... Aguarde'
do mensmeio with 'Classificando'
  ERASE TPNF.IDX
  USE TPNF
    INDEX ON TIPO TO TPNF.IDX COMPACT
*----------------------------------------------------------------------
* DUPLICATAS
MENS =  'Classificando Duplicatas... Aguarde'
do mensmeio with 'Classificando'
  ERASE DUPL.IDX
  ERASE DUPCGC.IDX
  
  USE DUPL
*    INDEX ON STR(NDUP,6) + STR(PDUP,1) + DTOS(EMIS)       TO DUPL.IDX COMPACT
*    INDEX ON CGC + STR(NDUP,6) + STR(PDUP,1) + DTOS(EMIS) TO DUPCGC.IDX COMPACT
*---------------------------------------------------------------------
*   N O T A S   F I S C A I S
MENS = 'Classificando Notas Fiscais... Aguarde'
do mensmeio with 'Classificando'
*  ERASE CADNF.IDX
*  ERASE NFREF.IDX
*  ERASE NFPED.IDX
*  ERASE NUMNF.IDX
*  USE CADNF
*    INDEX ON STR(NNF,6) + REF        TO CADNF.IDX COMPACT
*    INDEX ON STR(NNF,6) + REF        TO NFREF.IDX COMPACT
*    INDEX ON STR(PED,6) + STR(NNF,6) TO NFPED.IDX COMPACT
*    INDEX ON NNF                     TO NUMNF.IDX COMPACT
   
*  ERASE CABECNF.IDX
*   ERASE CABNFDT.IDX 
*   USE CABECNF
*     INDEX ON STR(NNF,6) TO CABECNF.IDX
*     INDEX ON STR(NNF,6)+STR(YEAR(DEMISSAO),2) TO CABNFDT.IDX

*---------------------------------------------------------------------
*   N O T A S   F I S C A I S  -  Entradas
MENS = 'Classificando Notas Fiscais de Entrada... Aguarde'
do mensmeio with 'Classificando'
  ERASE ENTR_CAB.IDX
  USE ENTR_CAB
     INDEX ON CGC + STR(NNF,6)  TO ENTR_CAB.IDX  COMPACT
  ERASE ENTR_ITE.IDX   
  USE ENTR_ITE
     INDEX ON CGC + STR(NNF,6)  TO ENTR_ITE.IDX  COMPACT

*--------------------------------------------------------------------
*   R E P O R T E S   D E   P R O D U C A O
mens = "Classificando os Report's de Produ‡„o... Aguarde"
do mensmeio with 'Classificando'
  ERASE REPORTE.IDX
  USE REPORTE
    INDEX ON REF TO REPORTE.IDX COMPACT
*--------------------------------------------------------------------
* D A D O S   P A R A   E M I S S A O   D O   M O D E L O 3
MENS =  'Classificando dados para emiss„o do Modelo 3... Aguarde'
do mensmeio with 'Classificando'
*  ERASE MOD3.IDX
  USE MOD3
  PACK
* INDEX ON REF + SUBS(DATA,7,2) + '/' + SUBS(DATA,4,2) + '/' + SUBS(DATA,1,2)+ ;
*          TDOC + STR(NDOC,6) TO MOD3.IDX COMPACT
* ------------------------------------------------------------------
*    C O N C E I T O S   D E   C L I E N T E S
do mensmeio with 'Classificando'
  ERASE CONCEITO.IDX
  USE CONCEITO
    INDEX ON CONCEITO TO CONCEITO.IDX COMPACT
*--------------------------------------------------------------------
*    S T A T U S   D E   C L I E N T E S
mens = 'Classificando os Status de Clientes... Aguarde'
do mensmeio with 'Classificando'
  ERASE STATUS.IDX
  ERASE STATCOL.IDX
  USE STATUS
  INDEX ON STATUS TO STATUS.IDX  COMPACT
  INDEX ON COLU   TO STATCOL.IDX COMPACT
*--------------------------------------------------------------------
*    A T I V I D A D E S    D E    C L I E N T E S
mens = 'Classificando os Ramos de Atividade dos Clientes... Aguarde'
do mensmeio with 'Classificando'
  ERASE ATIVIDAD.IDX
  USE ATIVIDAD
    INDEX ON ATIVIDADE TO ATIVIDAD.IDX COMPACT
* -------------------------------------------------------------------
*    R E G I O E S     D O S     C L I E N T E S
mens = 'Classificando as Regi”es dos Clientes... Aguarde'
do mensmeio with 'Classificando'
  ERASE REGIAO.IDX
  USE REGIAO
    INDEX ON REGIAO TO REGIAO.IDX COMPACT
* ------------------------------------------------------------------
*    S I G L A S     D O S     F U N C I O N A R I O S
mens = 'Classificando as Siglas dos Funcion rios... Aguarde'
do mensmeio with 'Classificando'
  ERASE SIGLAS.IDX
  USE SIGLAS
    INDEX ON SIGLA TO SIGLAS.IDX COMPACT
* ------------------------------------------------------------------
*    C A R G O S     D O S     V E N D E D O R E S / R E P R E S E N T
mens = 'Classificando os Cargos dos Contatos nos Clientes... Aguarde'
do mensmeio with 'Classificando'
  ERASE CARGOS.IDX
  USE CARGOS
    INDEX ON CARGO TO CARGOS.IDX COMPACT
* ------------------------------------------------------------------
*    C O N D I C O E S    D E    P A G A M E N T O    C L I E N T E S
mens = 'Classificando as Condi‡”es de Pagamento... Aguarde'
do mensmeio with 'Classificando'
  ERASE CONDPAG.IDX
  USE CONDPAG
    INDEX ON STR(CONDCPAG,3) TO CONDPAG.IDX COMPACT
* ------------------------------------------------------------------
*    S E N H A S     D E    A C E S S O
mens = 'Classificando as Senhas de Acesso ao Sistema... Aguarde'
do mensmeio with 'Classificando'
  ERASE CTR_SENH.IDX
  USE CTR_SENH
    INDEX ON PROGRAMA + SENHA TO CTR_SENH.IDX COMPACT
* ------------------------------------------------------------------
*    L I N H A S   D E   P R O D U T O S
mens = 'Classificando as Linhas de Produtos... Aguarde'
do mensmeio with 'Classificando'
  ERASE LIMPROD.IDX
  USE LIMPROD
    INDEX ON LPR TO LIMPROD.IDX COMPACT
* ------------------------------------------------------------------
*    U N I D A D E S    D E    M E D I D A
mens = 'Classificando as Unidades de Medida... Aguarde'
do mensmeio with 'Classificando'
  ERASE UNIDADES.IDX
  USE UNIDADES
    INDEX ON UNIDADE TO UNIDADES.IDX COMPACT
* ------------------------------------------------------------------
*    E M B A L A G E N S
mens = 'Classificando os Tipos de Embalagem... Aguarde'
do mensmeio with 'Classificando'
  ERASE EMBALAGE.IDX
  USE EMBALAGE
    INDEX ON EMBALAGEM TO EMBALAGE.IDX COMPACT
* ------------------------------------------------------------------
*    C L A S S I F I C A C A O    F I S C A L
mens = 'Classificando os C¢digos de Classifica‡„o do IPI... Aguarde'
do mensmeio with 'Classificando'
  ERASE CLASFISC.IDX
  USE CLASFISC
    INDEX ON CLASFISC TO CLASFISC.IDX COMPACT
* ------------------------------------------------------------------
*    G R U P O S    D E    P R O D U T O S
mens = 'Classificando os Grupos de Produtos... Aguarde'
do mensmeio with 'Classificando'
  ERASE GRUPOS.IDX
  USE GRUPOS
    INDEX ON GRUPO TO GRUPOS.IDX COMPACT
* ------------------------------------------------------------------
*    T A B E L A S    D E    P R E C O S    D E    P R O D U T O S
mens = 'Classificando as Tabelas de Pre‡os de Produtos... Aguarde'
do mensmeio with 'Classificando'
  ERASE TABREF.IDX
  USE TABREF
    INDEX ON TABREF + REF TO TABREF.IDX COMPACT
* ------------------------------------------------------------------
*    D E S C O N T O S     S E M A N A I S
mens = 'Classificando os Descontos Semanais... Aguarde'
do mensmeio with 'Classificando'
  ERASE TABSEM.IDX
  USE TABSEM
    INDEX ON TABSEM + DIAS_DE TO TABSEM.IDX COMPACT
* ------------------------------------------------------------------
*    C U S T O S    F I N A N C E I R O S
mens = 'Classificando os Custos Financeiros... Aguarde'
do mensmeio with 'Classificando'
  ERASE TABFIN.IDX
  USE TABFIN
    INDEX ON TABFIN + DIAS_DE TO TABFIN.IDX COMPACT
* ------------------------------------------------------------------
*   V E I C U L O S
mens = 'Classificando os Ve¡culos utilizados nas entregas... Aguarde'
do mensmeio with 'Classificando'
  ERASE VEICULOS.IDX
  USE VEICULOS
    INDEX ON VEICULO TO VEICULOS.IDX COMPACT
* ------------------------------------------------------------------
*    C U S T O S    F I N A N C E I R O S
mens = 'Classificando Percentuais de Exclus„o do ICMS... Aguarde'
do mensmeio with 'Classificando'
  ERASE TABFIN2.IDX
  USE TABFIN2
    INDEX ON TABFIN + STR(PARCELAS,1) + STR(DIAS_DE,3) TO TABFIN2.IDX COMPACT
* ------------------------------------------------------------------
*    VENDAS A PRAZO - PARCELA UNICA
  ERASE VENDUNIC.IDX
  USE VENDUNIC
    INDEX ON TABUNIC + STR(QTDDIAS,2) TO VENDUNIC.IDX COMPACT
* ------------------------------------------------------------------
  ERASE VENDPARC.IDX
  USE VENDPARC
    INDEX ON TABPARC + STR(PRAZMED,4,1) TO VENDPARC.IDX COMPACT
* ------------------------------------------------------------------
mens = 'Classificando Raz„o de Clientes... Aguarde'
do mensmeio with 'Classificando'
  ERASE RAZAOCLI.IDX
  USE RAZAOCLI
  INDEX ON SUBS(COMPET,4,2)+'/'+SUBS(COMPET,1,2)+CGC TO RAZAOCLI.IDX
* ------------------------------------------------------------------
CLOSE DATABAS
SET TALK OFF
SAVE SCREEN TO T1
=MENSAG('Criando Indices Permanentes (CDX)...')
do criacdx with 1
=MENSAG('')
RESTORE SCREEN FROM T1
RETURN
