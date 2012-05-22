* AV09100 - INDEXACAO DOS CADASTROS
* SISTEMA ADMINISTRACAO DE VENDAS
* ADAO ELI PEREIRA - 06/04/90
*
RUN ERASE *.IDX

*   E S T A D O S
DO MENSAG WITH 'Classificando os Estados da Federa‡„o... Aguarde'
  USE ESTADOS
    INDEX ON CEST TO ESTADOS COMPACT
*----------------------------------------------------------------------
*   O B S E R V A C O E S     P A R A     N O T A S    F I C A I S
DO MENSAG WITH 'Classificando Observa‡”es para Notas Fiscais... Aguarde'
  USE OBSNOTA
    INDEX ON CGC TO OBSNOTA COMPACT
*----------------------------------------------------------------------    
*   T R A N S P O R T A D O R A S
DO MENSAG WITH 'Classificando as Transportadoras... Aguarde'
  USE TRANS
    INDEX ON TRA TO TRANS  COMPACT
    INDEX ON RAZ TO TRARAZ COMPACT
    INDEX ON GUETRA TO TRAGUE COMPACT
*----------------------------------------------------------------------
*   V E N D E D O R E S
DO MENSAG WITH  'Classificando Vendedores... Aguarde'
  USE VEND
    INDEX ON CVR    TO VEND COMPACT
    INDEX ON VENGUE TO VENGUE COMPACT
    INDEX ON RAZ    TO VENRAZ COMPACT
*---------------------------------------------------------------------
*   C L I E N T E S
DO MENSAG WITH 'Classificando Clientes... Aguarde'
  USE CLIEN
    INDEX ON CGC TO CLIEN COMPACT
    INDEX ON GUE TO GUERRA COMPACT
    INDEX ON RAZ TO RAZAO COMPACT
    INDEX ON TRA TO CLITRANS COMPACT
    INDEX ON TRA_OPC TO CLITRAOP COMPACT
    
  USE CLI_ENTR
    INDEX ON CGC + CGCE TO CLI_ENTR COMPACT
    
  USE CLI_COBR
    INDEX ON CGCC TO CLI_COBR COMPACT
    
  USE CLI_MATR
    INDEX ON CGCM TO CLI_MATR COMPACT
*---------------------------------------------------------------------
*   M O E D A S
DO MENSAG WITH 'Classificando Moedas e Taxas de Convers„o... Aguarde'
  USE CADMOEDA
    INDEX ON STR(MOEDA,3)+DTOS(DATA) TO CADMOEDA COMPACT
*---------------------------------------------------------------------
*   B A N C O S
DO MENSAG WITH 'Classificando Bancos e Agˆncias... Aguarde'
  USE BANCOS
    INDEX ON STR(NBANCO,3)+STR(AGENCIA,4) TO BANCOS COMPACT
*---------------------------------------------------------------------
*   P R O D U T O S
DO MENSAG WITH  'Classificando Produtos... Aguarde'
  USE PROD
    INDEX ON REF TO PROD COMPACT
*---------------------------------------------------------------------
DO MENSAG WITH  'Classificando Tabela de Pre‡os dos Produtos... Aguarde'
  USE TABPRE
    INDEX ON GRUPO + ANO TO TABPRE COMPACT
*---------------------------------------------------------------------
*   SALDOS
DO MENSAG WITH 'Classificando Saldos do mes anterior... Aguarde'
  USE SALDOS
    INDEX ON REF TO SALDOS COMPACT
*---------------------------------------------------------------------
*   P E D I D O S    -    C A B E C A L H O
DO MENSAG WITH  'Classificando Pedidos... Aguarde'
  USE PEDIC
    INDEX ON STR(PED,6) TO PEDIC COMPACT
  USE PEDIC2
    INDEX ON STR(PED,6) TO PEDIC2 COMPACT
*----------------------------------------------------------------------
*   P E D I D O S    -    I T E N S
  USE PEDID
    INDEX ON STR(PED,6) + STR(ITE,2) TO PEDID COMPACT
    INDEX ON STR(PED,6) + REF TO PEDIDM COMPACT
    INDEX ON CGC + STR(PED,6) + REF TO PEDIDNF COMPACT
*----------------------------------------------------------------------
*   T I P O S   D E   N O T A S    F I S C A I S
DO MENSAG WITH  'Classificando Tipos de Notas Fiscais... Aguarde'
  USE TPNF
    INDEX ON TIPO TO TPNF COMPACT
*----------------------------------------------------------------------
* DUPLICATAS
DO MENSAG WITH  'Classificando Duplicatas... Aguarde'
  USE DUPL
    INDEX ON STR(NDUP,6) + STR(PDUP,1) TO DUPL COMPACT
    INDEX ON CGC + STR(NDUP,6) + STR(PDUP,1) TO DUPCGC COMPACT
*----------------------------------------------------------------------    
*   A P R O V A C A O   D E   P E D I D O S
  USE APROVP
    INDEX ON CGC + TIP + CCL TO APROVP COMPACT
*---------------------------------------------------------------------
*   N O T A S   F I S C A I S
DO MENSAG WITH 'Classificando Notas Fiscais... Aguarde'
  USE CADNF
    INDEX ON STR(NNF,6) + REF TO CADNF COMPACT
    INDEX ON STR(NNF,6) + REF TO NFREF COMPACT
    INDEX ON STR(PED,6) + STR(NNF,6) TO NFPED COMPACT
  USE CADNFD
    INDEX ON REF TO CADNFD COMPACT
  USE CABECNF
    INDEX ON STR(NNF,6) TO CABECNF  
*--------------------------------------------------------------------
*   R E P O R T E S   D E   P R O D U C A O
  USE REPORTE
    INDEX ON REF TO REPORTE COMPACT
*--------------------------------------------------------------------
* D A D O S   P A R A   E M I S S A O   D O   M O D E L O 3
DO MENSAG WITH  'Classificando dados para emiss„o do Modelo 3... Aguarde'
  USE MOD3
*    INDEX ON REF + SUBS(DATA,7,2) + '/' + SUBS(DATA,4,2) + '/' + SUBS(DATA,1,2)+ ;
*             TDOC + STR(NDOC,6) TO MOD3 COMPACT
* ------------------------------------------------------------------             
*    C O N C E I T O S   D E   C L I E N T E S
  USE CONCEITO
    INDEX ON CONCEITO TO CONCEITO COMPACT
*--------------------------------------------------------------------
*    S T A T U S   D E   C L I E N T E S
  USE STATUS
    INDEX ON STATUS TO STATUS COMPACT
*--------------------------------------------------------------------
*    A T I V I D A D E S    D E    C L I E N T E S
  USE ATIVIDAD
    INDEX ON ATIVIDADE TO ATIVIDAD COMPACT
* -------------------------------------------------------------------
*    R E G I O E S     D O S     C L I E N T E S
  USE REGIAO
    INDEX ON REGIAO TO REGIAO COMPACT
* ------------------------------------------------------------------        
*    S I G L A S     D O S     F U N C I O N A R I O S
  USE SIGLAS
    INDEX ON SIGLA TO SIGLAS COMPACT
* ------------------------------------------------------------------        
*    C A R G O S     D O S     V E N D E D O R E S / R E P R E S E N T
  USE CARGOS
    INDEX ON CARGO TO CARGOS COMPACT
* ------------------------------------------------------------------        
*    C O N D I C O E S    D E    P A G A M E N T O    C L I E N T E S
  USE CONDPAG
    INDEX ON STR(CONDCPAG,3) TO CONDPAG COMPACT
* ------------------------------------------------------------------        
*    S E N H A S     D E    A C E S S O
  USE CTR_SENH
    INDEX ON PROGRAMA + SENHA TO CTR_SENH COMPACT
* ------------------------------------------------------------------        
*    L I N H A S   D E   P R O D U T O S
  USE LIMPROD
    INDEX ON LPR TO LIMPROD COMPACT
* ------------------------------------------------------------------        
*    U N I D A D E S    D E    M E D I D A
  USE UNIDADES
    INDEX ON UNIDADE TO UNIDADES COMPACT
* ------------------------------------------------------------------        
*    E M B A L A G E N S
  USE EMBALAGE
    INDEX ON EMBALAGEM TO EMBALAGE COMPACT
* ------------------------------------------------------------------        
*    C L A S S I F I C A C A O    F I S C A L
  USE CLASFISC
    INDEX ON CLASFISC TO CLASFISC COMPACT
* ------------------------------------------------------------------        
*    G R U P O S    D E    P R O D U T O S     
  USE GRUPOS
    INDEX ON GRUPO TO GRUPOS COMPACT
* ------------------------------------------------------------------        
*    T A B E L A S    D E    P R E C O S    D E    P R O D U T O S     
  USE TABREF
    INDEX ON TABREF + REF TO TABREF COMPACT
* ------------------------------------------------------------------        
*    D E S C O N T O S     S E M A N A I S     
  USE TABSEM
    INDEX ON TABSEM + DIAS_DE TO TABSEM COMPACT
* ------------------------------------------------------------------        
*    C U S T O S    F I N A N C E I R O S     
  USE TABFIN
    INDEX ON TABFIN + DIAS_DE TO TABFIN COMPACT
* ------------------------------------------------------------------        
? mensag('Classificando Vendas a Prazo - Em Parcelas... Aguarde')
  USE VENDPARC
    INDEX ON TABPARC + STR(PRAZMED,4,1) TO VENDPARC COMPACT
* ------------------------------------------------------------------
*    C U S T O S    F I N A N C E I R O S     
  USE TABFIN2
    INDEX ON TABFIN + STR(PARCELAS,1) + STR(DIAS_DE,3) TO TABFIN2 COMPACT
* ------------------------------------------------------------------
*    VENDAS A PRAZO - PARCELA UNICA
  USE VENDUNIC
    INDEX ON TABUNIC + STR(QTDDIAS,2) TO VENDUNIC COMPACT
* ------------------------------------------------------------------
* CABECNF - PARA GUARDAR O VALOR BASE DO ICM
  USE CABECNF
    INDEX ON STR(NNF,6) TO CABECNF COMPACT
    
* ------------------------------------------------------------------        

CLOSE DATABAS
RETURN
