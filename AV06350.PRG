* AV06350 - LISTA DE PEDIDOS EM ABERTO POR ORDEM DE CLIENTE
* SISTEMA ADMINISTRACAO DE VENDAS
* ADAO ELI PEREIRA - 24-04-90 - 06/12/91
*
DO TESTIMP2
IF CP = 'N'
  RETURN
ENDIF

MENS = 'Seleciona por Ordem de Pedido (P) ou Raz„o Social (R)? '
DO PEGRESP WITH [PR]
*
* PREPARACAO DO ARQUIVO COM PEDIDOS E DATAS
? MENSAG('Preparando Pedidos para emiss„o do Relat¢rio... Aguarde')
*
CLOS DATA
MCGC = SPACE(20)
MDEN = {}
MDEM = {}
MREF = SPACE(11)
MRAZ = SPACE(40)
MIPI = 0.0
MITE = 0
STOR 0    TO MQTD, MTAB, MPRI, MSAL, MATE, MPED
STOR 0.00 TO MPRE, MDIT, MDGE, MBON, MEMB, IVAL, TBON, TDIT, TDGE, TEMB, MEMB

SELE 0
USE CLIEN ORDER P_CGC
SELE 0
USE PEDPR
ZAP
SELE 0
USE PEDID ORDER P_PEDID
SELE 0
USE PEDIC ORDER P_PED
SET FILTER TO PED = 7783
GO TOP

DO WHILE .NOT. EOF()
  MPED = PED
  MCGC = CGC
  MDEM = DEM
  MDEN = DEN
  MCP1 = CP1
  MCPD = CPD
  SELE PEDID
  SEEK STR(MPED,6)
  DO WHILE PED = MPED .AND. !EOF()
    IF SAL > 0
      MREF = REF
      MQTD = QTD
      MSAL = SAL
      MTAB = TABREF
      MPRE = PRE
      MDIT = DIT
      MBON = BON
      MDGE = DESCGRU
      MIPI = IPI
      MITE = ITE
           
      SELE CLIEN
      SEEK MCGC
      IF !(EOF() .OR. BOF())
        MRAZ = RAZ
        MCID = CID
        MEST = EST
        MVEN = VEN
      ELSE
        MRAZ = 'Cliente nao cadastrado'
        MCID = ''
        MEST = ''
        MVEN = ''
      ENDIF
           
      SELE PEDPR
      APPE BLAN
      REPL PED WITH MPED
      REPL ITE WITH MITE,CGC WITH MCGC,DEM WITH MDEM,DEN WITH MDEN
      REPL REF WITH MREF,QTD WITH MQTD,TAB WITH MTAB,PRE WITH MPRE
      REPL PRI WITH MPRI,SAL WITH MSAL,DGE WITH MDGE,DIT WITH MDIT
      REPL EMB WITH MEMB,BON WITH MBON,RAZ WITH MRAZ,IPI WITH MIPI
      REPL CID WITH MCID,EST WITH MEST,VEN WITH MVEN
    ENDIF  
    SELE PEDID
    SKIP
  ENDDO
  SELE PEDIC
  SKIP
ENDDO
CLOS DATA

USE PEDPR
IF MC = 'P'
  DO MENSAG WITH  'Classificando pelo n£mero dos Pedidos... Aguarde'
  INDEX ON STR(PED,6)+STR(ITE,2) TO CADWORK.IDX
ELSE
  DO MENSAG WITH  'Classificando Pedidos pela Raz„o Social dos Clientes... Aguarde'
  INDEX ON RAZ+STR(PED,6)+STR(ITE,2) TO CADWORK.IDX
ENDIF
*
CLOS DATA
*
SELE 0
USE PEDIC INDEX PEDIC.IDX
SELE 0
USE PROD INDEX PROD.IDX
SELE 0
USE PEDPR INDEX CADWORK.IDX
SET RELA TO STR(PED,6) INTO PEDIC
*
PAGCTR = 0
TAMREL = 2
*
DO MENSAG WITH 'Imprimindo Relat¢rio Pedidos por Cliente... Aguarde'

DO DEFSAIDA

TQTD = 0
TVAL = 0
GVAL = 0
TSAL = 0
CVAL = 0
KEY = SPACE(20)
KEYP = 0
PIPI = 0.00
GIPI = 0.00
VIPI = 0.00

IF MC = 'P'
  TITULO = 'PEDIDOS EM ABERTO POR ORDEM DE NUMERO'
ELSE
  TITULO = 'PEDIDOS EM ABERTO POR RAZAO SOCIAL'
ENDIF
*
SELE PEDPR

DO WHILE !EOF()
*
  ? CABEC('AV06350',TITULO,130)
*
  @ 3,01  SAY 'CLIENT'
  @ 3,09  SAY ' PED'
  @ 3,15  SAY 'DT EMISS'
  @ 3,25  SAY 'DT ENTR'
  @ 3,34  SAY 'D.GRU'
  @ 3,40  SAY 'IT'
  @ 3,43  SAY 'REF'
  @ 3,48  SAY 'QUANTIDADE'
  @ 3,64  SAY 'SALDO'
  @ 3,70  SAY 'TAB'
  @ 3,76  SAY 'PR UNIT'
  @ 3,87  SAY 'D.ITE'
  @ 3,93  SAY 'BONUS'
  @ 3,103 SAY 'VALOR ITEM'
  @ 3,115 SAY 'VALOR PEDIDO'
  @ 4,1   SAY REPLICATE('-',130)
*
  KEYP = 0
  LIN  = 5
*
  DO WHILE !EOF() .AND. LIN < 62
    IF CGC <> KEY .OR. LIN = 5
      @ LIN,01 SAY RAZ + ' '+ 'Cidade: '+CID+' '+'Estado: '+EST+'   '+ ;
               'Vend: '+VEN+' C.Pag. '+str(pedic->cp1,3)+' '+pedic->cpd
      KEY = CGC
      LIN = LIN + 1
    ENDIF
*
    IF PED <> KEYP
      @ LIN,08 SAY PED pict '999999'
      @ LIN,15 SAY DEM
      @ LIN,24 SAY DEN
      @ LIN,33 SAY DGE PICT '99.99'
    ENDIF
    
    @ LIN,40 SAY ITE PICT '99'
    @ LIN,43 SAY REF
    @ LIN,48 SAY QTD PICT '99,999,999'
    @ LIN,59 SAY SAL PICT '99,999,999'
    @ LIN,70 SAY TAB
    @ LIN,76 SAY PRE PICT '999,999.99'
    @ LIN,87 SAY DIT PICT '99.99'
    @ LIN,93 SAY BON PICT '99.99'
    IVAL = (SAL * PRE) 
    VIPI = ((IVAL * IPI) / 100)
    @ LIN,100 SAY IVAL PICT '99,999,999.99'
    TVAL = IVAL + TVAL
    GVAL = IVAL + GVAL
    CVAL = IVAL + CVAL
    PIPI = (PIPI + VIPI)
    GIPI = (GIPI + VIPI)
    KEY = CGC
    KEYP = PED
    
    SKIP
    IF CGC <> KEY .OR. PED <> KEYP .OR. EOF()
      @ LIN,114 SAY TVAL PICT '99,999,999.99'
      TVAL = 0
    ENDIF
    
    IF CGC <> KEY .OR. EOF()
      LIN = LIN + 1
      @ LIN,095 SAY 'TOTAL LIQUIDO '
      @ LIN,113 SAY CVAL PICT '999,999,999.99'
      IF PIPI > 0
        LIN = LIN + 1
        @ LIN,095 SAY 'TOTAL COM IPI'
        @ LIN,113 SAY (CVAL + PIPI) PICT '999,999,999.99'
      ENDIF  
      LIN = LIN + 1
      @ LIN,01 SAY REPLICATE('-',130)
      CVAL = 0.00
      PIPI = 0.00
    ENDIF
      
    LIN = LIN + 1
    IF LIN > 62
       ? CABEC('AV06350',TITULO,130)
*
       @ 3,01  SAY 'CLIENT'
       @ 3,09  SAY ' PED'
       @ 3,15  SAY 'DT EMISS'
       @ 3,25  SAY 'DT ENTR'
       @ 3,34  SAY 'D.GRU'
       @ 3,40  SAY 'IT'
       @ 3,43  SAY 'REF'
       @ 3,48  SAY 'QUANTIDADE'
       @ 3,64  SAY 'SALDO'
       @ 3,70  SAY 'TAB'
       @ 3,76  SAY 'PR UNIT'
       @ 3,87  SAY 'D.ITE'
       @ 3,93  SAY 'BONUS'
       @ 3,103 SAY 'VALOR ITEM'
       @ 3,115 SAY 'VALOR PEDIDO'
       @ 4,1   SAY REPLICATE('-',130)
*
       KEYP = 0
       MCGC = SPACE(20)
       LIN  = 5
    ENDIF
    
    IF EOF()
      @ LIN,1 SAY REPLICATE('-',130)
        LIN = LIN + 1
      @ LIN,10 SAY "F I N A L    D E S T E    R E L A T O'R I O"
      @ LIN,095 SAY 'TOTAL LIQUIDO'
      @ LIN,111 SAY GVAL PICT '9,999,999,999.99'
        LIN = LIN + 1
      @ LIN,095 SAY 'TOTAL COM IPI'
      @ LIN,111 SAY (GVAL + GIPI) PICT '9,999,999,999.99'  
    ENDIF
  ENDDO
ENDDO

DO IMPSAIDA

USE PEDPR

IF FILE("CADWORK.IDX")
  ERASE CADWORK.IDX
ENDIF

RETURN
