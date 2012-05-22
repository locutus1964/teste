* AV06350 - LISTA DE PEDIDOS EM ABERTO POR ORDEM DE CLIENTE
* SISTEMA ADMINISTRACAO DE VENDAS
* ADAO ELI PEREIRA - 24-04-90 - 06/12/91
*
DO TESTIMP2
IF CP = 'N'
  RETURN
ENDIF
MODULO = PROGRAM()
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

IF MC = 'P'
  DO MENSAG WITH  'Classificando pelo n£mero dos Pedidos... Aguarde'
  cOrdem = 'IT.PED, IT.ITE'
ELSE
  DO MENSAG WITH  'Classificando Pedidos pela Raz„o Social dos Clientes... Aguarde'
  cOrdem = 'CLI.RAZ, IT.PED, IT.ITE' 
ENDIF

SELECT;
   IT.PED, IT.ITE, P.TIPO, PED.CGC, CLI.RAZ, CLI.CID, CLI.EST, DEM, DEN, IT.REF, QTD, TAB, PRE,;
   IT.SAL, DGE, DIT, EMB, BON, IT.IPI, PED.VEN, PED.IDREPR, TIPO.IPIT;
 FROM;
   PEDIC PED, PEDID IT, CLIEN CLI, PROD P, TPNF TIPO;
 WHERE;
   PED.PED = IT.PED AND PED.CGC = CLI.CGC AND IT.SAL > 0;
   AND IT.REF = P.REF AND PED.TIP = TIPO.TIPO;
 ORDER BY &cOrdem;  
 INTO TABLE TMP  
CLOS DATA

SELE 0
USE PEDIC ORDER P_PED
SELE 0
USE PROD INDEX PROD.IDX
SELE 0
USE TMP
SET RELATION TO PED INTO PEDIC
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
SELE TMP

DO WHILE !EOF()
*
  ? CABEC(MODULO,TITULO,130)
*
  @ 3,01  SAY 'CLIENT'
  @ 3,09  SAY ' PED'
  @ 3,15  SAY 'DT EMISS'
  @ 3,25  SAY 'DT ENTR'
  @ 3,34  SAY 'D.GRU'
  @ 3,40  SAY 'IT'
  @ 3,43  SAY 'REF'
  @ 3,48  SAY 'TIPO' 
  @ 3,62  SAY 'QTD.'
  @ 3,68  SAY 'SALDO'
  @ 3,74  SAY 'TAB'
  @ 3,82  SAY 'PR UNIT'
  @ 3,90  SAY 'D.ITE'
  @ 3,96  SAY 'BONUS'
  @ 3,106 SAY 'VALOR ITEM'
  @ 3,119 SAY 'VALOR PEDIDO'
  @ 4,1   SAY REPLICATE('-',130)
*
  KEYP = 0
  LIN  = 5
*
  DO WHILE !EOF() .AND. LIN < 62
    IF CGC <> KEY .OR. LIN = 5
      @ LIN,01 SAY RAZ + ' '+ 'Cidade: '+CID+' '+'Estado: '+EST+'   '+ ;
               'Vend: '+FmtRepr(VEN,IDREPR)+' C.Pag. '+str(pedic->cp1,3)+' '+pedic->cpd
      KEY = CGC
      LIN = LIN + 1
    ENDIF
*
    IF PED <> KEYP
      @ LIN,08 SAY PED pict '999999'
      @ LIN,15 SAY DEM
      @ LIN,26 SAY DEN
      @ LIN,37 SAY DGE PICT '99.99'
    ENDIF
    
    @ LIN,44 SAY ITE PICT '99'
    @ LIN,47 SAY REF
    @ LIN,52 SAY TIPO
    @ LIN,64 SAY QTD PICT '99,999'
    @ LIN,71 SAY SAL PICT '99,999'
    @ LIN,78 SAY TAB
    @ LIN,84 SAY PRE PICT '99,999.99'
    @ LIN,94 SAY DIT PICT '99.99'
    @ LIN,100 SAY BON PICT '99.99'
    IVAL = (SAL * PRE)
    IF TMP.IPIT = '1' .OR. TMP.IPIT = '3'
      ALIQIPI = IPI
    ELSE  
      ALIQIPI = 0.0
    ENDIF  
    VIPI = ((IVAL * ALIQIPI) / 100)
    @ LIN,110 SAY IVAL PICT '999,999.99'
    TVAL = IVAL + TVAL
    GVAL = IVAL + GVAL
    CVAL = IVAL + CVAL
    PIPI = (PIPI + VIPI)
    GIPI = (GIPI + VIPI)
    TSAL = TSAL + SAL
    KEY = CGC
    KEYP = PED
    
    SKIP
    IF CGC <> KEY .OR. PED <> KEYP .OR. EOF()
      @ LIN,125 SAY TVAL PICT '999,999.99'
      TVAL = 0
    ENDIF
    
    IF CGC <> KEY .OR. EOF()
      LIN = LIN + 1
      @ LIN,099 SAY 'TOTAL LIQUIDO '
      @ LIN,121 SAY CVAL PICT '999,999,999.99'
      IF PIPI > 0
        LIN = LIN + 1
        @ LIN,099 SAY 'TOTAL COM IPI'
        @ LIN,121 SAY (CVAL + PIPI) PICT '999,999,999.99'
      ENDIF  
      LIN = LIN + 1
      @ LIN,01 SAY REPLICATE('-',130)
      CVAL = 0.00
      PIPI = 0.00
    ENDIF
      
    LIN = LIN + 1
    IF LIN > 62
       ? CABEC(modulo,TITULO,130)
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
      @ LIN+0,1 SAY REPLICATE('-',130)
      @ LIN+1,39 SAY 'TOTAL SALDO: ' + TRANSFORM( TSAL, [99,999] )
      @ LIN+1,095 SAY 'TOTAL LIQUIDO'
      @ LIN+1,115 SAY GVAL PICT '9,999,999,999.99'
      @ LIN+2,095 SAY 'TOTAL COM IPI'
      @ LIN+2,115 SAY (GVAL + GIPI) PICT '9,999,999,999.99'  
      @ LIN+3,10 SAY "F I N A L    D E S T E    R E L A T O'R I O"      
    ENDIF
  ENDDO
ENDDO

DO IMPSAIDA
close data

RETURN
