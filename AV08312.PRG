* AV08312 - CALCULA A POSICAO DE CLIENTES POR BANCO
* SISTEMA ADMINISTRACAO DE VENDAS
* ADAO ELI PEREIRA - 02/03/87
*
MODULO = PROGRAM()
TAMPAG = 65
NCOLS = 132
cPONT  = REPLICATE('-',NCOLS)
cPONTD = REPLICATE('=',NCOLS)

do TESTIMP2
IF CP = 'N'
  =FINALIZA()
  RETURN
ENDIF

@15,52 clear to 21,78
@15,52 to 21,78 double
@17,55 say "Data Inicial :"
@18,55 say "Data Final :"
stor {} to DTINI, DTFIM
@17,68 get DTINI 
@18,68 get DTFIM 
read

IF lastkey() = 27
  =FINALIZA()
  RETURN
ENDIF  

MB      = ' '
MNBANCO = 0
MENS = 'Seleciona Banco ?'
do PEGRESP WITH [SN]
MB = MC
if MB = 'S'
  use BANCOS INDEX BANCOS.IDX
  @19,55 say 'Informe o Banco 'get MNBANCO pict '999'
  read
else
  if empty(mc)
    =FINALIZA()
    return
  endif
endif

**--------- PREPARACAO do ARQUIVO COM DUPLICATAS E VENCIMENTOS ----------
=MENSAG('Classificando duplicatas para emissao do relatorio...Aguarde')
IF MB = 'N'
  lFiltro = '.T.'
ELSE
  lFiltro = 'NBANCO = MNBANCO'
ENDIF  
         
USE DUPL
COPY TO CADWORK;
     FOR (CTOD(VENC) >= DTINI .AND. CTOD(VENC) <= DTFIM) .AND.;
         EMPTY(CTOD(PAG)) .AND. EMPTY(DTCANCEL) .AND. &lFiltro
         

USE CADWORK
INDEX ON STR(NBANCO,3) + '/' + str(AGENCIA,4) +DTOS(CTOD(VENC)) + CGC TO POSFIN.IDX
CLOSE DATABASES
MCGC  = space(16)
MVENC = SPACE(8)
stor space(30) to MRAZ
stor 0 to MNBANCO, MAGENCIA, VALDEV, VALGER, VALDIA, PAGCTR
*stor 0 to VALBCO, TDESCTO, TVAL, TESTE, TOT_URV, TOT_URVB

* Totais acumulados do dia
nTNormal = 0    
nTDescto = 0
nTGarant = 0

* Totais acumulados do Banco
nTBNormal = 0
nTBDescto = 0
nTBGarant = 0

* Totais acumulados da Agencia
nTANormal = 0
nTADescto = 0
nTAGarant = 0

* Totais acumulados Geral
nTGNormal = 0
nTGDescto = 0
nTGGarant = 0

nTotDev     = 0
nAcumGeral  = 0
STOR 2 to TAMREL
sele 0
use BANCOS INDEX BANCOS.IDX
sele 0
Use CLIEN ORDER P_CGC
sele 0
use CADWORK INDEX POSFIN.IDX
sele 0
use CADMOEDA INDEX CADMOEDA.IDX
=MENSAG('Imprimindo Posicao de Clientes por Banco... Aguarde')
do DEFSAIDA
sele CADWORK
SET RELATION TO CGC INTO CLIEN
GO TOP                
=ImprCabec()
DO WHILE ! EOF()
  *---- Enquanto e o mesmo Banco 
  nTBNormal = 0
  nTBDescto = 0  
  nTBGarant = 0
  M->nBanco = CADWORK->NBANCO
  DO WHILE ! EOF() .AND. M->nBanco = CADWORK->NBANCO
    *--- Enquanto e a mesma Agencia
    nTANormal = 0
    nTADescto = 0  
    nTAGarant = 0
    =ImpCabBco()
    nAgencia = CADWORK->AGENCIA
    DO WHILE ! EOF() .AND. M->nBanco = CADWORK->NBANCO .AND. nAgencia = CADWORK->AGENCIA
      *---- Enquanto e o mesmo vencimento
      dVenc = CTOD(VENC)
      nTNormal = 0
      nTDescto = 0  
      nTGarant = 0
      nAcumDia = 0
      DO WHILE ! EOF() .AND. M->nBanco = CADWORK->NBANCO .AND. nAgencia = CADWORK->AGENCIA .AND. dVenc = CTOD(VENC)
        =ImprDupl()
        SKIP
      ENDDO
      *---- Imprime o Total do Dia
      =ImpTotDia()
      nTBNormal = nTBNormal + nTNormal
      nTBDescto = nTBDescto + nTDescto
      nTBGarant = nTBGarant + nTGarant
    ENDDO    
    *---- Imprime o Total da Agencia
    nTANormal = nTANormal + nTBNormal
    nTADescto = nTADescto + nTBDescto
    nTAGarant = nTAGarant + nTBGarant
  ENDDO
  *--- Imprime Total do Banco
  =ImpTotBco()
  nTGNormal = nTGNormal + nTANormal
  nTGDescto = nTGDescto + nTADescto
  nTGGarant = nTGGarant + nTAGarant
ENDDO
*---- Imprime o Total do Relatorio
=ImpTotal()
DO ImpSaida
=FINALIZA()
RETURN

*-------------------------------------------------------------------------- 
*                            ROTINAS AUXILIARES 
*--------------------------------------------------------------------------      

*================
FUNCTION IMPRDUPL
*================

  IF ! EOF('CLIEN')
    cCliente = clien->RAZ
  ELSE
    cCliente = '<<< Cliente Nao Encontrado >>>'
  ENDIF

  DuplDesc   = TIPOCART = 'D'
  DuplGarant = TIPOCART = 'G'
  DuplNormal = TIPOCART = 'N'

  ValorDia = VDP - DESC - VALDEV  
  IF DuplNormal 
    nTNormal = nTNormal + ValorDia
  ENDIF
  
  IF DuplDesc
    nTDescto = nTDescto + ValorDia
  ENDIF  

  IF DuplGarant 
    nTGarant = nTGarant + ValorDia
  ENDIF

  nAcumDia   = nAcumDia   + ValorDia
  nAcumGeral = nAcumGeral + ValorDia
  
  IF DESC > 0
    @lin,40 say 'VALOR DESCONTO---------'
    @lin,65 say DESC pict '9,999,999.99'
    =AddLin()
  ENDIF
  
  IF VALDEV > 0
    @lin,40 say 'VALOR DEVOLUCAO--------'
    @lin,65 say VALDEV pict '9,999,999.99'
    =AddLin()
  ENDIF  

  @lin,  00 say VENC
  @lin,  10 say cCliente
  @lin,  56 say NDUP 
  @lin,  67 say EMIS
  @lin,  77 say VDP       PICT '9,999,999.99'
  IF TIPOCART <> 'N'
    @Lin, 90 say TIPOCART
  ENDIF  
  @Lin, 100 say nAcumGeral  PICT '99,999,999.99'
  @Lin, 118 say nAcumDia    PICT '99,999,999.99'
  
  =AddLin()
  
RETURN

*=================
FUNCTION IMPTOTDIA
*=================  

  IF LIN + 3 >= TAMPAG
    =ImprCabec()
  ENDIF  
  @Lin+1,  00 SAY ' TOTAIS DO DIA:'
  @Lin+1, 100 say nAcumGeral  PICT '99,999,999.99'
  @Lin+1, 118 say nAcumDia PICT '99,999,999.99'
  @Lin+2,  00 SAY cPont
  Lin = Lin + 3
  @ Lin, 00
     
RETURN 

*=================
FUNCTION IMPTOTBCO
*=================  

  IF LIN + 5 >= TAMPAG
    =ImprCabec()
  ENDIF  
  
  nTotal = nTBNormal + nTBDescto + nTBGarant
  @lin+0,  81 say 'TOTAL do BANCO ----------->'
  @lin+0, 115 say nTBNormal pict  '99,999,999,999.99'
  @lin+1,  80 say 'DUPLICATAS DESCONTADAS----->'
  @lin+1, 115 say nTBDescto pict '99,999,999,999.99'
  @lin+2,  80 say 'DUPLICATAS EM GARANTIA ---->'
  @lin+2, 115 say nTBGarant pict '99,999,999,999.99'
  @lin+3,  85 say 'TOTAL LIQUIDO--------->'
  @lin+3, 115 say nTotal pict '99,999,999,999.99'
  *@Lin+4,  00 SAY cPont  
  Lin = Lin + 4
  @ Lin, 00
     
RETURN 

*================
FUNCTION IMPTOTAL
*================  
  PRIVATE Texto

  IF LIN + 6 >= TAMPAG
    =ImprCabec()
  ENDIF  

  nTotal = nTGNormal + nTGDescto + nTGGarant  
  Texto = 'F I N A L   D O   R E L A T O R I O'
  @Lin+0,  00 SAY cPontd    
  @lin+1,  86 say 'TOTAL GERAL BANCO --->'
  @lin+1, 115 say nTGNormal pict '99,999,999,999.99'
  @lin+2,  80 say 'DUPLICATAS DESCONTADAS----->'
  @lin+2, 115 say nTGDescto pict '99,999,999,999.99'
  @lin+3,  80 say 'DUPLICATAS EM GARANTIA ---->'
  @lin+3, 115 say nTGGarant pict '99,999,999,999.99'
  @lin+4,  85 say 'TOTAL LIQUIDO--------->'
  @lin+4, 115 say nTotal pict '99,999,999,999.99'
  @Lin+5,  00 SAY cPontd    
  @Lin+6, 00+CentraTxt(Texto,NCOLS) SAY Texto 
  Lin = Lin + 7
  @Lin, 00
  
RETURN 

*================
FUNCTION FINALIZA
*================
  CLOSE DATA
  *ERASE CADWORK.DBF
  *ERASE POSFIN.IDX
RETURN

*=================
FUNCTION IMPRCABEC
*=================
   PRIVATE TITULO
   
   TITULO = 'POSICAO DE CLIENTES POR BANCO - DE '+DTOC(DTINI)+' ATE '+DTOC(DTFIM)
   
   =CABEC(MODULO,TITULO,NCOLS)
   @03,00 say 'VENCIMEN RAZAO SOCIAL do CLIENTE                     DUPLICATA    EMISSAO       VAL EM R$         VALOR ACUMULADO   ACUMUL do DIA  '
   Lin = 4
   @ Lin, 000
  
RETURN

*=================
FUNCTION IMPCABBCO
*=================
   PRIVATE cBanco
   
   MNBANCO  = NBANCO
   MAGENCIA = AGENCIA
   
   if SEEK(str(MNBANCO,3) + str(MAGENCIA,4),'BANCOS')
     cBanco = BANCOS->DBANCO
   else
     cBanco = 'BANCO NAO CADASTRADO'
   endif
   @Lin+0,00 say cPontd
   @Lin+1,01 say 'BANCO: ' + PADL(MNBANCO,3,'0') +'/'+ PADL(MAGENCIA,4,'0') + ' - ' + cBanco
   @Lin+2,00 say cPont
   Lin = Lin + 3
   @ Lin, 00
  
RETURN

*==============
FUNCTION ADDLIN
*==============

  LIN = LIN + 1
  IF LIN >= TAMPAG
    =ImprCabec()
  ENDIF
RETURN    
  

* CENTROFIX Ind.Com. Ltda      AV08311     POSICAO DE CLIENTES POR BANCO - DE 01/01/97 ATE 31/12/97              01/09/97    PAG   1
* ------------------------------------------------------------------------------------------------------------------------------------
* VENCIMEN RAZAO SOCIAL do CLIENTE          DUPLICATA   EMISSAO       VAL EM R$         VAL EM URV   VALOR ACUMULADO   ACUMUL do DIA
*
*------------------------------------------------------------------------------------------------------------------------------------
* 237/2281 - BANCO BRADESCO S.A.           
*------------------------------------------------------------------------------------------------------------------------------------
*06/03/97  V E V - REPRESENTACOES LTDA.        1885     06/02/97        116,00                              116,00
*
* TOTAIS DO DIA:                                                        116,00
*------------------------------------------------------------------------------------------------------------------------------------
