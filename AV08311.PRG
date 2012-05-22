* AV08311 - CALCULA A POSICAO DE CLIENTES POR BANCO
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
  use BANCOS INDEX BANCOS
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
         EMPTY(PAG) .AND. EMPTY(DTCANCEL) .AND. &lFiltro
        

USE CADWORK
INDEX ON STR(NBANCO,3) + '/' + str(AGENCIA,4) +DTOS(CTOD(VENC)) + CGC TO POSFIN
CLOSE DATABASES
MCGC  = space(16)
MVENC = SPACE(8)
stor space(30) to MRAZ
stor 0 to MNBANCO, MAGENCIA, VALDEV, VALGER, VALDIA, PAGCTR
*stor 0 to VALBCO, TDESCTO, TVAL, TESTE, TOT_URV, TOT_URVB

nTotAcum  = 0
nTotDia   = 0
nTotBco   = 0
nTotAgenc = 0
nTotGeral = 0
nTotGarant = 0

nTotDesc  = 0
nBcoDesc  = 0
nTotDev   = 0

STOR 2 to TAMREL
sele 0
use BANCOS INDEX BANCOS
sele 0
Use CLIEN ORDER P_CGC
sele 0
use CADWORK INDEX POSFIN
sele 0
use CADMOEDA INDEX CADMOEDA
=MENSAG('Imprimindo Posicao de Clientes por Banco... Aguarde')
do DEFSAIDA
sele CADWORK
SET RELATION TO CGC INTO CLIEN
GO TOP                
=ImprCabec()
DO WHILE ! EOF()
  *---- Enquanto e o mesmo Banco 
  M->nBanco = CADWORK->NBANCO
  nTotBco  = 0
  DO WHILE ! EOF() .AND. M->nBanco = CADWORK->NBANCO
    *--- Enquanto e a mesma Agencia
    =ImpCabBco()
    nAgencia = CADWORK->AGENCIA
    nTotAgenc = 0
    DO WHILE ! EOF() .AND. M->nBanco = CADWORK->NBANCO .AND. nAgencia = CADWORK->AGENCIA
      *---- Enquanto e o mesmo dia
      dVenc = CTOD(VENC)
      nTotDia = 0
      DO WHILE ! EOF() .AND. M->nBanco = CADWORK->NBANCO .AND. nAgencia = CADWORK->AGENCIA .AND. dVenc = CTOD(VENC)
        =ImprDupl()
        SKIP
      ENDDO
      *---- Imprime o Total do Dia
      =ImpTotDia()
      nTotAgenc = nTotAgenc + nTotDia
    ENDDO    
    nTotBco  = nTotBco + nTotAgenc
    nTotDesc = nTotDesc + nBcoDesc
  ENDDO
  *--- Imprime Total do Banco
  =ImpTotBco()
  nTotGeral = nTotGeral + nTotBco
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

  nTotDia  = nTotDia + VDP
  nTotAcum = nTotAcum + VDP

  TemDescBco = TIPOCART = 'D'
  TemGara

  IF TemDescBco
    nBcoDesc = nBcoDesc + VDP
    nTotDia = nTotDia - VDP
    nTotAcum = nTotAcum - VDP
  ENDIF  
  

  IF DESC > 0
    =AddLin()
    @lin,40 say 'VALOR DESCONTO---------'
    @lin,65 say DESC pict '9,999,999.99'
    nTotDia = nTotDia - VALDEV
    nTotAcum = nTotAcum - VALDEV
  ENDIF
  
  IF VALDEV > 0
    =AddLin()
    @lin,40 say 'VALOR DEVOLUCAO--------'
    @lin,65 say VALDEV pict '9,999,999.99'
    nTotDia = nTotDia - VALDEV
    nTotAcum = nTotAcum - VALDEV
  ENDIF  

  @lin,  00 say VENC
  @lin,  10 say cCliente
  @lin,  56 say NDUP 
  @lin,  67 say EMIS
  @lin,  77 say VDP       PICT '9,999,999.99'
  IF TemDescBco
    @Lin, 90 say 'D'
  ENDIF  
  @Lin, 100 say nTotAcum  PICT '99,999,999.99'
  @Lin, 118 say nTotDia   PICT '99,999,999.99'
  
  =AddLin()
  
RETURN

*=================
FUNCTION IMPTOTDIA
*=================  

  IF LIN + 3 >= TAMPAG
    =ImprCabec()
  ENDIF  
  @Lin+1,  00 SAY ' TOTAIS DO DIA:'
  @Lin+1, 100 say nTotAcum  PICT '99,999,999.99'
  @Lin+1, 118 say nTotDia PICT '99,999,999.99'
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
  
  @lin+0,  81 say 'TOTAL do BANCO ----------->'
  @lin+0, 115 say nTotBco pict  '99,999,999,999.99'
  @lin+1,  80 say 'DUPLICATAS DESCONTADAS----->'
  @lin+1, 115 say nTotDesc pict '99,999,999,999.99'
  @lin+2,  85 say 'TOTAL LIQUIDO--------->'
  @lin+2, 115 say (nTotBco + nTotDesc) pict '99,999,999,999.99'
  *@Lin+4,  00 SAY cPont  
  Lin = Lin + 3
  @ Lin, 00
     
RETURN 

*================
FUNCTION IMPTOTAL
*================  
  PRIVATE Texto

  IF LIN + 6 >= TAMPAG
    =ImprCabec()
  ENDIF  
  
  Texto = 'F I N A L   D O   R E L A T O R I O'
  @Lin+0, 00 SAY cPontd    
  @lin+1, 86 say 'TOTAL GERAL --------->'
  *@lin+1, 86 say TOT_URV pict '9,999,999.99'
  @lin+1,115 say nTotGeral pict '99,999,999,999.99'
  @lin+2, 80 say 'DUPLICATAS DESCONTADAS----->'
  @lin+2,115 say nTotDesc pict '99,999,999,999.99'
  @lin+3, 85 say 'TOTAL LIQUIDO--------->'
  @lin+3,115 say (nTotGeral + nTotDesc) pict '99,999,999,999.99'
  @Lin+4, 00 SAY cPontd    
  @Lin+5, 00+CentraTxt(Texto,NCOLS) SAY Texto 
  Lin = Lin + 6
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
