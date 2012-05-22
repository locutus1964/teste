* AV08313 - CALCULA A POSICAO DE CLIENTES POR BANCO
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
stor CTOD('') to DTINI, DTFIM
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
=MENSAG('Selecionando duplicatas para emissao do relatorio...Aguarde')
IF MB = 'N'
  cFiltro = ""
ELSE
  cFiltro = "AND DUP.NBANCO = MNBANCO"
ENDIF  

*-------- Seleciona as duplicatas 
 SELECT;
   CLI.CGC,  CLI.RAZ,;
   DUP.VENC, DUP.NDUP, DUP.PDUP, DUP.EMIS, DUP.VDP, DUP.NBANCO, DUP.AGENCIA, DUP.TIPOCART,;
   DUP.DESC, DUP.VALDEV;
 FROM;
   CLIEN CLI ,CABECNF NF, DUPL DUP;
 WHERE;
   (DUP.VENC BETWEEN DTINI AND DTFIM AND EMPTY(DUP.PAG));
   AND NF.CANCELADA = .F.;
   AND (CLI.CGC = NF.CGC AND NF.NNF = DUP.NDUP);
   &cFiltro;
 INTO TABLE TMP  

CLOSE DATABASE

*=MENSAG('Classificando duplicatas para emissao do relatorio...Aguarde')  
*USE DUPL
*COPY TO CADWORK;
*     FOR (VENC >= DTINI .AND. VENC <= DTFIM) .AND.;
*         EMPTY(PAG) .AND. EMPTY(DTCANCEL) .AND. &lFiltro
*USE CADWORK

USE TMP
INDEX ON STR(NBANCO,3) + '/' + str(AGENCIA,4) +DTOS(VENC) + CGC TAG ORDEM

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
nTJurid  = 0
nTPerda  = 0

* Totais acumulados do Banco
nTBNormal = 0
nTBDescto = 0
nTBGarant = 0
nTBJurid  = 0
nTBPerda   = 0

* Totais acumulados da Agencia
nTANormal = 0
nTADescto = 0
nTAGarant = 0
nTAJurid  = 0
nTAPerda  = 0

* Totais acumulados Geral
nTGNormal = 0
nTGDescto = 0
nTGGarant = 0
nTGJurid  = 0
nTGPerda  = 0

nTotDev     = 0
nAcumGeral  = 0
STOR 2 to TAMREL
sele 0
use BANCOS INDEX BANCOS.IDX
sele 0
use TMP ORDER ORDEM
sele 0
use CADMOEDA INDEX CADMOEDA.IDX
=MENSAG('Imprimindo Posicao de Clientes por Banco... Aguarde')
do DEFSAIDA
sele TMP
GO TOP                

=ImprCabec()
DO WHILE ! EOF()
  *---- Enquanto e o mesmo Banco 
  nTBNormal = 0
  nTBDescto = 0  
  nTBGarant = 0
  nTBJurid  = 0
  nTBPerda  = 0
  M->nBanco = TMP->NBANCO
  DO WHILE ! EOF() .AND. M->nBanco = TMP->NBANCO
    *--- Enquanto e a mesma Agencia
    nTANormal = 0
    nTADescto = 0  
    nTAGarant = 0
    nTAJurid  = 0
    nTAPerda  = 0
    =ImpCabBco()
    nAgencia = TMP->AGENCIA
    DO WHILE ! EOF() .AND. M->nBanco = TMP->NBANCO .AND. nAgencia = TMP->AGENCIA
      *---- Enquanto e o mesmo vencimento
      dVenc = VENC
      nTNormal = 0
      nTDescto = 0  
      nTGarant = 0
      nTJurid  = 0
      nTPerda  = 0
      nAcumDia = 0
      DO WHILE ! EOF() .AND. M->nBanco = TMP->NBANCO .AND. nAgencia = TMP->AGENCIA .AND. dVenc = VENC
        =ImprDupl()
        SKIP
      ENDDO
      *---- Imprime o Total do Dia
      =ImpTotDia()
      nTBNormal = nTBNormal + nTNormal
      nTBDescto = nTBDescto + nTDescto
      nTBGarant = nTBGarant + nTGarant
      nTBJurid  = nTBJurid  + nTJurid
      nTBPerda  = nTBPerda  + nTPerda
    ENDDO    
    *---- Imprime o Total da Agencia
    nTANormal = nTANormal + nTBNormal
    nTADescto = nTADescto + nTBDescto
    nTAGarant = nTAGarant + nTBGarant
    nTAJurid  = nTAJurid  + nTBJurid
    nTAPerda  = nTAPerda  + nTBPerda
  ENDDO
  *--- Imprime Total do Banco
  =ImpTotBco()
  nTGNormal = nTGNormal + nTANormal
  nTGDescto = nTGDescto + nTADescto
  nTGGarant = nTGGarant + nTAGarant
  nTGJurid  = nTGJurid  + nTAJurid
  nTGPerda  = nTGPerda  + nTAPerda
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

  cCliente = TMP->RAZ

  DuplCart   = TIPOCART = 'CT'  && Carteira  
  DuplDesc   = TIPOCART = 'DE'  && Descontada
  DuplGarant = TIPOCART = 'GA'  && Garantia
  DuplNormal = TIPOCART = 'CS'  && Normal
  DuplJurid  = TIPOCART = 'JD'  && Juridico
  DuplPerda  = TIPOCART = 'PL'  && Perdas e Lucros

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

  IF DuplJurid
    nTJurid = nTJurid + ValorDia
  ENDIF

  IF DuplPerda
    nTPerda = nTPerda + ValorDia
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
  @lin,  11 say cCliente
  @lin,  57 say NDUP 
  @lin,  67 say EMIS
  @lin,  77 say VDP       PICT '9,999,999.99'
  IF ! DuplNormal
    @Lin, 91 say TIPOCART
  ENDIF  
  @Lin, 101 say nAcumGeral  PICT '99,999,999.99'
  @Lin, 119 say nAcumDia    PICT '99,999,999.99'
  
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
  PRIVATE nTotal
  
  IF LIN + 5 >= TAMPAG
    =ImprCabec()
  ENDIF  
  
  nTotal = nTBNormal + nTBDescto + nTBGarant + nTBJurid + nTBPerda
  @lin+0,  80 say 'TOTAL do BANCO ------------>'
  @lin+0, 109 say nTBNormal pict  '99,999,999,999.99'
  @lin+0, 127 say ROUND(nTBNormal / nTotal * 100,1) PICT [999.9%]   
  @lin+1,  80 say 'DUPLICATAS DESCONTADAS ---->'
  @lin+1, 109 say nTBDescto pict '99,999,999,999.99'
  @lin+1, 127 say ROUND(nTBDescto / nTotal * 100,1) PICT [999.9%]   
  @lin+2,  80 say 'DUPLICATAS EM GARANTIA ---->'
  @lin+2, 109 say nTBGarant pict '99,999,999,999.99'
  @lin+2, 127 say ROUND(nTBGarant / nTotal * 100,1) PICT [999.9%]   
  @lin+3,  80 say 'DUPLICATAS NO JURIDICO ---->'
  @lin+3, 109 say nTBJurid pict '99,999,999,999.99'
  @lin+3, 127 say ROUND(nTBJurid / nTotal * 100,1) PICT [999.9%]   
  @lin+4,  80 say 'DUPLICATAS C/PERDA/LUCRO -->'
  @lin+4, 109 say nTBPerda pict '99,999,999,999.99'
  @lin+4, 127 say ROUND(nTBPerda / nTotal * 100,1) PICT [999.9%]   
  @lin+5,  80 say 'TOTAL LIQUIDO ------------->'
  @lin+5, 109 say nTotal pict '99,999,999,999.99'
  @lin+5, 127 say "100,0%"
  *@Lin+4,  00 SAY cPont  
  Lin = Lin + 6
  @ Lin, 00
     
RETURN 

*================
FUNCTION IMPTOTAL
*================  
  PRIVATE Texto, nTotal

  IF LIN + 6 >= TAMPAG
    =ImprCabec()
  ENDIF  

  nTotal = nTGNormal + nTGDescto + nTGGarant + nTGJurid + nTGPerda
  Texto = 'F I N A L   D O   R E L A T O R I O'
  @Lin+0,  00 SAY cPontd    
  @lin+1,  80 say 'TOTAL GERAL BANCO --------->'
  @lin+1, 109 say nTGNormal pict '99,999,999,999.99'
  @lin+1, 127 say ROUND(nTGNormal / nTotal * 100,1) PICT [999.9%] 
  @lin+2,  80 say 'DUPLICATAS DESCONTADAS ---->'
  @lin+2, 109 say nTGDescto pict '99,999,999,999.99'
  @lin+2, 127 say ROUND(nTGDescto / nTotal * 100,1) PICT [999.9%]   
  @lin+3,  80 say 'DUPLICATAS EM GARANTIA ---->'
  @lin+3, 109 say nTGGarant pict '99,999,999,999.99'
  @lin+3, 127 say ROUND(nTGGarant / nTotal * 100,1) PICT [999.9%]   
  @lin+4,  80 say 'DUPLICATAS NO JURIDICO ---->'
  @lin+4, 109 say nTGJurid  pict '99,999,999,999.99'
  @lin+4, 127 say ROUND(nTGJurid / nTotal * 100,1) PICT [999.9%]   
  @lin+5,  80 say 'DUPLICATAS C/PERDA/LUCRO -->'
  @lin+5, 109 say nTGPerda  pict '99,999,999,999.99'
  @lin+5, 127 say ROUND(nTGPerda / nTotal * 100,1) PICT [999.9%]   
  @lin+6,  80 say 'TOTAL LIQUIDO ------------->'
  @lin+6, 109 say nTotal    pict '99,999,999,999.99'
  @lin+6, 127 say "100,0%"
  @Lin+7,  00 SAY cPontd    
  @Lin+8,  00+CentraTxt(Texto,NCOLS) SAY Texto 
  Lin = Lin + 8
  @Lin, 00
  
RETURN 

*================
FUNCTION FINALIZA
*================
  CLOSE DATA
  *ERASE TMP.DBF
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
