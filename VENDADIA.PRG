*:*********************************************************************
*: Procedure file: VENDADIA
*:         System: CPG
*:         Author: ADAO ELI PEREIRA
*:      Copyright (c) 1993, ADAO ELI PEREIRA
*:  Last modified: 10/04/94     15:51
*:*********************************************************************
do testimp2
if cp = 'N'
   return
endif
=janela(15,32,21,77)
=borda(15,32,21,77)
inicio = date()
fim    = date()
@17,34 say 'Informe DIA/MES/ANO para Inicio 'get inicio    pict '99/99/9999'
@19,34 say 'Informe DIA/MES/ANO para Final  ' get fim      pict '99/99/9999'
read
mens = dadcorr
do pegresp with [SN]
if mc = 'N' .or. empty(mc)
   return
endif
public mmvar
mccz   = 0
pagctr = 0
=mensag('Preparando os Arquivos... Aguarde')
use DUPL
copy to cadwork for emis >= inicio .and. emis <= fim ;
.and. EMPTY(dtcancel)
clos data
use cadwork
index on DTOS(emis) + str(nbanco,3) to cadwork
go top
PAGCTR  = 0
tamrel  = 2
MVDPC   = 0.00    && VENDA DIA CARTEIRA
MVDPB   = 0.00    &&           BANCO
DVDP    = 0.00
GVDPC   = 0.00    && VENDA GERAL CARTEIRA
GVDPB   = 0.00    &&             BANCO
ACUMUL  = 0.00    && CART + BANCOS
ACUMULC = 0.00    && ACUMULADO CARTEIRA
ACUMULB = 0.00    && ACUMULADO BANCO
ACUMULG = 0.00    && ACUMULADO GERAL
=mensag('Imprimindo D‚bitos IPI e ICM por data de emissao... Aguarde')
TIT = 'FATURAMENTO POR CARTEIRA E BANCOS DE '+INICIO+' ATE '+FIM
do defsaida
do while .not. eof()
    =cabec('VENDADIA',TIT,130)
   @3,01 say 'DATA FAT VALOR CARTEIRA ACUM. CARTEIRA  VALOR BANCOS   '+;
   'ACUM. BANCOS  CART + BANCOS  ACUM ATE DATA'
   @04,01 say replicate('-',130)
   lin = 6
   do while .not. eof() .and. lin < 62
      if nbanco = 2  &&---- Carteira
         mvdpc = (mvdpc + vdp)
         gvdpc = (gvdpc + vdp)
      else
         mvdpb = (mvdpb + vdp)
         gvdpb = (gvdpb + vdp)
      endif
      mdemi = emis
      skip
      if emis <> mdemi .or. eof()
         @lin,01 say mdemi
         @lin,10 say mvdpc              pict "99,999,999.99"
         @lin,25 say gvdpc              pict "99,999,999.99"
         @lin,40 say mvdpb              pict "99,999,999.99"
         acumul = (acumul + mvdpc + mvdpb)
         @lin,54 say gvdpb              pict '999,999,999.99'
         @lin,69 say (mvdpc + mvdpb)    pict '999,999,999.99'
         @lin,84 say acumul             pict '999,999,999.99'
         mvdpc = 0
         mvdpb = 0
         lin = lin + 1
      endif
      if eof()
         lin = lin + 1
         @lin,24 say gvdpc              pict "999,999,999.99"
         @lin,54 say gvdpb              pict '999,999,999.99'
         @lin,84 say acumul             pict '999,999,999.99'
         lin = lin + 1
         @lin,1 say replicate('-',130)
         lin = lin + 1
         @lin,10 say 'F I M    D E S T E    R E L A T O R I O'
      endif
   enddo
enddo
do impsaida
return
