*
* AV06400 - CALCULA A PREVISAO MENSAL DE PRODUCAO
* SISTEMA ADMINISTRACAO DE VENDAS
* ADAO ELI PEREIRA - 24-04-90 - FOX 08/10/92
*
DO TESTIMP2
if CP = 'N'
   return
endif
*------------- PREPARACAO DO ARQUIVO COM REFERENCIAS E DATAS -----------------
=MENSAG('Selecionando Pedidos em Aberto para Emiss„o do Relat¢rio...Aguarde')
stor {} to MDEN
stor 0         to MQTD, MSAL, MPED
stor space(14) to KEY
stor space(11) to KEY2, KEY3, MREF
sele 0
use PREPROD
zap

index on embalage + REF + DTOS(DEN) to CADWORK.IDX
set order to 0

sele 0
use PROD index PROD.IDX
sele 0
use PEDID ORDER X_PEDREF
sele 0
use clien ORDER P_CGC
sele 0
use PEDIC ORDER P_PED
copy stru fields ped to arqtemp
set rela to str(ped,6) into pedid,; 
            cgc into clien
sele 0
use arqtemp alias AUX
do av06400tmp
sele pedic
set rela to
go top
do while !eof()
   MPED = PED
   if Imprime()
      MDEN = DEN
      sele pedid
      SEEK STR(MPED,6)
      do while PED = MPED .and. !eof()
        if SAL > 0
          MREF = REF
          =seek(mref,'prod')
          MSAL = SAL
          sele PREPROD
          APPEND BLANK
          REPL DEN      WITH MDEN,; 
               REF      WITH MREF,; 
               QTD      WITH MSAL,;
               KEY      WITH REF + DTOS(DEN),;
               PED      WITH MPED,; 
               EMBALAGE with PROD->EMBALAGEM
          sele pedid
       endif
       skip
     enddo
     sele pedic
   endif
   skip
enddo

close data

=MENSAG('Totalizando os Produtos por Data de Entrega... Aguarde')
use PREPROD index CADWORK.IDX

*--- Tabela dos Produtos Vendidos S/N
*sele 0 
*USE LANCTO 
*INDEX ON REF+DTOS(DATAREF) TO LANCTO.IDX

sele 0
use PROD index PROD.IDX

sele PREPROD

=MENSAG('Imprimindo Relat¢rio Pedidos em aberto por Produto e Data... Aguarde')
stor 0         to PAGCTR, MQTD
stor space(11) to KEY3
stor 2         to TAMREL
DO DEFSAIDA
lin = 60
=CABEC('AV06400', 'PEDIDOS EM ABERTO POR PRODUTO E DATA',130)
@prow()+2,001 say 'REF. DESCRICAO DO PRODUTO'
@prow()  ,067 say 'SALDO'
@prow()  ,076 say 'EMBALAGEM'
@prow()+1,001 say REPLICATE('-',130)
lin = prow()+1
do while !eof()
   vemb = embalage
   do while !eof() .and. embalage = vemb
      if lin >= 60
         =CABEC('AV06400', 'PEDIDOS EM ABERTO POR PRODUTO E DATA',130)
         @prow()+2,001 say 'REF. DESCRICAO DO PRODUTO'
         @prow()  ,067 say 'SALDO'
         @prow()  ,076 say 'EMBALAGEM'
         @prow()+1,001 say REPLICATE('-',130)
         lin = prow()+1
      endif
      @lin,1 say ref
      if seek(ref,'prod')
         @lin,06 say prod->des
      else
         @lin,13 say '**** PRODUTO NAO CADASTRADO **** '
      endif
      vref = ref
      vqtd = 0
      do while !eof() .and. embalage = vemb .and. ref = vref
         vqtd = vqtd + qtd
         skip
      enddo
      *-------- Verifica se existem itens nao vendidos com Nota
      MQTDVND     = 0
      vqtd = vqtd - mqtdvnd
      @lin,65 say vqtd             pict '99,999,999'
      @lin,76 say vemb
      lin = lin + 1
   enddo
   lin = 60
enddo
@lin,1 say REPLICATE('-',130)
@lin+1,10 say 'F I M    D E S T E    R E L A T O R I O'
DO IMPSAIDA
close data

*erase CADWORK.IDX
erase arqtemp.dbf
erase arqtemp.idx
return

*===================
procedure AV06400tmp
*===================
* Esta rotina seleciona alguns pedidos
MENS = 'Deseja selecionar Todos os PEDIDOS'
do PEGRESP with [SN]
if MC = 'N'
   sele AUX
   index on str(ped,6) to arqtemp.IDX
   @23,01 clear to 23,78
   save screen to xlixo
*   @04,00 clear to 21,79
   @11,26 clear to 18,52
   @11,26 to    18,52 double
   @16,28 say 'Tecle ESC para continuar'
   stor space(4) to mregiao, mcvr
   do while .t.
      =MENSAG('   ')
      mped = 0
      @13,30 say 'Pedido.......:' get mped  pict '999999' valid av06400val(mped)
      read
      if lastkey() = 27
         exit
      endif
      sele AUX
      seek str(mped,6)
      if !found()
         append blank
         replace ped with mped
      else
         =MENSAG('Pedido j  foi digitado - ENTER')
         =INKEY(0)
      endif
   enddo
   restore screen from xlixo
endif
=MENSAG('selecionando Pedidos em aberto para emiss„o do Relat¢rio...Aguarde')
return
*ßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßß
function AV06400val
para xped
priv vlixo
vlixo = .t.
if !seek(xped,'pedic')
   =MENSAG('Pedido n„o existe - ENTER')
   =INKEY(0)
   vlixo = .f.
else
   if seek(str(xped,6),'pedid')
      if pedid->sal = 0
         =MENSAG('Pedido Sem Saldo - ENTER')
         =INKEY(0)
         vlixo = .f.
      endif
   else
      =MENSAG('Pedido Sem Itens - ENTER')
      =INKEY(0)
      vlixo = .f.
   endif
endif
return vlixo
*ßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßß
function Imprime
 private mfaz
mfaz  = .t.
sele AUX
if reccount() # 0
   seek str(mped,6)
   if !found()
      mfaz = .f.
   endif
endif
sele pedic
return mfaz

