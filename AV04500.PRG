*ÉÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ»
*º Programa....: AV04500.prg                                                 º
*º Sistema.....: Administra‡„o de Vendas - Ad„o Eli Pereira                  º
*º Objetivo....: Relat¢rio resumo da situa‡„o dos clientes por regi„o e      º
*º               representantes.                                             º
*º Data........: Quarta, 22 de novembro de 1995 …s 10:49 horas               º
*º Autor.......: Manuel In cio Lino de Almeida                               º
*ÈÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¼
private mven, mregiao, vtotreg, mstatus, mperc
do TESTIMP2
if CP = 'N'
   return
endif
pagctr = 0
tamrel = 2
sele 0
use regiao index regiao
sele 0
use vend   index vend
sele 0
use status index status
sele 0
use clien

=mensag('Criando ind¡ce tempor rio')
*index on ven + regiao + estatus to arqtmp
index on regiao + ven + estatus to arqtmp
set rela to ven into vend, regiao into regiao
go top
=MENSAG('Imprimindo Relat¢rio da Situa‡„o dos Clientes... Aguarde')
do DEFSAIDA
tit = '==  RESUMO  DE  SITUACAO  DOS  CLIENTES  =='
lin = 60
do while !eof()
   if lin >= 60
      =av04500cab()
   endif
   @lin,001 say ven
   mven = ven
   do while !eof() .and. ven = mven
      if lin >= 60
         =av04500cab()
         @lin,001 say mven
      endif
      cabregiao = regiao->dregiao
      cabven    = vend->vengue
      @lin,009 say left(cabregiao,29)
      @lin,039 say cabven
      mregiao = regiao
      vtotreg = 0
      sele status
      repl all quan with 0
      sele clien
      do while !eof() .and. ven = mven .and. regiao = mregiao
         mstatus = estatus
         vtotsta = 0
         do while !eof() .and. ven = mven .and. regiao = mregiao .and. estatus=mstatus
            vtotreg = vtotreg + 1
            vtotsta = vtotsta + 1
            skip
         enddo
         sele status
         seek mstatus
         mobs = ' '
         if found()
            repla quan with vtotsta
         else
            mobs = '*'
         endif
         sele clien
      enddo
      if lin >= 60
         =av04500cab()
         @lin,001 say mven
         @lin,009 say left(cabregiao,29)
         @lin,039 say cabven
      endif
      if vtotreg # 0
         sele status
         go top
         do while !eof()
            @lin,status->colu   say quan                    pict '@z 9999'
            @lin,status->colu+5 say (100 * quan) / vtotreg  pict '@z 999'
            skip
         enddo
         @lin,127 say vtotreg pict '99999'
      endif
      @lin,pcol()+1 say mobs
      sele clien
      lin = lin + 1
   enddo
enddo
@lin-1,1 SAY REPLICATE('-',131)
@lin,10 SAY 'F I N A L    D E S T E     R E L A T O R I O'
DO IMPSAIDA
return

*ßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßß
function av04500cab
=CABEC('AV04500',TIT,131)
@prow()+2,01   say '                                                     ³ ATIVOS ³CORTADO ³DESATIVA³ FECHOU ³INATIVO ³MUDOU RA³NAO COMP³POSSIVEL³     '
@prow()+1,01   say 'REPR UF         R E G I A O            REPRESENTANTE ³QTDE  % ³QTDE  % ³QTDE  % ³QTDE  % ³QTDE  % ³QTDE  % ³QTDE  % ³QTDE  % ³TOTAL'
@prow()+1,01   say '==== == ============================= ===============³==== ===³==== ===³==== ===³==== ===³==== ===³==== ===³==== ===³==== ===³====='
lin = prow()+1
return ""

