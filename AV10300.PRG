* AV10300 - CALCULO E IMPRESSAO DAS VENDAS do MES POR VALOR DE PRODUTOS
* SISTEMA ADMINISTRACAO DE VENDAS - REVISAdo EM 13/02/93
* ADAO ELI PEREIRA
*
do TESTIMP2
if CP = 'N'
   return
endif
if !FILE('CADWORK1.DBF')
   =mensag('Processar primeiro a sele‡„o de Notas Fiscais - op‡„o a - ENTER')
   =INKEY(0)
   return
endif
=mensag('Classificando as Notas Fiscais pelo C¢digo')
use CADWORK1
index on REF to CADWORK1.IDX
=mensag('Totalizando as Notas Fiscais pelo C¢digo... Aguarde')
use CADWORK1 index CADWORK1.IDX
TOTAL TO CADWORK2 ON REF FIELDS VAL,QTD
=mensag('Totalizando valores e quantidades... Aguarde')
stor 0 TO PAGCTR, MCONT, QTDGER, VALGER, ACV, ACQ
TAMREL = 2
use CADWORK2
do WHILE !eof()
   stor (VAL + VALGER) TO VALGER
   stor (QTD + QTDGER) TO QTDGER
   skip
enddo
=mensag('Classificando por Valor de Produto... Aguarde')
use CADWORK2
index ON (100000000000 - VAL) TO CADWORK2.IDX
close data
use PROD index PROD.IDX
sele 0
use CADWORK2 index CADWORK2.IDX
=mensag('Imprimindo o Relat¢rio... Aguarde')
do DEFSAIDA

rest from av10100 addi
if left(m_mmes,2) = '00'
   vcab = 'RELATORIO VENDAS DO ANO DE '+ right(m_mmes,2) + ' POR VALOR'
else
   if empty(m_mmes2)
      vcab = 'RELATORIO VENDAS DE '+ m_mmes + ' POR VALOR'
   else
      vcab = 'RELATORIO VENDAS DE '+ m_mmes + ' A ' + m_mmes2 + ' POR VALOR'
   endif
endif
do WHILE !eof()
   =CABEC('AV10300',VCAB,131)
   @03,001 say 'SEQ REF. EMB           D   E   S   C   R   I   C   A   O           VALOR TOTAL   QUANTID.  PRECO UNIT.  |--------PERCENTUAIS------|'
   @04,001 say '                                                                      DO MES                  MEDIO      S/QTD  ACUM  S/VAL   ACUM '
   @05,001 say '=== ==== === ==================================================== ============== ======== ============= ======= ===== ======= ====='
   stor 6 TO lin
   do WHILE !eof() .AND. lin < 58
      stor MCONT + 1 TO MCONT
      @lin,001 say STR(MCONT,3)
      @lin,005 say REF
      stor SUBSTR(REF,1,11) TO MREF
      if seek(MREF,'prod')
         @lin,010 say prod->embalagem
         @lin,014 say left(prod->DES,52)
      else
         @lin,14 say 'PRODUTO NAO CADASTRADO'
      endif
      @lin,067 say VAL                 pict '9999999,999.99'
      @lin,082 say QTD                 pict '9999,999'
      stor (VAL / QTD) TO MPREU
      @lin,091 say MPREU               pict '999999,999.99'
      if QTDGER > 0
         stor ((QTD / QTDGER) * 100) TO MPART
         if MPART > 0.000
            @lin,105 say MPART               pict '999.999'
            stor (MPART + ACQ) TO ACQ
            @lin,113 say ACQ                 pict '999.9'
         endif
      endif
      if VALGER > 0
         stor ((VAL / VALGER) * 100) TO MPART
         if MPART > 0.000
            @lin,119 say MPART               pict '999.999'
            stor (MPART + ACV) TO ACV
            @lin,127 say ACV                 pict '999.9'
         endif
      endif
      skip
      if eof()
         @lin+1,1 say replicate('-',131)
         stor lin + 2 TO lin
         @lin,053 say 'T O T A I S'
         @lin,067 say VALGER                 pict '9999999,999.99'
         @lin,082 say QTDGER                 pict '9999,999'
      endif
      stor lin + 1 TO lin
   enddo
enddo
do IMPSAIDA
return

TEXT

SEQ REF. EMB           D   E   S   C   R   I   C   A   O           VALOR TOTAL   QUANTID.  PRECO UNIT.  |--------PERCENTUAIS------|
.                                                                     DO MES                  MEDIO      S/QTD  ACUM  S/VAL   ACUM
=== ==== === ==================================================== ============== ======== ============= ======= ===== ======= =====
999 AAAA AAA AAAAAAAAAAAAAAAAAAA- 52 -AAAAAAAAAAAAAAAAAAAAAAAAAAA 9999999,999.99 9999,999 999999,999.99 999.999 999.9 999.999 999.9
1   5    10  14                                                   67             82       91            105     113   119     127
-----------------------------------------------------------------------------------------------------------------------------------

@lin,001 say STR(MCONT,3)
@lin,005 say REF
@lin,010 say prod->embalagem
@lin,014 say left(prod->DES,52)
@lin,067 say VAL                 pict '9999999,999.99'
@lin,082 say QTD                 pict '9999,999'
@lin,091 say MPREU               pict '999999,999.99'
@lin,105 say MPART               pict '999.999'
@lin,113 say ACQ                 pict '999.9'
@lin,119 say MPART               pict '999.999'
@lin,127 say ACV                 pict '999.9'

ENDTEXT

