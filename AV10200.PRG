* AV010200 - CALCULO E IMPRESSAO DAS VENDAS do MES POR QUANTIDADE DE PRODUTOS
* ADAO ELI PEREIRA - REVISAdo EM 13/02/93
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
=mensag('Classificando as Notas Fiscais selecionadas... Aguarde')
use CADWORK1
index ON REF to CADWORK1.IDX
=mensag('Totalizando as Notas Fiscais por C¢digo... Aguarde')
use CADWORK1 index CADWORK1.IDX
TOTAL to CADWORK2 ON REF FIELDS VAL,QTD
=mensag('Totalizando valores e quantidades... Aguarde')
stor 0 to PAGCTR, MCONT, QTDGER, VALGER, ACQ, ACV
TAMREL = 2
use CADWORK2
do WHILE !eof()
   stor (VAL + VALGER) to VALGER
   stor (QTD + QTDGER) to QTDGER
   skip
enddo
=mensag('Classificando por quantidade de Produto... Aguarde')
use CADWORK2
index ON (100000000000 - QTD) to CADWORK2.IDX
close data
use PROD index PROD.IDX
sele 0
use CADWORK2 index CADWORK2.IDX
=mensag('Imprimindo o Relat¢rio... Aguarde')
do DEFSAIDA
*-------------- PREPARACAO DOS VALORES FINAIS DA NOTA E CALCULO do ICM
rest from av10100 addi
if left(m_mmes,2) = '00'
   vcab = 'RELATORIO VENDAS DO ANO DE '+ right(m_mmes,2) + ' POR QUANTIDADE'
else
   if empty(m_mmes2)
      vcab = 'RELATORIO VENDAS DE '+ m_mmes + ' POR QUANTIDADE'
   else
      vcab = 'RELATORIO VENDAS DE '+ m_mmes + ' A ' + m_mmes2 + ' POR QUANTIDADE'
   endif
endif
do WHILE !eof()
   =CABEC('AV10200',vcab,131)
   @03,001 say 'SEQ REF. EMB           D   E   S   C   R   I   C   A   O          QUANTID.  VALOR TOTAL    PRECO UNIT.  |--------PERCENTUAIS------|'
   @04,077 say '                                                                               DO MES         MEDIO      S/QTD  ACUM  S/VAL   ACUM '
   @05,001 say '=== ==== === ==================================================== ======== ============== ============= ======= ===== ======= ====='
   stor 6 to lin
   do WHILE !eof() .AND. lin < 58
      stor MCONT + 1 to MCONT
      @lin,001 say STR(MCONT,3)
      @lin,005 say REF
      stor REF to MREF
      if seek(MREF,'prod')
         @lin,010 say prod->embalagem
         @lin,014 say left(prod->DES,52)
      else
         @lin,14 say 'PRODUTO NAO CADASTRADO'
      endif
      @lin,067 say QTD                 pict '9999,999'
      @lin,076 say VAL                 pict '9999999,999.99'
      stor (VAL / QTD) to MPREU
      @lin,091 say MPREU               pict '999999,999.99'
      if QTDGER > 0
         stor ((QTD / QTDGER) * 100) to MPART
         if MPART > 0.000
            @lin,105 say MPART               pict '999.999'
            stor (MPART + ACQ) to ACQ
            @lin,113 say ACQ                 pict '999.9'
         endif
      endif
      if VALGER > 0
         stor ((VAL / VALGER) * 100) to MPART
         if MPART > 0.000
            @lin,119 say MPART               pict '999.999'
            stor (MPART + ACV) to ACV
            @lin,127 say ACV                 pict '999.9'
         endif
      endif
      skip
      if eof()
         stor lin + 1 to lin
         @lin,1 say replicate('-',131)
         @prow()+1,053 say 'T O T A I S'
         @prow(),067 say QTDGER                 pict '9999,999'
         @prow(),076 say VALGER                 pict '9999999,999.99'
         lin = prow()
      endif
      stor lin + 1 to lin
   enddo
enddo
do IMPSAIDA
return

