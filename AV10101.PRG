* AV10100 - PREPARACAO DE RELATORIOS PARA VENDAS
* SISTEMA ADMINSTRACAO DE VENDAS
* ADAO ELI PEREIRA - 30/07/90
*
public m_mmes, m_mmes2
stor space(5) to m_mmes, m_mmes2

* =janela(16,33,22,78)
* =borda(16,33,22,78)
* @ 18,38 say 'Informe MES/ANO para sele‡„o 'get m_mmes pict '99/99'
* @ 20,35 say 'Para selecionar s¢ o ano, digite 00 no mˆs'

lSoSP = .F.
@04,00 clear to 21,79
=janela(06,17,20,63)
=borda(06,17,20,63)
@08,20 say 'Informe os dados abaixo:'
@10,29 say '1§ MES / ANO -'
@12,29 say '2§ MES / ANO -'
@14,20 say 'ATEN€ŽO - Para selecionar:'
@15,24 say '- Um periodo informe o 1§ e 2§.'
@16,24 say '- Um mˆs Digite apenas a 1¦ data.'
@17,24 say '- Para selecionar s¢ o ano, digite 00'
@18,24 say '  no mˆs da 1¦ data e o ano desejado'

@10,44 get m_mmes  pict '99/99' valid ! empty(m_mmes)
@12,44 get m_mmes2 valid empty(m_mmes2) .or.;
                         right(m_mmes2,2) + left(m_mmes2,2) > ;
                         right(m_mmes,2)  + left(m_mmes,2)  pict '99/99'
read

IF LASTKEY() = 27
  RETURN
ENDIF
  
mens = dadcorr
do pegresp with [SN]
if mc = 'N' .or. empty(mc)
   return
endif
*
MENS = 'Gerar dados somente de SP ?'
DO PEGRESP WITH [SN]

lSoSP = MC = 'S'

=mensag('Selecionando as Notas Fiscais de Venda... Aguarde')
cFiltro = '.T.'
cFiltSP = '.T.'

if SUBSTR(m_mmes,1,2) = '00'
   *--------- Seleciona s¢ o Ano
   cFiltro = 'MONTH(NF->demissao) = VAL(SUBSTR(m_mmes,4,2))'
   cFiltSP = 'MONTH(CAB->dtfatura) = VAL(SUBSTR(m_mmes,4,2))'
else
   if empty(m_mmes2)
      *--------- Seleciona s¢ o Mes e Ano da primeira data
     cFiltro = 'SUBSTR(DTOC(NF->demissao),4,5) = m_mmes'
     cFiltSP = 'SUBSTR(DTOC(CAB->dtfatura),4,5) = m_mmes'
   else  
      *---------- Seleciona o per¡odo
      dData1 = CTOD( '01/' + m_mmes  )
      dData2 = CTOD( '01/' + m_mmes2 )
      dData2 = CTOD(STR(NDIAMES(dData2),2) + '/' + m_mmes2 )
      cFiltro = '(NF->demissao >= dData1) AND (NF->demissao <= dData2)'
      cFiltSP = '(CAB->dtfatura >= dData1) AND (CAB->dtfatura <= dData2)'
   endif
endif

save to av10100 all like m_*

SELECT ;
  NF.CGC, NF.NNF, NF.DEMISSAO, NF.REGIAO, NF.IDREGIAO, NF.VEN, IT.REF, IT.QTD, IT.VAL;
  FROM CABECNF NF, CADNF IT, TPNF TP;
  WHERE (NF.NNF = IT.NNF) AND (NF.TIPNF = TP.TIPO) AND (TP.VENDA = 'S') AND;
        (CANCELADA = .F.) AND &cFiltro;
  INTO TABLE CADWORK1      

SELECT cli.cgc, cab.dtentreg, cli.regiao, cli.idregiao, cab.repr, it.ref, it.qtd;
  from cabpedsp cab, itemsp it, clien cli;
  where (it.idpedido=cab.idpedido) AND (cli.gue = cab.guerra) AND &cFiltSP;
  into table avsp

close data

SELE 0 
USE CADWORK1
SELE 0
USE AVSP
INDEX ON REF TO AVSP

IF lSoSP 
  *--------- Somente dados do AVSP
  SELE CADWORK1
  ZAP
  SELE AVSP
  DO WHILE ! EOF()
    SELE CADWORK1
    APPEND BLANK
    DO GRAVAR
    SELE AVSP
    SKIP
  ENDDO
ELSE
  *--------- Inclui os dados do avsp no cadwork1
  SELE CADWORK1
  DO WHILE ! EOF()
    IF SEEK( REF, 'AVSP' )
      APPEND BLANK
      DO GRAVAR
    ENDIF  
    SKIP  
  ENDDO
ENDIF
CLOSE DATA

return

****************
PROCEDURE GRAVAR
****************

   REPLACE CGC      WITH AVSP->CGC
   REPLACE NNF      WITH 0
   REPLACE DEMISSAO WITH AVSP->DTENTREG
   REPLACE REGIAO   WITH AVSP->REGIAO
   REPLACE VEN      WITH AVSP->REPR
   REPLACE REF      WITH AVSP->REF
   REPLACE QTD      WITH AVSP->QTD
   REPLACE VAL      WITH 0

RETURN