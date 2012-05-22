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
mens = dadcorr
do pegresp with [SN]
if mc = 'N' .or. empty(mc)
   return
endif
*
=mensag('Selecionando as Notas Fiscais de Venda... Aguarde')
if SUBSTR(m_mmes,1,2) = '00'
   *--------- Seleciona s¢ o Ano
   cFiltro = MONTH(demissao) = VAL(SUBSTR(m_mmes,4,2))
   cFiltSP = MONTH(dtfatura) = VAL(SUBSTR(m_mmes,4,2))
else
   if empty(m_mmes2)
      *--------- Seleciona s¢ o Mes e Ano da primeira data
     cFiltro = SUBSTR(DTOC(demissao),4,5) = m_mmes
     cFiltSP = SUBSTR(DTOC(dtfatura),4,5) = m_mmes
   else  
      *---------- Seleciona o per¡odo
      dData1 = CTOD( '01/' + m_mmes  )
      dData2 = CTOD( '01/' + m_mmes2 )
      dData2 = CTOD(STR(NDIAMES(dData2),2) + '/' + m_mmes2 )
      cFiltro = (demissao >= dData1) AND (demissao <= dData2)
      cFiltSP = (dtfatura >= dData1) AND (dtfatura <= dData2)
   endif
endif

save to av10100 all like m_*

SELECT IT.*;
  FROM CABECNF NF, CADNF IT, TPNF TP;
  WHERE (NF.NNF = IT.NNF) AND (NF.TIPNF = TP.TIPO AND TP.VENDA = 'S') AND;
        (CANCELADA = .F.) AND &cFiltro;
  INTO TABLE CADWORK1      

IF FILE('CABPEDSP.DBF' ) .AND. FILE('ITEMSP.DBF' )
  select cad.dtfatura, it.ref, it.qtd;
    from cabpedsp cad, itemsp it;
    where (it.idpedido=cad.idpedido) AND &cFiltSP
    into table avsp
 
  close data

  *--------- Inclui os dados do avsp no cadwork1
  SELE 0 
  USE CADWORK1
  SELE 0
  USE AVSP
  INDEX ON REF TO AVSP
  SELE CADWORK1
  DO WHILE ! EOF()
    IF SEEK( REF, 'AVSP' )
      APPEND BLANK
      REPLACE REF WITH AVSP->REF
      REPLACE QTD WITH AVSP->QTD
    ENDIF  
    SKIP  
  ENDDO
ENDIF

CLOSE DATA

return

