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
@12,44 get m_mmes2 valid empty(m_mmes2) .or. right(m_mmes2,2)+left(m_mmes2,2) > ;
                         right(m_mmes,2)+left(m_mmes,2)  pict '99/99'
read
mens = dadcorr
do pegresp with [SN]
if mc = 'N' .or. empty(mc)
   return
endif
*

=mensag('Selecionando as Notas Fiscais de Venda... Aguarde')
SELE 0
USE TPNF INDEX TPNF.IDX
SELE 0
use cadnf
SET RELATION TO TIP INTO TPNF
if SUBSTR(m_mmes,1,2) = '00'
   *--------- Seleciona s¢ o Ano
   copy to cadwork1 for SUBSTR(demi,7,2) = SUBSTR(m_mmes,4,2) .and.;
                        empty(cancelnf) .AND.;
                        TPNF->VENDA = 'S'

else
   if empty(m_mmes2)
      *--------- Seleciona s¢ o Mes e Ano da primeira data
      copy to cadwork1 for SUBSTR(demi,4,5) = m_mmes .and.;
                           empty(cancelnf) .AND.;
                           TPNF->VENDA = 'S'
   else  
      *---------- Seleciona o per¡odo
      dData1 = CTOD( '01/' + m_mmes  )
      dData2 = CTOD( '01/' + m_mmes2 )
      dData2 = CTOD(STR(NDIAMES(dData2),2) + '/' + m_mmes2 )
      copy to cadwork1 for;
              CTOD(demi) >= dData1 .AND. CTOD(demi) <= dData2 .AND.;
              empty(cancelnf) .AND.;
              TPNF->VENDA = 'S'
   endif
endif
save to av10100 all like m_*
close data
return

