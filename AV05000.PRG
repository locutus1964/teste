* AV05000 - MENU DE PRODUTOS
* SISTEMA ADMINISTRACAO DE VENDAS
* ADAO ELI PEREIRA - 14-04-90
*
DO WHILE .T.
   NUMIN2 = MENUP_IN(.T.)
   DO CASE
      CASE NUMIN2 = 1
         DO AV05100
      CASE NUMIN2 = 2
         DO AV05200
      CASE NUMIN2 = 3
         DO AV05300
      CASE NUMIN2 = 4
         DO AV05400
      CASE NUMIN2 = 5
         DO AV05500
      CASE NUMIN2 = 6
         DO AV05600
      CASE NUMIN2 = 7
         DO AV05700
      CASE NUMIN2 = 8
         DO AV05800
      CASE NUMIN2 = 9
         DO AV05900
      CASE NUMIN2 = 10
         DO AV05990
      CASE NUMIN2 = 11
         DO AV05999
       CASE LASTKEY() = 27
         RESTORE SCREEN FROM MAIN_SCR
         RETURN
   ENDCASE
   RESTORE SCREEN FROM MAIN2_SCR
ENDDO
QUIT
*                   FUNCAO  ESCOLHA                            *
FUNCTION MENUP_IN
*
PARAMETERS CLR_SCR
NUMHOLD = 0
*
=NOMEMOD( 'AV05000' )
SAVE SCREEN TO MAIN2_SCR
SET MESS TO 23 CENTER
*
IF CLR_SCR
   SET MESSAGE TO 23 CENTER
   DO JANELA WITH 08,20,21,75
   DO BORDA WITH 08,20,21,75
*
   @ 10,23 PROMPT "a - Atualiza‡„o da Identifica‡„o do Produto       " MESSAGE ;
   "Inclui/Altera/Exclui os Produtos e seus dados de identifica‡„o"
   @ 11,23 PROMPT "b - Atualiza‡„o das Quantidades do Produto        " MESSAGE ;
   "Altera as Quantidades usadas para estat¡sticas de Pedidos e Vendas"
   @ 12,23 PROMPT "c - Relat¢rio das Referˆncias cadastradas         " MESSAGE ;
   "Relat¢rio completo ou simplificado de todas as Referˆncias cadastradas"
   @ 13,23 PROMPT "d - Entradas no Estoque por Produ‡„o ou Compra    " MESSAGE ;
   "Processa as entradas no Estoque de Produtos por Produ‡„o ou Compra"
   @ 14,23 PROMPT "e - Relat¢rio das Entradas por Produ‡„o ou Compra " MESSAGE ;
   "Imprime Relat¢rio das Entradas do dia no Estoque por Produ‡„o ou Compra"
   @ 15,23 PROMPT "f - Relat¢rio dos Produtos Faturados e Embarcados " MESSAGE ;
   "Imprime Relat¢rio do Faturamento e Embarque efetuados no dia"
   @ 16,23 PROMPT "g - Registro dos Ajustes de Invent rio            " MESSAGE ;
   "Processa as corre‡”es dos Saldos do Estoque por Ajuste de Invent rio"
   @ 17,23 PROMPT "h - Modelo 3 - Consulta ou Impress„ao de Relat¢rio" MESSAGE ;
   "Consulta ou Relaciona Saldo inicial, Movimentos e Saldo final do estoque"
   @ 18,23 PROMPT "i - Curva ABC - C lculo e Impress„o do Relat¢rio  " MESSAGE ;
   "Calcula e Imprime Relat¢rio das Vendas efetuadas no formato ABC"
   @ 19,23 PROMPT "j - Manuten‡„o das Tabelas de Pre‡os              " MESSAGE ;
   "Atualiza‡„o das Tabelas de Pre‡os por Produto e por Grupo"
   @ 20,23 PROMPT "k - Relatorio de Produtos Vendidos por Regiao     " MESSAGE ;
   ""
   
*
   MENU TO NUMHOLD
ENDIF
*
RETURN(NUMHOLD)
