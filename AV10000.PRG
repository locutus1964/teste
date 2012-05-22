* AV10000 - MENU DE ESTATISTICAS DE VENDAS
* SISTEMA ADMINISTRACAO DE VENDAS
* ADAO ELI PEREIRA - 30/07/90
*
DO WHILE .T.
   NUMINES = MENUES_IN(.T.)
   DO CASE
      CASE NUMINES = 1        &&--- a - Sele‡„o de Notas Fiscais do Mes
         DO AV10100
      CASE NUMINES = 2        &&--- b - Relat¢rio Vendas por Quantidade
         DO AV10200
      CASE NUMINES = 3        &&--- c -                  por Valor
         DO AV10300
      CASE NUMINES = 4        &&--- d -                  por Cliente
         DO AV10400
      CASE NUMINES = 5        &&--- e - Sele‡„o de Dados p/Demonstrativo
         DO AV10500
      CASE NUMINES = 6        &&--- f - Impress„o do Demonstrativo
         DO AV10600
      CASE NUMINES = 7        &&--- g - Limpa arquivos utilizados p/estat
         DO AV10700
      CASE NUMINES = 8        &&--- h - Relacao do maiores clientes por Regiao
         DO AV10800
      CASE NUMINES = 9        &&--- h - Relacao do maiores clientes Geral
         DO AV10900
      CASE LASTKEY() = 27
         RESTORE SCREEN FROM MAIN_SCR
         RETURN
   ENDCASE
   RESTORE SCREEN FROM MAIN2_SCR
ENDDO
QUIT
* --------------------------------------------------------------------
FUNCTION MENUES_IN
* --------------------------------------------------------------------
PARAMETERS CLR_SCR
NUMHOLD = 0
=NOMEMOD( 'AV10000' )
SAVE SCREEN TO MAIN2_SCR
SET MESS TO 23 CENTER
IF CLR_SCR
   SET MESSAGE TO 23 CENTER
   DO JANELA WITH 10,20,22,63
   DO BORDA WITH 10,20,22,63
   @ 12,23 PROMPT " a - Sele‡„o de Notas Fiscais do Mes  "MESSAGE ;
   'Seleciona as Notas Fiscais das Vendas do mes para as Estat¡sticas'
   @ 13,23 PROMPT " b - Relat¢rio Vendas por Quantidade  "MESSAGE ;
   'Imprime Relat¢rio das Vendas do mes - Quantidade de Produto Faturado'
   @ 14,23 PROMPT " c -                  por Valor       "MESSAGE ;
   'Imprime Relat¢rio das Vendas do mes - Valor Faturado por Produto'
   @ 15,23 PROMPT " d -                  por Cliente     "MESSAGE ;
   'Imprime Relat¢rio das Vendas do mes - Valor Faturado por Cliente'
   @ 16,23 PROMPT " e - Sele‡„o de Dados p/Demonstrativo "
   @ 17,23 PROMPT " f - Impress„o do Demonstrativo       "
   @ 18,23 PROMPT " g - Limpa arquivos utilizados p/estat"
   @ 19,23 PROMPT " h - Maiores Cliente por RegiÆo" MESSAGE;
   'Imprime Relat¢rio dos Maiores Clientes Por RegiÆo'
   @ 20,23 PROMPT " i - Maiores Cliente Geral" MESSAGE;
   'Imprime Relat¢rio dos Maiores Clientes Geral'
   MENU TO NUMHOLD
ENDIF
RETURN(NUMHOLD)
