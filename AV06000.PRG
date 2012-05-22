* AV06000 - MENU DE PEDIDOS
* ADAO ELI PEREIRA - 14-04-90 - 29/11/91
DO WHILE .T.
   NUMIN2 = MENUX_IN(.T.)
   DO CASE
      CASE NUMIN2 = 1        &&--------- a - Manuten‡„o de Pedidos de Clientes
         DO AV060507
      CASE NUMIN2 = 2        &&--------- b - Consulta Pedido pelo n£mero
         DO AV06201
      CASE NUMIN2 = 3        &&--------- c - Consulta Pedidos p/Nome de Guerra do Cliente
         DO AV06250
      CASE NUMIN2 = 4        &&--------- d - Relat¢rio Previa/Lista Selecionada
         DO AV06300
      CASE NUMIN2 = 5        &&--------- e - Relat¢rio dos Pedidos em Aberto por Cliente
         DO AV06353
      CASE NUMIN2 = 6        &&--------- f - Relat¢rio Pedidos em Aberto por Produto/data
         DO AV06401
      CASE NUMIN2 = 7        &&--------- g - Manuten‡„o de Observa‡”es para os Pedidos
         DO AV06450
      CASE NUMIN2 = 8        &&--------- h - Impress„o de Pedidos de Clientes
         DO AV065006
      CASE NUMIN2 = 9        &&--------- h - Relatorio de Embarque de e Pedidos em Aberto
         DO AV06601
      CASE LASTKEY() = 27
         RESTORE SCREEN FROM MAIN_SCR
         CLOS DATA
         RETURN
   ENDCASE
   RESTORE SCREEN FROM MAIN2_SCR
ENDDO
QUIT
* --------------------------------------------------------------------
FUNCTION MENUX_IN
* --------------------------------------------------------------------
PARAMETERS CLR_SCR
NUMHOLD = 0
=NOMEMOD(  'AV06000' )
SAVE SCREEN TO MAIN2_SCR
SET MESS TO 23 CENTER
IF CLR_SCR
   SET MESSAGE TO 23 CENTER
*  DO COR2
   ? JANELA(09,21,19,74)
   ? BORDA(09,21,19,74)
   @ 10,23 PROMPT "a - Manuten‡„o de Pedidos de Clientes            " MESSAGE ;
   "Inclui, Altera e Exclui os Pedidos recebidos dos Clientes"
   @ 11,23 PROMPT "b - Consulta Pedido pelo n£mero                  " MESSAGE ;
   "Consulta os dados do Pedido a partir do n£mero do Pedido"
   @ 12,23 PROMPT "c - Consulta Pedidos p/Nome de Guerra do Cliente " MESSAGE ;
   "Consulta todos os Pedidos do Cliente a partir do seu Nome de Guerra"
   @ 13,23 PROMPT "d - Relat¢rio Previa/Lista Selecionada           " MESSAGE ;
   "Imprime Relat¢rio dos Pedidos para consulta ou corre‡„o de erros"
   @ 14,23 PROMPT "e - Relat¢rio dos Pedidos em Aberto por Cliente  " MESSAGE ;
   "Imprime Relat¢rio dos Pedidos cadastrados por ordem de CGC de Cliente"
   @ 15,23 PROMPT "f - Relat¢rio Pedidos em Aberto por Produto/data " MESSAGE ;
   "Imprime Relat¢rio dos Pedidos em aberto por ordem de Produto (Referˆncia)"
   @ 16,23 PROMPT "g - Manuten‡„o de Observa‡”es para os Pedidos    " MESSAGE ;
   "Inclui/Altera e Exclui as Observa‡”es fixas impressas nos Pedidos"
   @ 17,23 PROMPT "h - Impress„o de Pedidos de Clientes             " MESSAGE ;
   "Imprime os Pedidos recebidos dos Clientes no formato Padr„o"
   @ 18,23 PROMPT "i - Relatorio de Embarque e Pedidos em Aberto    " MESSAGE ;
   "Imprime a Situacao dos Pedidos em Aberto e Embarque de N.F"
   
   MENU TO NUMHOLD
ENDIF
RETURN(NUMHOLD)

