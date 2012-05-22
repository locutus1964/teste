* AV07500 - MENU DE NOTAS FISCAIS ON-LINE
* SISTEMA ADMINISTRACAO DE VENDAS
* ADAO ELI PEREIRA - 09/07/90
*
DO WHILE .T.
   NUMIN2 = MENUY_IN(.T.)
   DO CASE
      CASE NUMIN2 = 1               &&--- Seleciona Pedidos para aprova‡„o
         DO AV07510
      CASE NUMIN2 = 2               &&--- Imprime Notas com Pedidos aprovados
         DO AV074206
      CASE LASTKEY() = 27
         CLEAR
         RESTORE SCREEN FROM MAIN_SCR
         RETURN
   ENDCASE
   RESTORE SCREEN FROM MAIN2_SCR
ENDDO
QUIT

************************************************************************
FUNCTION MENUY_IN
************************************************************************
PARAMETERS CLR_SCR
NUMHOLD = 0
*
=NOMEMOD( 'AV07500' )
SAVE SCREEN TO MAIN2_SCR
*
IF CLR_SCR
   SET MESSAGE TO 23 CENTER
   ? JANELA(12,34,18,78)
   ? BORDA(12,34,18,78)
   @ 14,37 PROMPT "a - Seleciona Pedidos para aprova‡„o   " MESSAGE ;
     "Seleciona os Pedidos de Clientes para aprova‡„o e emiss„o das Notas Fiscais"
   **  @ 16,37 PROMPT "b - Aprova os Pedidos selecionados     " MESSAGE ;
   **    "Os Pedidos selecionados s„o aprovados quanto … Saldo e Limite de Cr‚dito"
   @ 16,37 PROMPT "b - Imprime Notas com Pedidos aprovados" MESSAGE ;
     "Imprime as Notas Fiscais com Pedidos aprovados - Tem Saldo e Cr‚dito"
   MENU TO NUMHOLD
ENDIF

RETURN(NUMHOLD)
