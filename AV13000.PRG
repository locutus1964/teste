* MCONSULT - MENU DE CONSULTAS
* ADAO ELI PEREIRA - VERSAO 1.2 - 04/11/92

DO WHILE .T.
   NUMIN2 = MENU_IN12(.T.)
   DO CASE
    CASE NUMIN2 = 1   && Notas Fiscais emitidas por Cliente
      DO AV13100
    CASE NUMIN2 = 2   && Notas Fiscais emitidas por Produto
      DO AV13200
    CASE NUMIN2 = 3   && Notas Fiscais emitidas por Pedido
      DO AV13300
    CASE LASTKEY() = 27 
      CLEAR
      RESTORE SCREEN FROM MAIN_SCR
      RETURN
   ENDCASE
   RESTORE SCREEN FROM MAIN2_SCR
ENDDO
QUIT

************************************************************************
FUNCTION MENU_IN12
************************************************************************
PARAMETERS CLR_SCR
NUMHOLD = 0

=NOMEMOD( 'AV13000' )
SAVE SCREEN TO MAIN2_SCR
SET MESSAGE TO 23 CENTER
*
IF CLR_SCR
  DO JANELA WITH 08,34,15,77
  DO BORDA WITH 09,34,15,77

   @ 10,37 PROMPT "a - Notas Fiscais emitidas por Cliente" MESSAGE ;
   "Dado um Cliente, mostra as Notas Fiscais emitidas"
   @ 11,37 PROMPT "b - Notas Fiscais emitidas por Produto" MESSAGE ;
   "Dado um Produto, mostra as Notas Fiscais emitidas"
   @ 12,37 PROMPT "c - Notas Fiscais emitidas por Pedido " MESSAGE ;
   "Dado um Pedido, mostra as Notas Fiscais emitidas"
   
  MENU TO NUMHOLD
ENDIF
*
RETURN(NUMHOLD)
