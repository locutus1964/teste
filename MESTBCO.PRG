* MESTBCO - MENU DE CLIENTES POR BANCO
* SISTEMA CONTAS A RECEBER
* ADAO ELI PEREIRA - 02/04/94
*
DO WHILE .T.
  NUMIN2 = MENUBCO_IN(.T.)
  DO CASE
    CASE NUMIN2 = 1
      DO BANCOCLI
    CASE NUMIN2 = 2
      DO CLIBANCO
    CASE LASTKEY() = 27
      RESTORE SCREEN FROM MAIN_SCR
      RETURN
  ENDCASE
      RESTORE SCREEN FROM MAIN2_SCR
ENDDO
QUIT
* --------------------------------------------------------------------
                   FUNCTION MENUBCO_IN
* --------------------------------------------------------------------
PARAMETERS CLR_SCR
NUMHOLD = 0
*
@ 02,70 SAY ' ESTBCO'
SAVE SCREEN TO MAIN2_SCR
SET MESS TO 23 CENTER
*
IF CLR_SCR
  SET MESSAGE TO 23 CENTER
  DO JANELA WITH 12,37,18,76
  DO BORDA WITH 12,37,18,76
*
@ 14,40 PROMPT "a - Relat¢rio Clientes por Banco" MESSAGE ;
   "Dado um Banco, relaciona as duplicatas em cobran‡a por Cliente"
@ 16,40 PROMPT "b - Relat¢rio Bancos por Cliente" MESSAGE ;
   "Dado um Cliente, relaciona os Bancos e as duplicatas em cobran‡a"
*
MENU TO NUMHOLD
ENDIF
*
RETURN(NUMHOLD)
