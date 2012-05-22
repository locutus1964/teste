* MESTATIS - MENU DE ESTATISTICAS
* SISTEMA CONTAS A RECEBER
* ADAO ELI PEREIRA - 03/10/93
*
DO WHILE .T.
  NUMIN2 = MENUEST_IN(.T.)
  DO CASE
    CASE NUMIN2 = 1
      DO POSFIN          && Imprime a Posi‡„o Financeira dos Clientes
    CASE NUMIN2 = 2
      DO AV10401         && Imprime Relat¢rio das Vendas por Cliente em um Per¡odo
    CASE NUMIN2 = 3
      DO VENDADIA        && Imprime Relat¢rio das Vendas por Banco e Carteira em um Per¡odo
    CASE NUMIN2 = 4  
      DO ATRASOS2        && Imprime Relat¢rio dos Atrasos por Cliente
    CASE NUMIN2 = 5
      DO VALREC          && Imprime Relat¢rio dos Valores Recebidos por Cliente
    CASE LASTKEY() = 27
      RESTORE SCREEN FROM MAIN_SCR
      RETURN
  ENDCASE
      RESTORE SCREEN FROM MAIN2_SCR
ENDDO
QUIT
* --------------------------------------------------------------------
                   FUNCTION MENUEST_IN
* --------------------------------------------------------------------
PARAMETERS CLR_SCR
NUMHOLD = 0
*
@ 02,70 SAY 'ESTATIST'
SAVE SCREEN TO MAIN2_SCR
SET MESS TO 23 CENTER
*
IF CLR_SCR
  SET MESSAGE TO 23 CENTER
  DO JANELA WITH 09,37,21,76
  DO BORDA WITH 09,37,21,76
*
@ 11,40 PROMPT "a - Posi‡„o Financeira de Clientes" MESSAGE ;
  "Imprime a Posi‡„o Financeira dos Clientes"
@ 13,40 PROMPT "b - Vendas no Per¡odo por Cliente " MESSAGE ;
  "Imprime Relat¢rio das Vendas por Cliente em um Per¡odo"
@ 15,40 PROMPT "c - Vendas por Banco e Carteira   " MESSAGE ;
  "Imprime Relat¢rio das Vendas por Banco e Carteira em um Per¡odo"
@ 17,40 PROMPT "d - Atrasos por Cliente           " MESSAGE ;
  "Imprime Relat¢rio dos Atrasos por Cliente"
@ 19,40 PROMPT "e - Valores recebidos por Cliente " MESSAGE ;
  "Imprime Relat¢rio dos Valores Recebidos por Cliente"
*
MENU TO NUMHOLD
ENDIF
*
RETURN(NUMHOLD)
