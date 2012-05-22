* MESTATIS - MENU DE ESTATISTICAS
* SISTEMA CONTAS A RECEBER
* ADAO ELI PEREIRA - 03/10/93
*
DO WHILE .T.
  NUMIN2 = MENUEST_IN(.T.)
  DO CASE
    CASE NUMIN2 = 1
      DO AV08210          && Imprime a Posi‡„o Financeira dos Clientes
    CASE NUMIN2 = 2
      DO AV08220          && Imprime Relat¢rio das Vendas por Cliente em um Per¡odo
    CASE NUMIN2 = 3
      *DO VENDADIA        && Imprime Relat¢rio das Vendas por Banco e Carteira em um Per¡odo
      DO AV08230
    CASE NUMIN2 = 4  
      *DO ATRASOS2        && Imprime Relat¢rio dos Atrasos por Cliente
      DO AV08240
    CASE NUMIN2 = 5
      *DO VALREC          && Imprime Relat¢rio dos Valores Recebidos por Cliente
      DO AV08251
    CASE NUMIN2 = 6
      DO AV08264         && Relatorio de Duplicatas em Aberto por Periodo
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
  PRIVATE nLin
NUMHOLD = 0
*
=NOMEMOD( 'AV08200' )
SAVE SCREEN TO MAIN2_SCR
SET MESS TO 23 CENTER
*
nLin = 10
IF CLR_SCR
  SET MESSAGE TO 23 CENTER
  DO JANELA WITH nLin,37,nLin+7,76
  DO BORDA WITH nLin,37,nLin+7,76
*
  @ nLin+1,40 PROMPT "a - Posi‡„o Financeira de Clientes" MESSAGE ;
    "Imprime a Posi‡„o Financeira dos Clientes"
  @ nLin+2,40 PROMPT "b - Vendas no Per¡odo por Cliente " MESSAGE ;
    "Imprime Relat¢rio das Vendas por Cliente em um Per¡odo"
  @ nLin+3,40 PROMPT "c - Vendas por Banco e Carteira   " MESSAGE ;
    "Imprime Relat¢rio das Vendas por Banco e Carteira em um Per¡odo"
  @ nLin+4,40 PROMPT "d - Atrasos por Cliente           " MESSAGE ;
    "Imprime Relat¢rio dos Atrasos por Cliente"
  @ nLin+5,40 PROMPT "e - Valores recebidos por Cliente " MESSAGE ;
    "Imprime Relat¢rio dos Valores Recebidos por Cliente"
  @ nLin+6,40 PROMPT "f - Duplicatas Vencidas           " MESSAGE ;
    "Imprime Relat¢rio das Duplicatas em Aberto por Periodo"

*
  MENU TO NUMHOLD
ENDIF
*
RETURN(NUMHOLD)
