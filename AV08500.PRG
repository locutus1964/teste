* AV08500 - MENU DO RELATORIO DE FATURAMENTO
* JAIR GONÄALVES
*
DO WHILE .T.
   NUMIN2 = MENUDUP_IN(.T.)
   DO CASE
      CASE NUMIN2 = 1                 &&---   a - Relat¢rio por Data de Faturamento
         DO AV08510
      CASE NUMIN2 = 2                 &&---   b - Relat¢rio por Regi∆o/Valor de Venda 
         DO AV08520
      CASE NUMIN2 = 3                 &&---   c - Relat¢rio Resumido por Mes/Regiao
         DO AV08530
      CASE NUMIN2 = 4                 &&---   d - Relat¢rio Resumido por Mes/Representante
         DO AV08540
      CASE NUMIN2 = 5                 &&---   e - Relat¢rio de Ativo Realizavel
         DO AV08560
      CASE LASTKEY() = 27
         RESTORE SCREEN FROM MAIN_SCR
         RETURN
   ENDCASE
   RESTORE SCREEN FROM MAIN2_SCR
ENDDO
QUIT
* --------------------------------------------------------------------
FUNCTION MENUDUP_IN
* --------------------------------------------------------------------
PARAMETERS CLR_SCR
PRIVATE LIN, COL
NUMHOLD = 0
*
=NOMEMOD( 'AV08500' )
SAVE SCREEN TO MAIN2_SCR
SET MESS TO 23 CENTER
SET INTE ON
*
NITENS = 5
IF CLR_SCR
   LIN = 12
   COL = 31
   SET MESSAGE TO 23 CENTER
   DO JANELA WITH LIN,COL,LIN+NITENS+1,COL+43
   DO BORDA WITH LIN,COL,LIN+NITENS+1,COL+43
*
*
   @ LIN+1,COL+3 PROMPT "a - Relat¢rio por Data de Faturamento " MESSAGE ;
   "Imprime Relat¢rio baseado no dia de Faturamento"
   @ LIN+2,COL+3 PROMPT "b - Relat¢rio por Regi∆o / Valor" MESSAGE ;
   "Imprime Relat¢rio baseado na Regi∆o de Vendas"
   @ LIN+3,COL+3 PROMPT "c - Relat¢rio Resumido por Mes/Regi∆o" MESSAGE ;
   "Imprime Relat¢rio Resumido por mes de Faturamento e Regi∆o"
   @ LIN+4,COL+3 PROMPT "d - Relat¢rio Resumido por Mes/Repres." MESSAGE ;
   "Imprime Relat¢rio Resumido por mes de Faturamento e Representante"
   @ LIN+5,COL+3 PROMPT "e - Relat¢rio de Ativo Realiz†vel" MESSAGE ;
   "Imprime Relat¢rio de Dupl.Faturadas em Atraso e em Aberto"
   
   
   

*
   MENU TO NUMHOLD
ENDIF
*
RETURN(NUMHOLD)
