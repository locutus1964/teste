* CREC - CONTAS A RECEBER
* ADAO ELI PEREIRA - 02/10/93
*
DO WHILE .T.
   NUMIN2 = MENUDUP_IN(.T.)
   DO CASE
      CASE NUMIN2 = 1                 &&---   a - Duplicatas
         DO AV08100
      CASE NUMIN2 = 2                 &&---   b - Estatisticas de Clientes
         DO AV08200
      CASE NUMIN2 = 3                 &&---   c - Posi‡ao por Banco
         DO AV08300
      CASE NUMIN2 = 4                 &&---   d - Relat¢rios Cont beis
         DO AV08400
      CASE NUMIN2 = 5                 &&---   e - Relat¢rios de Faturamento
         DO AV08500
      CASE NUMIN2 = 6                 &&---   f - Gerar Dados para o SCI
         DO AV08600
      CASE NUMIN2 = 7                 &&---   g - Exportar Clientes p/ HSBC
         DO AV08700
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
=NOMEMOD( 'AV08000' )
SAVE SCREEN TO MAIN2_SCR
SET MESS TO 23 CENTER
SET INTE ON
*
IF CLR_SCR
   LIN = 10
   COL = 29
   LINF = LIN + 8
   COLF = COL + 35
   SET MESSAGE TO 23 CENTER
   DO JANELA WITH LIN,COL,LINF,COLF
   DO BORDA WITH LIN,COL,LINF,COLF
*
*
   @ LIN+1,COL+3 PROMPT "a - Duplicatas              " MESSAGE ;
   "Imprime, Reimprime, Consulta, Exclui, Recebe Duplicatas"
   @ LIN+2,COL+3 PROMPT "b - Estat¡sticas de Clientes" MESSAGE ;
   "Posi‡ao dos Clientes, Debitos, Atrasos, Valores Recebidos"
   @ LIN+3,COL+3 PROMPT "c - Posi‡ao por Banco       " MESSAGE ;
   "Consulta situa‡„o das Duplicatas e Clientes por Banco"
   @ LIN+4,COL+3 PROMPT "d - Relat¢rios Cont beis    " MESSAGE ;
   "Define Competˆncia, Razao por Fornecedor e Registro de Sa¡das"
   @ LIN+5,COL+3 PROMPT "e - Relat¢rio de Faturamento" MESSAGE ;
   "Emite Relat¢rios do Faturamento por Per¡odo ou RegiÆo"
   @ LIN+6,COL+3 PROMPT "f - Gerar Dados para o SCI" MESSAGE ;
   "Grava em disquete os dados para o SCI"
   @ LIN+7,COL+3 PROMPT "g - Exportar Clientes para HSBC" MESSAGE;
   "Exporta dados de Clientes para a Cobranca On-Line do HSBC Bamerindus"

*
   MENU TO NUMHOLD
ENDIF
*
RETURN(NUMHOLD)
