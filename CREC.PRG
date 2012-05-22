* CREC - CONTAS A RECEBER
* ADAO ELI PEREIRA - 02/10/93
*
DO WHILE .T.
   NUMIN2 = MENUDUP_IN(.T.)
   DO CASE
      CASE NUMIN2 = 1                 &&---   a - Duplicatas
         DO AV08000     && MDUPLIC
      CASE NUMIN2 = 2                 &&---   b - Estatisticas de Clientes
         DO MESTATIS
      CASE NUMIN2 = 3                 &&---   c - Posi‡ao por Banco
         DO MESTBCO
      CASE NUMIN2 = 4                 &&---   d - Relat¢rios Cont beis
         DO MRELCONT
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
NUMHOLD = 0
*
@ 02,70 SAY '  CREC'
SAVE SCREEN TO MAIN2_SCR
SET MESS TO 23 CENTER
SET INTE ON
*
IF CLR_SCR
   SET MESSAGE TO 23 CENTER
   DO JANELA WITH 07,19,17,53
   DO BORDA WITH 07,19,17,53
*
*
   @ 09,22 PROMPT "a - Duplicatas              " MESSAGE ;
   "Imprime, Reimprime, Consulta, Exclui, Recebe Duplicatas"
   @ 11,22 PROMPT "b - Estat¡sticas de Clientes" MESSAGE ;
   "Posi‡ao dos Clientes, Debitos, Atrasos, Valores Recebidos"
   @ 13,22 PROMPT "c - Posi‡ao por Banco       " MESSAGE ;
   "Consulta situa‡„o das Duplicatas e Clientes por Banco"
   @ 15,22 PROMPT "d - Relat¢rios Cont beis    " MESSAGE ;
   "Define Competˆncia, Razao por Fornecedor e Registro de Sa¡das"
*
   MENU TO NUMHOLD
ENDIF
*
RETURN(NUMHOLD)
