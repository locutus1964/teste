* AV05990 - MENU DOS PRECOS
* SISTEMA ADMINISTRACAO DE VENDAS
* ADAO ELI PEREIRA - 15/05/90
*
TITULO = '***    Tabelas de Pre‡os     ***'
*
DO WHILE .T.
  NUMIN5 = MENU6_IN(.T.)
  
    IF NUMIN5 = 1      && TABELAS DE PRECOS POR PRODUTO
      DO AV05994
    ENDIF
    
    IF NUMIN5 = 2      && DESCONTOS POR GRUPO
      DO AV05993
    ENDIF
    
    IF NUMIN5 = 3      && DESCONTOS SEMANAIS
      DO AV059941
    ENDIF
    
    IF NUMIN5 = 4      && CUSTOS FINANCEIROS
*     DO AV059942  - CUSTO FINANCEIRO ANTERIOR AO DEFLATOR DO ICMS - 14/2/94      
      DO AV059952
    ENDIF
    
    IF NUMIN5 = 5      && TABELA DE EXCLUSAO DO ICM EM PARCELAS
      DO AV059953
    ENDIF
    
    IF NUMIN5 = 6      && TABELA DE EXCLUSAO DO ICM EM PARCELA UNICA
      DO AV059954
    ENDIF
    
   IF NUMIN5 = 7      && IMPRESSAO DAS TABELAS
      DO AV05995          && TABELA DE PRECOS ANTERIOR AO DEFLATOR DO ICMS
*     DO TAB_PREC
   ENDIF
    
    IF LASTKEY() = 27
      RESTORE SCREEN FROM MAIN_SCR
      RETURN
    ENDIF
      RESTORE SCREEN FROM MAIN2_SCR
ENDDO
QUIT
*---------------------------------------------------------------
FUNCTION MENU6_IN
*---------------------------------------------------------------
PARAMETERS CLR_SCR
NUMHOLD = 0
*
=NOMEMOD( 'AV05990' )
SAVE SCREEN TO MAIN2_SCR
*
IF CLR_SCR
  SET MESSAGE TO 23 CENTER
  ? JANELA(09,35,19,78)
  ? BORDA(09,35,19,78)
*
*
@ 11,38 PROMPT "a - Atualiza‡„o dos Pre‡os por Produto " MESSAGE ;
  'Inclui, Altera, Consulta ou Exclui Pre‡os por Tabela e Produto'
@ 12,38 PROMPT "b - Atualiza‡„o dos Descontos por Grupo" MESSAGE ;
  'Inclui, Altera, Consulta ou Exclui Descontos por Grupo de Produtos'
@ 13,38 PROMPT "c - Atualiza‡„o dos Descontos Semanais " MESSAGE ;
  'Inclui, Altera, Consulta ou Exclui Tabela de Descontos Semanais'
@ 14,38 PROMPT "d - Atualiza‡„o dos Custos Financeiros " MESSAGE ;
  'Inclui, Altera, Consulta ou Exclui Custos Financeiros'
@ 15,38 PROMPT "e - Custo Financ. Governo - em Parcelas" MESSAGE ;
  'Inclui, Altera, Consulta ou Exclui Acr‚scimo Financeiro - em Parcelas'
@ 16,38 PROMPT "f - Custo Financ. Governo - Parc. Unica" MESSAGE ;
  'Inclui, Altera, Consulta ou Exclui Acr‚scimo Financeiro - Parcela Unica'
@ 17,38 PROMPT "g - Impress„o das Tabelas de Pre‡os    " MESSAGE ;
 'Imprime Tabelas de Pre‡os Geral ou Selecionada por ordem de C¢digo'
*
MENU TO NUMHOLD
ENDIF
*
RETURN(NUMHOLD)


