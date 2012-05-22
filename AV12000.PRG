* AV12000 - MENU DE ARQUIVOS AUXILIARES
* SISTEMA ADMINISTRACAO DE VENDAS
* ADAO ELI PEREIRA - 20/10/92
*
DO WHILE .T.
  NUMIN2 = MENUAR_IN(.T.)
  DO CASE

    CASE NUMIN2 = 1
      DO BANCO

    CASE NUMIN2 = 2
      DO MOEDA

    CASE NUMIN2 = 3
      DO MENUCPAG
      
    CASE NUMIN2 = 4  
      DO MENUCONC
      
    CASE NUMIN2 = 5
      DO MENUSTAT
      
    CASE NUMIN2 = 6
      DO MENUATIV
      
    CASE NUMIN2 = 7
      *DO MENUREGI
      DO USEWINDOWS
      
    CASE NUMIN2 = 8
      DO MENUSIGL  
      
    CASE NUMIN2 = 9
      DO MENUCARG
 
    CASE NUMIN2 = 10
      DO MENULPRO
             
    CASE NUMIN2 = 11
      DO MENUUNID
      
    CASE NUMIN2 = 12
      DO MENUEMBA
      
    CASE NUMIN2 = 13
      DO MENUCLAS
      
    CASE NUMIN2 = 14      
      DO MENUGRUP
      
    CASE NUMIN2 = 15      
      DO MENUVEIC
      
    CASE LASTKEY() = 27
      CLEAR
      RESTORE SCREEN FROM MAIN_SCR
      RETURN
  ENDCASE
      RESTORE SCREEN FROM MAIN2_SCR
ENDDO
QUIT
************************************************************************
FUNCTION MENUAR_IN
************************************************************************
PARAMETERS CLR_SCR
NUMHOLD = 0
*
=NOMEMOD( 'CR12000' )
SAVE SCREEN TO MAIN2_SCR
SET MESS TO 23 CENTER
*
IF CLR_SCR
*  DO COR2
  DO JANELA WITH 05,20,21,56
  DO BORDA WITH 05,20,21,56
*
    @ 06,23 PROMPT "a - Bancos e Agˆncias          " MESSAGE ;
      "Inclus„o, Consulta, Altera‡„o e Exclus„o de Bancos e Agˆncias"
    @ 07,23 PROMPT "b - Moedas e Taxas de Convers„o" MESSAGE ;
      "Inclus„o, Consulta, Altera‡„o e Exclus„o de Moedas e Taxas de Convers„o"
    @ 08,23 PROMPT "c - Condi‡”es de Pagamento     " MESSAGE ;
      "Manuten‡„o das Condi‡”es de Pagamento para Venda aos Clientes"
    @ 09,23 PROMPT "d - Conceito de Clientes       " MESSAGE ;
      "Inclus„o, Consulta, Altera‡„o e Exclus„o do Conceito de Clientes"
    @ 10,23 PROMPT "e - Status de Clientes         " MESSAGE ;
      "Inclus„o, Consulta, Altera‡„o e Exclus„o do Status de Clientes"
    @ 11,23 PROMPT "f - Atividades do Cliente      " MESSAGE ;
      "Inclus„o, Consulta, Altera‡„o e Exclus„o da Atividade de Clientes"
    @ 12,23 PROMPT "g - Regi”es dos Clientes       " MESSAGE ;
      "Inclus„o, Consulta, Altera‡„o e Exclus„o de Regi”es de Clientes"
    @ 13,23 PROMPT "h - Siglas dos Funcion rios     " MESSAGE ;
      "Inclus„o, Consulta, Altera‡„o e Exclus„o de Siglas dos Funcion rios"
    @ 14,23 PROMPT "i - Cargos dos Clientes/Repres. " MESSAGE ;
      "Inclus„o, Consulta, Altera‡„o e Exclus„o de Cargos dos Contatos"
    @ 15,23 PROMPT "j - Linhas de Produtos          " MESSAGE ;
      "Inclus„o, Consulta, Altera‡„o e Exclus„o de Linhas de Produtos"
    @ 16,23 PROMPT "k - Unidades de Medida          " MESSAGE ;
      "Inclus„o, Consulta, Altera‡„o e Exclus„o de "
    @ 17,23 PROMPT "l - Tipos de Embalagem          " MESSAGE ;
      "Inclus„o, Consulta, Altera‡„o e Exclus„o de Tipos de Embalagem"
    @ 18,23 PROMPT "m - Classifica‡„o Fiscal do IPI " MESSAGE ;
      "Inclus„o, Consulta, Altera‡„o e Exclus„o de Classifica‡„o Fiscal do IPI"
    @ 19,23 PROMPT "n - Grupos de Produtos          " MESSAGE ;
      "Inclus„o, Consulta, Altera‡„o e Exclus„o de Grupos de Produtos"
    @ 20,23 PROMPT "o - Ve¡culos utilizados         " MESSAGE ;
      "Inclus„o, Consulta, Altera‡„o dos Ve¡culos utilizados nas Entregas"
*
  MENU TO NUMHOLD
ENDIF
*
RETURN(NUMHOLD)
