* AV04000 - MENU DE CLIENTES
* SISTEMA ADMINISTRACAO DE VENDAS
* ADAO ELI PEREIRA - 12-04-90
*
DO WHILE .T.

  NUMIN3 = MENU22_IN(.T.)
  DO CASE
    CASE NUMIN3 = 1        &&---- a - Atualizacao do cadastro
      DO AV04107
    CASE NUMIN3 = 2        &&---- b - Impress„o dos Relat¢rios
      DO AV04300
    CASE NUMIN3 = 3        &&---- c - Consulta Clientes cadastrados
      DO AV04400
    CASE NUMIN3 = 4        &&---- d - Situa‡„o dos Clientes
      DO AV04502
    CASE NUMIN3 = 5        &&---- e - Inativar Clientes por Regiao
      DO AV04600           
    CASE NUMIN3 = 6        &&---- f - Inativar Clientes por Estado
      DO AV04701
    CASE NUMIN3 = 7        &&---- g - Relatorio de Clientes por CEP/End
      DO AV04801           
    CASE NUMIN3 = 8        &&---- h - Atualizar Data de Ult.Compra SP'
      DO AV04900           
   
    CASE LASTKEY() = 27
      RESTORE SCREEN FROM MAIN_SCR
      RETURN
  ENDCASE
      RESTORE SCREEN FROM MAIN2_SCR
ENDDO
QUIT
******************* FIM  DO  PROGRAMA  *************************
                    FUNCTION MENU22_IN
****************************************************************
PARAMETERS CLR_SCR
NUMHOLD = 0
*
SET MESS TO 23 CENTER
=NOMEMOD( 'AV04000')
SAVE SCREEN TO MAIN2_SCR
*
IF CLR_SCR
  DO JANELA WITH 12,20,21,66
  DO BORDA WITH 12,20,21,66
*
@ 13,23 PROMPT "a - "+ATUCAD   MESSAGE  INCALTEXC
@ 14,23 PROMPT "b - Impress„o dos Relat¢rios     " MESSAGE ;
  "Imprime Relat¢rios por ordem de C¢digo e Alfab‚tica"
@ 15,23 PROMPT "c - Consulta Clientes cadastrados" MESSAGE ;
  "Consulta na Tela pelo C¢digo, Raz„o Social ou Nome de Guerra"
@ 16,23 PROMPT "d - Situa‡„o dos Clientes        " MESSAGE ;
  "Resumo das situa‡„o de Clientes por Representante e Regi„o"
@ 17,23 PROMPT "e - Inativar Clientes  por Regiao " MESSAGE ;
  "Inativar clientes baseado no ultimo mes de compra"
@ 18,23 PROMPT "f - Inativar Clientes  por Estado " MESSAGE ;
  "Inativar clientes baseado no ultimo mes de compra"
@ 19,23 PROMPT "g - Relatorio de Clientes por CEP/Endereco " MESSAGE ;
  "Relatorio de Clientes por CEP / Endereco" 
@ 20,23 PROMPT "h - Atualizar Data Ult.Compra SP " MESSAGE ;
  "Atualiza data de ultima compra AVSP"
  
*
MENU TO NUMHOLD
ENDIF
*
RETURN(NUMHOLD)
