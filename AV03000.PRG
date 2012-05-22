* AV03000 - MENU DE VENDEDORES
* SISTEMA ADMINISTRACAO DE VENDAS
* ADAO ELI PEREIRA - VERSAO 1.2 - 24/10/91
*
DO WHILE .T.
   NUMIN4 = MENU4_IN(.T.)
   DO CASE
      CASE NUMIN4 = 1    &&--- a - Atualiza‡„o do Cadastro
         *DO AV03101
         DO USEWINDOWS
      CASE NUMIN4 = 2    &&--- b - Relat¢rios do Cadastro
         DO AV03300
      CASE NUMIN4 = 3    &&--- c - Relat¢rio Clientes por Representante
         DO AV03400
      CASE NUMIN4 = 4    &&--- d - Comiss„o dos Representantes
         vopcao = SelecRelat()
         if vopcao # 0
           =AV03514()
         endif
      CASE NUMIN4 = 5    &&--- e - Relat¢rio de Dupl.com Vencto no periodo 
         DO AV03601
      CASE LASTKEY() = 27
         RESTORE SCREEN FROM MAIN_SCR
         RETURN
   ENDCASE
   RESTORE SCREEN FROM MAIN2_SCR
ENDDO
QUIT
******************* FIM  DO  PROGRAMA  *************************
FUNCTION MENU4_IN
****************************************************************
PARAMETERS CLR_SCR
NUMHOLD = 0
*
=NOMEMOD( 'AV03000' )
SAVE SCREEN TO MAIN2_SCR
SET MESS TO 23 CENTER
*
IF CLR_SCR
   SET MESSAGE TO 23 CENTER
*  SET COLOR SET TO COR2
   DO JANELA WITH 11,20,19,66
   DO BORDA WITH 11,20,19,66
   @ 13,23 PROMPT "a - Atualiza‡„o do Cadastro             " MESSAGE  INCALTEXC
   @ 14,23 PROMPT "b - Relat¢rios do Cadastro              " MESSAGE ;
   "Imprime Relat¢rios dos Representantes por Ordem de C¢digo e Alfab‚tica"
   @ 15,23 PROMPT "c - Relat¢rio Clientes por Representante" message ;
   "Imprime Relat¢rio dos Clientes cadastrados para cada Representante"
   @ 16,23 PROMPT "d - Comiss„o dos Representantes         ";
   message "Calcula e imprime Relat¢rio das Comiss”es dos Representantes"
   @ 17,23 PROMPT "e - Relatorio de Duplicatas do Periodo  ";
   message "Imprime Relat¢rio das Duplicatas que vencem no periodo da Comissao"
   
   MENU TO NUMHOLD
ENDIF
RETURN(NUMHOLD)

*******************
Function SelecRelat
*******************
  =janela(16,49,21,69)
  =borda(16,49,21,69)
  @18,51 prompt 'Relat¢rio Normal' message 'Imprime Comiss”es de Todos os Representantes ou S¢ Alguns'
  @19,51 prompt '    S¢ Resumo   ' message 'Imprime s¢ o Resumo Final de Todas as Comiss”es'
  menu to vopcao
return vopcao

