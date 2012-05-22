* AV09000 - UTILITARIOS
* SISTEMA ADMINISTRACAO DE VENDAS
* ADAO ELI PEREIRA - 06/04/90
*
DO WHILE .T.
   NUMIN2 = MENUIN_IN(.T.)
   DO CASE
      CASE NUMIN2 = 1              &&  a - Classifica‡„o dos Dados
         DO AV09100
      CASE NUMIN2 = 2              &&  b - Limpeza mensal dos Arquivos
         DO AV09300
      CASE NUMIN2 = 3              &&  c - Troca de Senha autorizada
         DO SCC_SENH
      CASE NUMIN2 = 4              &&  d - Define padr„o de Impressora
         DO AV09900
      CASE NUMIN2 = 5              &&  e - Altera cores da Tela do Sistema
         DO DEFCORES
      CASE NUMIN2 = 6              &&  f - Backup dos Cadastros e Arquivos
         DO AV09200
      CASE NUMIN2 = 7              &&  f - Restaura o Backup
         DO AV09400
      CASE NUMIN2 = 8              &&  g - Formatar disquetes no drive B: 
        save screen
        clear
        @01,00 say 'Para abortar esta operacao pressione Ctrl C'
        run format b: /u
        rest screen      
        CASE LASTKEY() = 27
          RESTORE SCREEN FROM MAIN_SCR
          RETURN
      CASE NUMIN2 = 9              &&  h - Verificar integridade de Duplicatas/Notas Fiscais
         DO CHKGERAL
 *     CASE NUMIN2 = 10
 *        DO SCICGC   
   ENDCASE
   RESTORE SCREEN FROM MAIN2_SCR
ENDDO
QUIT
* --------------------------------------------------------------------
FUNCTION MENUIN_IN
* --------------------------------------------------------------------
PARAMETERS CLR_SCR
PRIVATE LINI, COLI, COLF, LINF
NUMHOLD = 0

*
=NOMEMOD( 'AV09000' )
SAVE SCREEN TO MAIN2_SCR
SET MESS TO 23 CENTER
*
IF CLR_SCR
   SET MESSAGE TO 23 CENTER
   LINI = 9
   COLI = 20   
   LINF = LINI + 10
   COLF = COLI + 53
   =JANELA(LINI,COLI,LINF,COLF)
   =BORDA(LINI,COLI,LINF,COLF)
   @LINI+1,COLI+3 PROMPT "a - Classifica‡„o dos Dados        " MESSAGE ;
   "Exclui ¡ndices velhos e refaz ¡ndices dos Arquivos e Cadastros do Sistema"
   @LINI+2,COLI+3 PROMPT "b - Limpeza mensal dos Arquivos    " MESSAGE ;
   "Elimina dados sem utiliza‡„o do Estoque, Pedidos, N.Fiscais e Duplicatas"
   @LINI+3,COLI+3 PROMPT "c - Troca de Senha autorizada      " MESSAGE ;
   "Altera e mant‚m a Senha autorizada para utiliza‡„o do Sistema"
   @LINI+4,COLI+3 PROMPT "d - Define padr„o de Impressora    "MESSAGE ;
   "Define se Impressora padr„o Epson, Rima/Emilia ou Laser"
   @LINI+5,COLI+3 PROMPT "e - Altera cores da Tela do Sistema" MESSAGE ;
   "Altera a composi‡„o das cores da Tela do Sistema"
   @LINI+6,COLI+3 PROMPT "f - Backup dos Cadastros e Arquivos" MESSAGE ;
   "Gera Backup Compactado dos Cadastros e Arquivos do Sistema"
   @LINI+7,COLI+3 PROMPT "g - Restaura os Arquivos do Back-up"        MESSAGE;
   "Restaura os arquivos copiados pela rotina de Back-up"
   @LINI+8,COLI+3 PROMPT "h - Formatar disquetes 3« HD (1.44)" MESSAGE ;
   "Formata disquetes no drive B: com capacidade 1.44 MB"
   @LINI+9,COLI+3 PROMPT "i - Checar Integridade Notas Fiscais e Duplicatas" MESSAGE ;
   "Verifica inconsistencias entre Notas Fiscais e Duplicatas"
*   @LINI+10,COLI+3 PROMPT "j - Listagem digitada de Clientes CGC" MESSAGE ;
*   "Listagem de Cliente digitada por CGC" 
   MENU TO NUMHOLD
ENDIF
*
RETURN(NUMHOLD)
