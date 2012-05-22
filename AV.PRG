* ADMV - MENU DO SISTEMA ADMINISTRACAO DE VENDAS
* ADAO ELI PEREIRA - VERSAO 1.5 - 03/03/91  05/03/91  22/06/91 09/06/93
*
PUBLIC LP, CalcDifDup, HOJE, MRAZAOSOC

FLUSH
CLOSE DATA
CLOSE ALL
SEPARATOR = "."
HOURS     = 24
* Acumula a diferenca das Duplicatas para a primeira Duplicata
CalcDifDup = .F.
*SET DISPLAY TO VGA25
set century on
SET BELL ON
SET BLINK ON
SET CLOCK TO 01,70
SET CONFIRM ON
SET CONSOLE OFF
SET DATE BRIT
SET TALK OFF
SET DELETE ON
SET DELIMITER OFF
SET DEVICE TO SCREEN
SET ESCAPE ON

SET EXCLUSIVE ON
SET HEADINGS OFF
SET MENU OFF
SET NOTIFY OFF
SET OPTIMIZE ON
SET PRINT OFF
SET RESOURCE ON
SET SAFETY OFF
SET SCORE OFF
SET STATUS OFF
_FOXDOC   = ""
_FOXGRAPH = ""
_GENMENU  = ""
_GENGRAPH = ""
_GENPD    = ""
_GENSCRN  = ""
_GENXTAB  = ""
_STARTUP  = ""

CLEAR
SET DISP TO VGA25
SET INTENSITY ON
SET PROCEDURE TO ADMVFUNC
_PEJECT  = "NONE"
_PLENGTH = 66
_PLINENO = 0
_PAGENO  = 1
_PCOPIES = 1
_PBPAGE  = 1
_PEPAGE  = 1
_PSCODE  = ""
_PECODE  = ""

ON KEY LABEL F1 DO BROWARQ

* ON KEY = 315 DO TESTE
*RET = CHR(17) + CHR(117)
DO TELAI
Versao = '2.00.00'
USE ARQCORES
PAR1_P1 = RTRIM(CORTELA)+RTRIM(CORTELAB)
PAR1_P2 = RTRIM(CORLINHA)+RTRIM(CORLINHAB)
if PAR1_P1 + PAR1_P2 <> SPACE(02)
   PAR1 = (PAR1_P1 +'/'+PAR1_P2) + ','
ELSE
   PAR1 = ','
endif
PAR2_P1 = RTRIM(SAYLETRA)+RTRIM(SAYLETRAB)
PAR2_P2 = RTRIM(SAYREVERSO)+RTRIM(SAYREVERSB)
if PAR2_P1 + PAR2_P2 <> SPACE(02)
   PAR2 = (PAR2_P1 +'/'+PAR2_P2) + ','
ELSE
   PAR2 = ','
endif

PAR3_P1 = RTRIM(BORDA1)+RTRIM(BORDA1B)
PAR3_P2 = RTRIM(BORDA2)+RTRIM(BORDA2B)
if PAR3_P1 + PAR3_P2 <> SPACE(02)
   PAR3 = (PAR3_P1 +'/'+PAR3_P2) + ','
ELSE
   PAR3 = ','
endif
PAR4_P1 = RTRIM(GETLETRA)+RTRIM(GETLETRAB)
PAR4_P2 = RTRIM(GETREVERSO)+RTRIM(GETREVERSB)
if PAR4_P1 + PAR4_P2 <> SPACE(02)
   PAR4 = (PAR4_P1 +'/'+PAR4_P2) + ','
   CABCOLOR = PAR4
ELSE
   PAR4 = ','
   CABCOLOR = PAR4
endif
PAR5_P1 = RTRIM(CORMENSAG1)+RTRIM(CORMENSAB1)
PAR5_P2 = RTRIM(CORMENSAG2)+RTRIM(CORMENSAB2)
if PAR5_P1 + PAR5_P2 <> SPACE(02)
   PAR5 = (PAR5_P1 +'/'+PAR5_P2)
ELSE
   PAR5 = ','
endif
MCORSOMB1 = CORSOMB1
MCORSOMB2 = CORSOMB2
BCORSOMB1 = BCORSOM1
BCORSOMB2 = BCORSOM2
PAR0_P1 = RTRIM(CORSOMB1)+RTRIM(BCORSOM1)
PAR0_P2 = RTRIM(CORSOMB2)+RTRIM(BCORSOM2)
if PAR0_P1 + PAR0_P2 <> SPACE(02)
   CORSCHE = (PAR0_P1 +'/'+PAR0_P2)
ELSE
   CORSCHE = ','
endif
DEFCORES = PAR1+PAR2+PAR3+PAR4+PAR5+CORSCHE
SET COLOR TO &DEFCORES
*     SET COLOR OF SCHEME 6 TO &PAR4
CLEAR
LINTELA = PAR3_P1+'/'+PAR2_P2
CORARQ = 'W+/R+'
CORVER = 'W+/G'
CORPISCA = 'r*/g'
CORAVISO = 'W+/R'
*
* Data com anos entrados com dois digitos sao interpretados da seguinte forma:
* Se o Ano >= 00      And Ano <= AnoPivot  Ex: 01/02/03 => 01/02/2003
* Se o Ano > AnoPivot And Ano <= 99        Ex: 01/02/98 => 01/02/1998
AnoPivot = 50
HOJE     = DATE()
DO TELAG
VNOMEPRG = 'AV'
DTLANC = {}

cDeveloper = GETENV('AV')
sele 0
USE CTR_SENH
index on programa + senha to ctr_senh.idx
IF UPPER(cDeveloper) <> 'JAIR'
  do senha
  DTLANC = CTOD(space(8))
  MDATE  = HOJE

  DO WHILE .NOT. DTLANC = MDATE
    =JANELA(18,56,21,78)
    =BORDA(18,56,21,78)
    =MENSAG('Informe a Data do Sistema no formato [DD/MM/AAAA]')
    @19,59 say ' Data do Sistema'
    @20,64 GET DTLANC 
    READ
    if DTLANC = MDATE
      EXIT
    ELSE
      =MENSAG ('Data do Sistema ‚ '+dtoc(mdate)+' - ENTER e reinforme')
      DTLANC = {}
      =INKEY(0)
    ENDIF
  ENDDO
ELSE
  DTLANC = HOJE  
ENDIF  
*
* Se o for um novo mes Acumula os Pedidos em Aberto
cMES = STR(MONTH(DTLANC),2)
cANO = STR(YEAR(DTLANC),4)
MULTDATA = CTOD( '01/' + cMes + '/' + cAno ) - 1
SELE 0
USE PEDABERT ORDER P_DATA
GO BOTTOM
IF MULTDATA > DATA
  =AtualPedAberto()   
ENDIF
USE  

DO TELAG
  
USE PARAM
TIPOIMPR = PADRIMPR
close data
@04,01 CLEAR TO 21,78

LP=GETENV("LP")
if empty(LP) 
   LP = "PRN"
endif   

DO WHILE .T.
   NUMIN = MENU_IN(.T.)
   DO CASE
      CASE NUMIN = 1        &&------ a - Estados da Federa‡„o (UF)
         DO AV01000
      CASE NUMIN = 2        &&------ b - Transportadoras
         DO AV02000
      CASE NUMIN = 3        &&------ c - Representantes
         DO AV03000
      CASE NUMIN = 4        &&------ d - Clientes
         DO AV04000
      CASE NUMIN = 5        &&------ e - Produtos
         DO AV05000
      CASE NUMIN = 6        &&------ f - Arquivos e Parƒmetros
         DO AV12000
      CASE NUMIN = 7        &&------ g - Pedidos de Clientes
         DO AV06000
      CASE NUMIN = 8        &&------ h - Notas Fiscais
         DO AV07000
      CASE NUMIN = 9        &&------ i - Contas a Receber
         * DO CREC
         DO AV08000
      CASE NUMIN = 10       &&------ j - Estat¡sticas de Vendas
         DO AV10000
      CASE NUMIN = 11       &&------ k - Relat¢rios Cont beis
         DO MRELCONT
      CASE NUMIN = 12       &&------ u - Utilit rios
         DO AV09000
      CASE LASTKEY() = 27
         _PLINENO = 0
         CLOS DATA
         USE ARQREL10
         ZAP
         USE ARQREL80
         ZAP
         if FILE("CADWORK.DBF")
          * ERASE CADWORK.DBF
         endif
         if FILE("CADWORK.FPT")
           * ERASE CADWORK.FPT
         endif
         set color to
         exit
   ENDCASE
   RESTORE SCREEN FROM MAIN_SCR
ENDDO
=FINALIZA()
return

*================
FUNCTION FINALIZA
*================
  set printer to
  DO RESTAMB
  ON ERROR 
  CLEAR ALL
  CLOSE ALL
  CLEAR
RETURN  


*********************************************************

****************************************************************
FUNCTION MENU_IN
****************************************************************
PARAMETERS CLR_SCR
NUMHOLD = 0
=NOMEMOD( 'AV' )
SAVE SCREEN TO MAIN_SCR
if CLR_SCR
   SET message TO 23 CENTER
   DO JANELA WITH 05,00,20,37
   DO BORDA WITH 05,00,20,37
   @07,03 PROMPT "a - Estados da Federa‡„o (UF)   " message(INCALTEXC)
   @08,03 PROMPT "b - Transportadoras             " message(INCALTEXC)
   @09,03 PROMPT "c - Representantes              " message(INCALTEXC)
   @10,03 PROMPT "d - Clientes                    " message(INCALTEXC)
   @11,03 PROMPT "e - Produtos                    " message(INCALTEXC)
   @12,03 PROMPT "f - Arquivos e Parƒmetros       " message ;
     "Bancos e Agˆncias, Moedas e Taxas de Convers„o"
   @13,03 PROMPT "g - Pedidos de Clientes         " message(INCALTEXC)
   @14,03 PROMPT "h - Notas Fiscais               " message(INCALTEXC)
   @15,03 PROMPT "i - Contas a Receber            " message(INCALTEXC)
   @16,03 PROMPT "j - Estat¡sticas de Vendas      " message(INCALTEXC)
   @17,03 PROMPT "k - Relat¢rios Cont beis        " message ;
     "Define a Competˆncia e imprime o Registro de Sa¡das"
   @18,03 PROMPT "u - Utilit rios                 " message ;
     "Cria INDEX, BACKUP/REST, formata PRINTER, atualiza SENHA e limpa CADASTROS"
   MENU TO NUMHOLD
endif
return(NUMHOLD)


*================
PROCEDURE MSGERRO
*================
  PRIVATE WTELA
  
  DEFINE WINDOW WTELA FROM 10, 10 TO 17, 70;
         TITLE "ERRO DE SISTEMA";
         COLOR 'W+/R'
  ACTIVATE WINDOW WTELA
  @ 00, 00 SAY 'MODULO..: ' + PROGRAM(2)
  @ 01, 00 SAY 'MENSAGEM: ' + MESSAGE()
  @ 02, 00 SAY 'ERRO....: ' + MESSAGE(1)
  @ 05, 00 SAY 'Tecle algo para encerrar...'
  =INKEY(0)
  HIDE WINDOW WTELA
  DEACTIVATE WINDOW WTELA
  =FINALIZA()
  CANCEL
RETURN  
