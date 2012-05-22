* ADMV - MENU DO SISTEMA ADMINISTRACAO DE VENDAS
* ADAO ELI PEREIRA - VERSAO 1.5 - 03/03/91  05/03/91  22/06/91 09/06/93
*
SET CONS OFF
SET SCOR OFF
SET STAT OFF
SET TALK OFF
SET PRIN OFF
SET BLINK ON
SET DELE ON
SET DEVI TO SCRE
SET ESCA ON
SET HEAD OFF
SET SAFE OFF
SET DELI OFF
SET NOTIFY ON
SEPARATOR = "."
*
CLEAR

SET DISP TO VGA25
SET INTENSITY ON
SET PROCEDURE TO ADMVFUNC

ON KEY = 315 DO TESTE
RET = CHR(17) + CHR(117)

DO TELAI

USE ARQCORES

    PAR1_P1 = RTRIM(CORTELA)+RTRIM(CORTELAB)
    PAR1_P2 = RTRIM(CORLINHA)+RTRIM(CORLINHAB)
      IF PAR1_P1 + PAR1_P2 <> SPACE(02)
        PAR1 = (PAR1_P1 +'/'+PAR1_P2) + ','
      ELSE
        PAR1 = ','
      ENDIF

    PAR2_P1 = RTRIM(SAYLETRA)+RTRIM(SAYLETRAB)
    PAR2_P2 = RTRIM(SAYREVERSO)+RTRIM(SAYREVERSB)
      IF PAR2_P1 + PAR2_P2 <> SPACE(02)
        PAR2 = (PAR2_P1 +'/'+PAR2_P2) + ','
      ELSE
        PAR2 = ','
      ENDIF


    PAR3_P1 = RTRIM(BORDA1)+RTRIM(BORDA1B)
    PAR3_P2 = RTRIM(BORDA2)+RTRIM(BORDA2B)
      IF PAR3_P1 + PAR3_P2 <> SPACE(02)
        PAR3 = (PAR3_P1 +'/'+PAR3_P2) + ','
      ELSE
        PAR3 = ','
      ENDIF

    PAR4_P1 = RTRIM(GETLETRA)+RTRIM(GETLETRAB)
    PAR4_P2 = RTRIM(GETREVERSO)+RTRIM(GETREVERSB)
      IF PAR4_P1 + PAR4_P2 <> SPACE(02)
        PAR4 = (PAR4_P1 +'/'+PAR4_P2) + ','
        CABCOLOR = PAR4
      ELSE
        PAR4 = ','
        CABCOLOR = PAR4
      ENDIF

    PAR5_P1 = RTRIM(CORMENSAG1)+RTRIM(CORMENSAB1)
    PAR5_P2 = RTRIM(CORMENSAG2)+RTRIM(CORMENSAB2)
      IF PAR5_P1 + PAR5_P2 <> SPACE(02)
        PAR5 = (PAR5_P1 +'/'+PAR5_P2)
      ELSE
        PAR5 = ','
      ENDIF
     MCORSOMB1 = CORSOMB1
     MCORSOMB2 = CORSOMB2
     BCORSOMB1 = BCORSOM1
     BCORSOMB2 = BCORSOM2

     PAR0_P1 = RTRIM(CORSOMB1)+RTRIM(BCORSOM1)
     PAR0_P2 = RTRIM(CORSOMB2)+RTRIM(BCORSOM2)
       IF PAR0_P1 + PAR0_P2 <> SPACE(02)
         CORSCHE = (PAR0_P1 +'/'+PAR0_P2)
       ELSE
         CORSCHE = ','
       ENDIF

     DEFCORES = PAR1+PAR2+PAR3+PAR4+PAR5+CORSCHE
     SET COLOR TO &DEFCORES
*     SET COLOR OF SCHEME 6 TO &PAR4
     CLEAR
 
     LINTELA = PAR3_P1+'/'+PAR2_P2
     CORARQ = 'W+/R+'
     CORVER = 'W+/G'
*
DO TELAG
*
DO TELAROST

? MENSAG('Pressione qualquer tecla para continuar')

USE CTR_SENH
  INDEX ON PROGRAMA + SENHA TO CTR_SENH

VNOMEPRG = 'AV      '
  USE CTR_SENHA INDEX CTR_SENHA
  DO SENHA

DTLANC = CTOD(space(8))
MDATE  = DATE()
       
DO WHILE .NOT. DTLANC = MDATE
  ? JANELA(18,56,21,78)
  ? BORDA(18,56,21,78)
  ? MENSAG('Informe a Data do Sistema no formato [DD/MM/AA]')
  @ 19,59 say ' Data do Sistema'
  @ 20,64 GET DTLANC PICTURE "99/99/99"
    READ
    IF DTLANC = MDATE
      EXIT
    ELSE
      ? MENSAG ('Data do Sistema ‚ '+dtoc(mdate)+' - ENTER e reinforme')
      DTLANC = CTOD("0")
      =INKEY(0)
   ENDIF
ENDDO

DO TELAG
*
USE PARAM
  TIPOIMPR = PADRIMPR

@ 04,01 CLEAR TO 21,78

DO WHILE .T.
  NUMIN = MENU_IN(.T.)
  DO CASE

    CASE NUMIN = 1
      DO AV01000

    CASE NUMIN = 2
      DO AV02000

    CASE NUMIN = 3
      DO AV03000

    CASE NUMIN = 4
      DO AV04000

    CASE NUMIN = 5
      DO AV05000

    CASE NUMIN = 6
      DO AV12000

    CASE NUMIN = 7
      DO AV06000

    CASE NUMIN = 8
      DO AV07000

    CASE NUMIN = 9
      DO AV08000

    CASE NUMIN = 10
      DO AV10000

    CASE NUMIN = 11
      DO MRELCONT

    CASE NUMIN = 12
      DO AV09000
      
    CASE NUMIN = 13 .OR. LASTKEY() = 27
      CLEAR
      CLOS DATA
      RETURN
  ENDCASE
*
    RESTORE SCREEN FROM MAIN_SCR
ENDDO
*
CLEAR
QUIT
****************************************************************
                    FUNCTION MENU_IN
****************************************************************
PARAMETERS CLR_SCR
NUMHOLD = 0
*
=NOMEMOD( 'AV' )
SAVE SCREEN TO MAIN_SCR
*
IF CLR_SCR
  SET MESSAGE TO 23 CENTER
*
  DO JANELA WITH 05,00,21,37
  DO BORDA WITH 05,00,21,37
*
    @ 07,03 PROMPT "a - Estados da Federa‡„o (UF)   " MESSAGE(INCALTEXC)
    @ 08,03 PROMPT "b - Transportadoras             " MESSAGE(INCALTEXC)
    @ 09,03 PROMPT "c - Vendedores                  " MESSAGE(INCALTEXC)
    @ 10,03 PROMPT "d - Clientes                    " MESSAGE(INCALTEXC)
    @ 11,03 PROMPT "e - Produtos                    " MESSAGE(INCALTEXC)
    @ 12,03 PROMPT "f - Arquivos e Parƒmetros       " MESSAGE ;
      "Bancos e Agˆncias, Moedas e Taxas de Convers„o"
    @ 13,03 PROMPT "g - Pedidos de Clientes         " MESSAGE(INCALTEXC)
    @ 14,03 PROMPT "h - Notas Fiscais               " MESSAGE(INCALTEXC)
    @ 15,03 PROMPT "i - Duplicatas                  " MESSAGE(INCALTEXC)
    @ 16,03 PROMPT "j - Estat¡sticas de Vendas      " MESSAGE(INCALTEXC)
    @ 17,03 PROMPT "k - Relat¢rios Cont beis        " MESSAGE ;
      "Define a Competˆncia e imprime o Registro de Sa¡das"
    @ 18,03 PROMPT "u - Utilit rios                 " MESSAGE ;
      "Cria INDEX, BACKUP/REST, formata PRINTER, atualiza SENHA e limpa CADASTROS"
    @ 19,03 PROMPT "v - Volta ao Sistema Operacional" MESSAGE ;
      "Encerra o Sistema 'Administra‡„o de Vendas' e volta ao DOS"
*
  MENU TO NUMHOLD
ENDIF
*
RETURN(NUMHOLD)
*
