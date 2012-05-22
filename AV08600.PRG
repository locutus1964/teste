*********************************************************************
* MODULO.: Gera um disquete e um relat¢rio com dados para o SCI
* AUTOR..: Jair Gon‡alves
* CRIA€ÇO: 21/07/97
*
**********************************************************************
PRIVATE dData, nDia, nMes, nAno, dInicio, MC, cTela

MC = 'N'
CodigoSCI = '10802098646' 
MENS = 'Confirma Gera‡Æo de Arquivo'
DO PEGRESP WITH [SN]
IF MC = 'N' .OR. LASTKEY() = 27
  RETURN
ENDIF  

*----- Inicializa a data para um Ano antes da Data Atual
dData   = DATE()
nDia    = DAY( dData )
nMes    = MONTH( dData )
nAno    = YEAR( dData ) - 1
dInicio = CTOD( STR(nDia,2) + '/' + STR(nMes,2) + '/' + STR(nAno,4))

=MENSAG( 'Selecionando Clientes Ativos e Poss¡veis...' )

SELE 0
USE CLIEN ORDER P_CGC
COPY FIELDS CGC, ESTATUS, FIJU, CONCEITO;
     TO CLITMP FOR (ESTATUS = 'A' .OR. ESTATUS = 'P' .OR. ESTATUS = 'I') .AND. FIJU = 'J'
     
=MENSAG('Selecionando Duplicatas...')
SELECT DUPL.CGC, NDUP, SUM(VDP) AS SOMA, EMIS AS EMISSAO, VENC, PAG;
       FROM DUPL, CLITMP;
       GROUP BY NDUP;
       HAVING EMISSAO >= dInicio;
       WHERE DUPL.CGC = CLITMP.CGC;
       ORDER BY DUPL.CGC, EMISSAO;
       INTO TABLE WORK 
CLOSE DATA    

SELE 0
USE CONCEITO INDEX CONCEITO.IDX
SELE 0
USE CLITMP ALIAS CLIEN
INDEX ON CGC TO CLITMP.IDX
SELE 0
USE SCI
SELE 0
USE WORK
INDEX ON CGC TO WORK.IDX

SELE CLIEN
SET RELATION TO CONCEITO INTO CONCEITO,;
                CGC      INTO WORK

SELE SCI
ZAP

MC = ''
MENS = 'Incluir todos os Clientes na Pesquisa ?'
DO PEGRESP WITH [NS]
lSelecTudo = MC == 'S'
  
SELE CLIEN
=MENSAG( 'Gerando Arquivo SCI...' )
GO TOP
DO WHILE ! EOF()
  IF FOUND('CONCEITO') .AND. ! EMPTY(CONCEITO->CONCSCI)
    cConceito = CONCEITO->CONCSCI
  ELSE
    cConceito = '00'
  ENDIF    
  dtEmissao   = CTOD('')
  nAtraso    = 0
  nMaiorAcum = 0
  nMaiorFat  = 0
  cPrimFatura = '0000'
  =ChkDupl()
  IF FOUND('WORK') .OR. lSelecTudo
    =GravaSCI()
  ENDIF  
  SELECT CLIEN
  SKIP
ENDDO
=GravaTexto()
CLOSE DATA
ERASE WORK.DBF
ERASE CLITMP.DBF
ERASE CLITMP.IDX
ERASE WORK.IDX
RETURN 

****************
FUNCTION CHKDUPL
****************
  PRIVATE nCont, nDias
  
  SELE WORK
  MCGC = CGC
  nAtraso = 0
  nCont = 0
  nDias = 0
  DO WHILE ! EOF() .AND. MCGC = CGC 
    *---- Guarda a éltima Fatura
    dtEmissao = EMISSAO
    *---- Guarda o maior Atraso
    IF cConceito <> '01' .AND. ((PAG - VENC) > 5)
      nDias = nDias + (PAG - VENC)
    ENDIF
    *---- Acumula Somat¢rio das Faturas
    IF EMPTY(PAG) .OR. PAG = dtEmissao
      nMaiorAcum = nMaiorAcum + SOMA
    ENDIF  
    *---- Determina a Maior Fatura
    IF SOMA > nMaiorFat 
      nMaiorFat = SOMA
    ENDIF  
    nCont = nCont + 1
    SKIP
  ENDDO
  *------ Calcula a M‚dia de Atraso
  IF nCont > 0
    nAtraso = ROUND(nDias / nCont,0)
  ENDIF  
  SELECT CLIEN
  
RETURN

*****************
FUNCTION GRAVASCI
*****************

  *---------- Grava o Arquivo SCI
  SELE SCI
  APPEND BLANK
  * 01 a 11
  REPLACE CODSCI    WITH CodigoSCI
  * 12 a 25
  REPLACE CGC       WITH SUBSTR(CLIEN->CGC,1,8)+;
                         SUBSTR(CLIEN->CGC,10,4)+;
                         SUBSTR(CLIEN->CGC,15,2)
  * 26 a 27
  REPLACE CONCEITO  WITH cConceito
  * 28 a 30
  REPLACE ATRASO    WITH PADL(LTRIM(STR(nAtraso)),3,'0')
  * 31 a 43
  REPLACE MAIORFAT  WITH ConvStr(nMaiorFat)
  * 44 a 56
  IF nMaiorAcum > nMaiorFat
    REPLACE MAIORACUM WITH ConvStr(nMaiorAcum)
  ELSE  
    REPLACE MAIORACUM WITH ConvStr( 0 )
  ENDIF  
  * 57 a 60
  REPLACE PRIMFATURA WITH cPrimFatura
  * 61 a 64
  MTEMP = DTOS(dtEmissao)
  REPLACE ULTFATURA  WITH SUBSTR(MTEMP,3,2)+SUBSTR(MTEMP,5,2)
  * 65
  REPLACE DISPSAC    WITH '1'

RETURN

*******************
FUNCTION GRAVATEXTO
*******************

  =MENSAG( 'Gerando Arquivo SCI.TXT...' )
  SELE SCI
  COPY TO SCI SDF
  cUnid = 'A'
  @ 23, 01 SAY SPACE(78)
  @ 23, 20 SAY 'Escolha a Unidade?: (A/B)' GET cUnid PICT [!] VALID cUnid $ [AB]
  READ
  IF LASTKEY() = 13
    cUnid = cUnid + ':' 
    =MENSAG( 'Formatando o disquete...' )
    cComando = 'FORMAT ' + cUnid + ' /U /V:SCI'
    SAVE SCREEN TO cTela
    CLEAR
    RUN &cComando
    RESTORE SCREEN FROM cTela
    =MENSAG( 'Copiando SCI.TXT para o disquete...' )
    cComando = 'COPY FILE SCI.TXT TO ' + cUnid + 'SCI.TXT'
    &cComando
  ENDIF  

RETURN

*------ Remove as Virgulas e Pontos do Valor
*===============
FUNCTION CONVSTR
*================
PARAMETER nNumero
  PRIVATE cTmp, cTexto

  cTmp = STR(nNumero,14,2)
  cTexto = LTRIM(SUBSTR(cTmp,1,11) + SUBSTR(cTmp,13,2)) 
  cTexto = PADL( cTexto, 13, '0' ) 

RETURN cTexto
      