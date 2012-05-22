CLEAR
SET PROCEDURE TO ADMVFUNC
PAR4_P1 = ''
PAR1_P2 = ''
 
SET TALK OFF
=ExportHsbc()
SET TALK ON
RETURN

********************
PROCEDURE EXPORTHSBC
********************
  PRIVATE ARQ, dInicio, dFim

  ARQ='SACADOS.TXT'
  dInicio = DATE() - 365
  dFim    = DATE()
  =SelecCli()
  =MENSAG( 'Exportando dados para ' + ARQ + ' ...' )
  SELE 0 
  USE HSBC
  ZAP
  SELE 0
  USE WORK
  GO TOP
  DO WHILE ! EOF()
    SELE HSBC 
    APPEND BLANK
    REPLACE CPFCGC   WITH FMTCGC(WORK->CGC)
    REPLACE NOME     WITH SUBSTR(WORK->RAZ,1,40)
    REPLACE ENDERECO WITH SUBSTR(WORK->END,1,38)
    REPLACE CEP      WITH SUBSTR(WORK->CEP,1,5)+SUBSTR(WORK->CEP,7,3)
    REPLACE BAIRRO   WITH SUBSTR(WORK->BAI,1,15)
    REPLACE CIDADE   WITH SUBSTR(WORK->CID,1,15)
    REPLACE UF       WITH WORK->EST
    SELE WORK
    SKIP
  ENDDO
  SELE HSBC
  COPY TO &Arq TYPE SDF
  CLOSE DATA

RETURN

******************
PROCEDURE SELECCLI
******************

  =MENSAG('Selecionando Clientes...')
  SELECT;
    CLI.CGC, CLI.RAZ, CLI.END, CLI.BAI, CLI.CID, CLI.EST, CLI.CEP;
  FROM;
    CLIEN CLI, CABECNF NF;
  WHERE;
    NF.CANCELADA = .F. AND;
    NF.DEMISSAO BETWEEN dInicio AND dFim;
    AND CLI.CGC = NF.CGC;
  GROUP BY;
    CLI.CGC;    
  INTO TABLE;
     WORK.DBF

  CLOSE DATA
  
RETURN

****************
PROCEDURE FMTCGC
****************
PARAMETER cCGC
  PRIVATE i, Digito, Texto
  
  Texto = ''
  FOR i = 1 TO 20 
    Digito = SUBSTR(cCgc,i,1)
    IF Digito >= '0' AND Digito <= '9'
      Texto = Texto + Digito
    ENDIF
  NEXT i    

RETURN Texto
