***************************************************************************************
* PROGRAMA.: AV08700 
* DESCRICAO: Exporta os Dados dos Clientes para a Cobranca On-Line do HSBC Bamerindus
* AUTOR....: Jair Goncalves
* DATA.....: 12/10/98
***************************************************************************************
* Lay-out do Arquivo SACADOS.TXT
* CAMPO		TIPO	TAM	DEC
*
* CPFCGC	C		 14	 0
* NOME		C		 40	 0
* ENDERECO	C		 38	 0
* CEP		C		  8	 0
* BAIRRO	C		 15	 0		
* CIDADE	C		 15	 0
* UF		C		  2	 0
****************************************************************************************
  PRIVATE ARQ, dInicio, dFim, cTexto

  ARQ='SACADOS.TXT'
  * Datas para filtrar os Clientes que faturaram no ultimo ano
  dInicio = HOJE - 365
  dFim    = HOJE
  
  =SelecCli()  
  
  =MENSAG( "Exportando dados para 'HSBC.DBF'..." )
  CLOSE DATA
  CREATE HSBC FROM XHSBC
  CLOSE DATA
  SELE 0
  USE CLI_COBR ORDER P_CGC
  SELE 0 
  USE HSBC
  SELE 0
  USE WORK
  GO TOP
  DO WHILE ! EOF()
    SELE HSBC 
    APPEND BLANK
    
    REPLACE CPFCGC   WITH FmtCgc( WORK->CGC )
    REPLACE NOME     WITH SUBSTR(WORK->RAZ,1,40)
    IF SEEK(alltrim(WORK->CGC), 'CLI_COBR') THEN
      REPLACE ENDERECO WITH SUBSTR(CLI_COBR->ENDC,1,38)
      REPLACE CEP      WITH SUBSTR(CLI_COBR->CEPC,1,5)+SUBSTR(CLI_COBR->CEPC,7,3)
      REPLACE BAIRRO   WITH SUBSTR(CLI_COBR->BAIC,1,15)
      REPLACE CIDADE   WITH SUBSTR(CLI_COBR->CIDC,1,15)
      REPLACE UF       WITH CLI_COBR->ESTC
    ELSE
      REPLACE ENDERECO WITH SUBSTR(WORK->END,1,38)
      REPLACE CEP      WITH SUBSTR(WORK->CEP,1,5)+SUBSTR(WORK->CEP,7,3)
      REPLACE BAIRRO   WITH SUBSTR(WORK->BAI,1,15)
      REPLACE CIDADE   WITH SUBSTR(WORK->CID,1,15)
      REPLACE UF       WITH WORK->EST
    ENDIF
    SELE WORK
    SKIP
    
  ENDDO
  nRec = RECCOUNT()
  =MENSAG( "Criando arquivo '" + ARQ + "'..." )
  SELECT HSBC
  COPY TO &ARQ TYPE SDF
  CLOSE DATA
  =MENSAG('Coloque um disquete vazio no drive A: e tecle <ENTER> ou <ESC> para sair')
  IF INKEY(0) <> 27
    =MENSAG("Copiando '" + ARQ + "' para o disquete" )
    COPY FILE &ARQ TO A:&ARQ
    =MENSAG('Copia concluida. ' + STR(nRec,5) + ' Registro(s).' + ' Tecle algo para sair')
    =INKEY(0)
  ENDIF  


RETURN

******************
PROCEDURE SELECCLI
******************

  =MENSAG('Selecionando Clientes...')
  SELECT;
    CLI.CGC, CLI.RAZ, CLI.END, CLI.BAI, CLI.CID, CLI.EST, CLI.CEP;
  FROM;
    CLIEN CLI, CABECNF NF, TPNF TP;
  WHERE;
    NF.CANCELADA = .F.; 
    AND NF.DEMISSAO BETWEEN dInicio AND dFim;    
    AND (NF.TIPNF = TP.TIPO AND TP.VENDA = "S");
    AND CLI.CGC = NF.CGC;
  GROUP BY;
    CLI.CGC;    
  INTO TABLE;
    WORK.DBF

  CLOSE DATA
  
RETURN

***************
FUNCTION FMTCGC
***************
  PARAMETER cCgc
  
  cTexto = ''
  FOR i = 1 TO 20 
    cDigito = SUBSTR(cCGC,i,1)
    IF cDigito >= '0' AND cDigito <= '9'
      cTexto = cTexto + cDigito
    ENDIF
  NEXT i    
  
RETURN cTexto
