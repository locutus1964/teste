* Inicio
CLEAR
SET TALK OFF
SET STAT OFF
close all
Limite = {01/01/90}

* Atualiza IDREGIAO dos CLIENTES
* =UpDateCli()
* Atualiza IDREPR / IDREGIAO dos Pedidos
* =UpDatePed()
* Atualiza IDREPR / IDREGIAO dos CabecNf
 =UpdateNF()

* Encerra
SET TALK ON
SET STAT ON
CLOSE DATA
USE ERRO
IF RECCOUNT() > 0
  BROW
ENDIF  

*******************
PROCEDURE UPDATECLI
*******************

  ? 'Atualizando Clientes...'
  CLOSE DATABASE
  SELE 0
  USE VEND ORDER X_CVR
  sele 0
  USE regiao ORDER X_REGIAO
  sele 0
  USE CLIEN
  DO WHILE ! EOF()
    IF SEEK( CLIEN->regiao, 'REGIAO') AND CLIEN->IDREGIAO = 0
      REPLACE IDREGIAO WITH REGIAO->IDREGIAO
    ENDIF  
    IF SEEK( CLIEN->VEN, 'VEND') AND CLIEN->IDREPR = 0
      REPLACE IDREPR WITH VEND->IDREPR
    ENDIF  
    SKIP
  ENDDO
  
RETURN

*******************
PROCEDURE UPDATEPED
*******************  
  ? 'Atualizando Pedidos...'
  CLOSE DATABASE
  SELE 0
  USE REGIAO ORDER X_REGIAO
  SELE 0
  USE VEND ORDER X_CVR
  SELE 0
  USE PEDIC
  SET FILTER TO DEM >= Limite
  GO TOP
  DO WHILE ! EOF()
    IF PEDIC->IDREGIAO = 0 AND SEEK( PEDIC->REGIAO, 'REGIAO')
      REPLACE IDREGIAO WITH REGIAO->IDREGIAO
    ENDIF     
    IF PEDIC->IDREPR = 0 AND SEEK( PEDIC->VEN, 'VEND')
      REPLACE IDREPR WITH VEND->IDREPR
    ENDIF  
    SKIP
  ENDDO
RETURN

******************
PROCEDURE UPDATENF
******************  

  ? 'Atualizando Notas Fiscais...'
  CLOSE DATABASE
  SELE 0
  USE REGIAO ORDER X_REGIAO
  SELE 0
  USE VEND ORDER X_CVR
  SELE 0
  USE CABECNF
  SET FILTER TO DEMISSAO >= Limite
  GO TOP
  DO WHILE ! EOF()
    IF CABECNF->IDREGIAO = 0 AND SEEK( CABECNF->REGIAO, 'REGIAO')
      REPLACE IDREGIAO WITH REGIAO->IDREGIAO
    ENDIF     
    IF CABECNF->IDREPR = 0 AND SEEK( CABECNF->VEN, 'VEND')
      REPLACE IDREPR WITH VEND->IDREPR
    ENDIF  
    SKIP
  ENDDO
  CLOSE DATA
  
  SELECT NF.NNF, CLI.GUE, NF.DEMISSAO, NF.VEN, NF.IDREPR, NF.REGIAO, NF.IDREGIAO;
    FROM;
      CABECNF NF, TPNF T, CLIEN CLI;
    WHERE;
      (NF.CGC = CLI.CGC) AND;
      (NF.TIPNF = T.TIPO AND T.VENDA = 'S' AND NF.CANCELADA = .F.);
      AND (EMPTY(NF.VEN) OR EMPTY(NF.IDREPR) OR EMPTY(NF.REGIAO) OR EMPTY(NF.IDREGIAO));
    ORDER BY;
      CLI.GUE, NF.NNF ; 
    INTO TABLE;
      ERRO.DBF    
    
RETURN