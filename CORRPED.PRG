SET TALK OFF
SET STATUS ON
MESBASE=28
*----- Cria Tabela de Itens de Pedido Agrupados

SELECT PED, SUM(PRE * QTD), IPI, REGIAO, IDREGIAO;
       FROM PEDID, CLIEN;
       GROUP BY PED;
       ORDER BY PED;
       WHERE PEDID.CGC = CLIEN.CGC;
       INTO TABLE WORK1
CLOSE DATA
*------- Corrige a Tabela PEDIC   
SELE 0 
USE WORK1 
INDEX ON PED TO WORK1
SELE 0 
USE PEDIC
SET RELATION TO PED INTO WORK1

SET PROCEDURE TO ADMVFUNC
GO TOP
DO WHILE ! EOF()
  IF ! EOF( 'WORK1' )
    REPLACE REGIAO WITH WORK1->REGIAO
    IF VALPED = 0  
       REPLACE VALPED WITH WORK1->SUM_EXP_2 + DESPFINANC( WORK1->SUM_EXP_2, CFINANC,;
                                                                            CP1, PE1,;
                                                                            CP2, PE2,;
                                                                            CP3, PE3,;
                                                                            CP4, PE4 )
       REPLACE VALIPI WITH WORK1->SUM_EXP_2 * WORK1->IPI  / 100
    ENDIF
    
  ENDIF
  SKIP
ENDDO
CLOSE ALL

SET TALK ON
SET STATUS ON
CLOSE ALL