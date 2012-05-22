close DATA
set talk off
set safety off
SET PROCEDURE TO ADMVFUNC

* 1o Periodo
INICIO = {01/06/97}
FIM    = {01/06/98}
SELECT; 
  NF.CGC, SUM(NF.VALPROD) AS VENDA;
  FROM ;
    CABECNF NF, CLIEN CLI, TPNF T;
  WHERE ;
    (NF.DEMISSAO BETWEEN INICIO AND FIM); 
    AND;
    (CLI.CGC = NF.CGC) AND (NF.CANCELADA = .F.);
    AND (NF.TIPNF = T.TIPO AND T.VENDA = 'S');
  GROUP BY;
    NF.CGC;
  INTO TABLE;
    TMP  

* 2o Periodo
INICIO = {01/06/98}
FIM    = {01/06/99}
SELECT; 
  NF.CGC, SUM(NF.VALPROD) AS VENDA;
  FROM ;
    CABECNF NF, CLIEN CLI, TPNF T;
  WHERE ;
    (NF.DEMISSAO BETWEEN INICIO AND FIM); 
    AND;
    (CLI.CGC = NF.CGC) AND (NF.CANCELADA = .F.);
    AND (NF.TIPNF = T.TIPO AND T.VENDA = 'S');
  GROUP BY;
    NF.CGC;
  INTO TABLE;
    TMP2  

CLOSE ALL

SELECT;
   cli.gue, t1.venda as val1, t2.venda as val2,;
   (((T2.VENDA / T1.VENDA) - 1) * 100.0) AS PORC;
  FROM;
    CLIEN CLI, TMP T1, TMP2 T2;
  WHERE;
    (CLI.CGC = T1.CGC);
    AND;
    (T1.CGC = T2.CGC);
  ORDER BY;
    T2.VENDA DESC;  
  INTO TABLE;
    RESULT1

DO IMPSAIDA
    
CLOSE DATA
SET TALK ON


  