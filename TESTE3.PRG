close data
set talk on
 SELECT;
  CLI.EST, CAD.IDREGIAO, CAD.REGIAO, R.DREGIAO, CAD.IDREPR, CAD.VEN, V.VENGUE, CAD.REF, P.TIPO, P.EMBALAGEM, SUM(CAD.QTD) AS SOMAQTD, SUM(CAD.VAL) AS SOMAVAL, AVG(PED.DIT) AS DESC1;
 FROM;
   "CLIEN.DBF" CLI JOIN "NFVENDA.DBF" CAD ON CLI.CGC = CAD.CGC;
                   JOIN "PROD.DBF" P      ON CAD.REF = P.REF;
                   JOIN "PEDID.DBF" PED   ON P.REF = PED.REF,;
                   "REGIAO.DBF" R, "VEND.DBF" V;
 WHERE;
   (CAD.IDREGIAO=R.IDREGIAO) AND (CAD.IDREPR = V.IDREPR) AND (CAD.PED = PED.PED);
 GROUP BY;
  CAD.IDREPR, CAD.VEN, V.VENGUE, CAD.IDREGIAO, CAD.REGIAO, R.DREGIAO, CAD.REF, P.TIPO, P.EMBALAGEM, CLI.EST;
 ORDER BY;
   CAD.VEN, CAD.IDREPR, CAD.REGIAO, CAD.IDREGIAO, SOMAQTD DESC

