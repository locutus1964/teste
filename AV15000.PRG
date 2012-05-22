*--------- Totalizar Pedidos

SELECT PED.CGC, PED.PINT, PED.DTPEDREP, PED.PED, PED.DEM, SUM(QTD * PRE);
       FROM PEDIC PED, PEDID P;
       WHERE PED.PED = P.PED;
       GROUP BY PED.PED;
       INTO TABLE WORK1
       
*---------- Totalizar Notas Fiscais
SELECT NF.CGC, NF.NNF, NF.DEMI, SUM(VAL), NF.ENT;
       FROM CADNF NF;
       GROUP BY NF.NNF;
       INTO TABLE WORK2

*--------- Ligar Tudo       
CLOSE ALL
SELECT W1.*, W2.*;
       FROM WORK1 W1, WORK2 W2;
       WHERE W1.CGC = W2.CGC;
       ORDER BY W2.NNF;
       INTO TABLE WORK3
                     


