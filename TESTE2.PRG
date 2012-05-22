SET TALK ON
SET safety off
CLOSE ALL
SELECT;
  CLI.CGC, SUM(NF.QTD);
FROM;
  CLIEN CLI INNER JOIN NFVENDA NF ON CLI.CGC = NF.CGC;
            INNER JOIN prod p     ON nf.ref = p.ref;
            inner join pedid it   on (nf.ref = it.ref and nf.ped = it.ped);            
group by;
  cli.cgc;            
INTO TABLE T  
set safety on
  
  
  