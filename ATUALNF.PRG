USE CADNF
APPEND FROM av5\cadnf.dbf FOR SELENF() = .T.
USE CABECNF
APPEND FROM av5\cabecnf.dbf FOR SELENF() = .T.

FUNCTION SELENF 

   ACHOU = CTOD(DEMI) = {21/03/97} .AND.;
           ((NNF >= 2220 .AND. NNF <= 2226) .OR.;
           (NNF >= 2228 .AND. NNF <= 2229))


RETURN Achou                 