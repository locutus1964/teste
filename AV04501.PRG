CLEAR
SET TALK OFF
SET CONFIRM ON
PRIVATE lin, tamlin, maxlin, maxcol

DO CABTELA WITH 0, 0, 24, 79
MSG = 'Relatorio de Situacao de Clientes'
@ 01, CentraTxt( MSG, 80 ) SAY MSG
maxcol = 5
DIMENSION coluna(maxcol), totais(maxcol)
CLOSE DATA
SELE 0 
USE CLIEN
=MENSAG( 'Processando Clientes...' )
INDEX ON VEN+REGIAO TO TST
COPY FIELDS RAZ,VEN,REGIAO,ESTATUS,EST;
     TO WORK FOR VEN='0199'
USE WORK
SELE 0
USE REGIAO INDEX REGIAO
SELE 0
USE VEND INDEX VEND
SELE 0
USE STATUS INDEX STATUS
REPLACE ALL QUAN WITH 0
SELE WORK
SET RELATION TO ven     INTO VEND,;
                regiao  INTO REGIAO,;
                estatus INTO STATUS

TotOutros = 0
maxlin = 56
tamlin = 132
NaoAchou = 0
FOR i = 1 TO MAXCOL
   totais(i) = 0
NEXT i   
GO TOP
TamRel = 2
DO TESTIMP2
=MENSAG('Imprimindo...' )
DO DEFSAIDA
lin = 0
DO WHILE !EOF()
   * Enquanto e o mesmo vendedor
   mven = ven
   mraz = vend->raz
   @ lin, 00 SAY mven + ' - ' + mraz
   lin = lin + 1
   DO WHILE !EOF() .AND. mven = ven 
      mregiao  = regiao
      mdregiao = regiao->dregiao
      FOR i = 1 TO MAXCOL
         coluna(i) = 0
      NEXT i    
      * Enquanto e a mesma regiao 
      DO WHILE !EOF() .AND. mven = ven .AND. mregiao = regiao
         IF FOUND('STATUS') 
            coluna(STATUS->COLU) = coluna(STATUS->COLU) + 1
            totais(STATUS->COLU) = totais(STATUS->COLU) + 1
         ENDIF
         SKIP
      ENDDO
      @ lin, 10 SAY mregiao + ' -  ' + mdregiao
      && =IMPRCOL( 71, @coluna )
      lin = lin + 1
   ENDDO
ENDDO 
lin = lin + 1
=IMPRCOL( 71, @totais )
DO IMPSAIDA
CLOSE DATA
SET TALK ON

*-------------------------------------------------------------
FUNCTION IMPRCOL
PARAMETER col, MATRIZ 
PRIVATE TotLinha, i

  TotLinha = matriz(1)+matriz(2)+matriz(3)+matriz(4)+matriz(5)
  FOR i = 1 TO MAXCOL
    porc = IIF( TotLinha > 0, MATRIZ(i) / TotLinha * 100, 0 )
    @ lin, col SAY STR(matriz(i),5,0) 
    col = col + 9
  NEXT i   
  @ prow(), 89 SAY STR(TotLinha,5,0)

RETURN 


