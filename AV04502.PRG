*
* Programa....: AV04500.prg                                                 
* Sistema.....: Administra‡„o de Vendas - Ad„o Eli Pereira                  
* Objetivo....: Relat¢rio resumo da situa‡„o dos clientes por regi„o e      
*               representantes.                                             
* Data........: 26/06/97
* Revisao.....: 03/03/99
* Autor.......: Jair Goncalves
*
PRIVATE mven, mregiao, vtotreg, mstatus, mperc, ASTATUS,;
        nTamLinha, cCabecStatus, ATOTAL, nCols, MODULO

MODULO = PROGRAM()
nCols = 8
DIMENSION ASTATUS(nCols), ATOTAL(nCols)
nTamLinha = 135
do TESTIMP2
if CP = 'N'
   return
endif
pagctr = 0
tamrel = 2

*---- Contabiliza todos os cliente pelo status
=MENSAG( 'Totalizando por Status...' )

SELECT REGIAO, IDREGIAO, COUNT(ESTATUS) AS QTD,ESTATUS;
  FROM CLIEN;
  GROUP BY REGIAO, IDREGIAO, ESTATUS;
  ORDER BY REGIAO, IDREGIAO;
  INTO TABLE WORK
  
=MENSAG('Imprimindo Relat¢rio da Situa‡„o dos Clientes... Aguarde')
do DEFSAIDA
tit = '==  RESUMO  DE  SITUACAO  DOS  CLIENTES  =='
lin = 0

SELE 0
USE REGIAO ORDER X_REGIAO
SELE 0
USE STATUS INDEX STATUS.IDX, STATCOL.IDX

cCabecStatus = '   '
=CabecStatus()

SELE WORK
SET RELATION TO REGIAO+STR(IDREGIAO,2) INTO REGIAO
=ImprCabec()

FOR I = 1 TO nCols
  ATotal(i) = 0
NEXT I
DO WHILE !EOF()
  FOR I = 1 TO nCols
    ASTATUS(I) = 0
  NEXT I  
  MREGIAO  = FmtRegiao(REGIAO, IDREGIAO)
  MDREGIAO = REGIAO->DREGIAO
  DO WHILE !EOF() .AND. MREGIAO = FmtRegiao(REGIAO, IDREGIAO)
    IF SEEK( ESTATUS, 'STATUS' )
      ASTATUS(STATUS->COLU) = ASTATUS(STATUS->COLU) + WORK->QTD
    ENDIF  
    SKIP
  ENDDO  
  =ImprLin()
ENDDO
*----- Fim do Relatorio
=ImprTotal()
DO IMPSAIDA


*=================
function IMPRCABEC
*=================
  =CABEC(modulo, TIT, nTamLinha)
  @ 03, 00 say '                R E G I A O'
  @ 03, 44 say cCabecStatus + '| T  O  T  A  L '
  @ 04, 00 say '===============================================|========|========|========|========|========|========|========|========|================'
  LIN = 5
return 

*===============
FUNCTION IMPRLIN
*===============
  PRIVATE nTam, nTotal, nCol, Mascara
  
  Mascara = '  99,999'
  nTam = 9
  IF LIN > 60
    =ImprCabec()
  ENDIF  
  @ Lin, 00 SAY MREGIAO
  @ Lin, 08 SAY MDREGIAO
  nTotal = 0
  FOR I = 1 TO nCols
    nCol = 47+((I-1)*nTam)
    IF ASTATUS(i) > 0
      @ Lin, nCol SAY TRANSFORM(ASTATUS(i), Mascara )
    ENDIF  
    nTotal = nTotal + ASTATUS(i)
    ATotal(i) = ATotal(i) + ASTATUS(i)
  NEXT I  
  @ Lin, nCol+nTam SAY TRANSFORM( nTotal, Mascara )
  Lin = PROW() + 1
   
RETURN

*=================
FUNCTION IMPRTOTAL
*=================
  PRIVATE nTam, nTotal, nCol
  
  nTam = 9
  nTotal = 0
  @ Lin+0, 00 SAY REPLICATE('-', nTamLinha)
  @ Lin+1, 00 SAY 'T O T A I S:'
  FOR I = 1 TO nCols
    nCol = 47+((I-1)*nTam)
    @ Lin+1, nCol SAY TRANSFORM(ATOTAL(i), '  99,999' )
    nTotal = nTotal + ATOTAL(i)
  NEXT I  
  @ Lin+1, nCol+nTam SAY TRANSFORM( nTotal, '  99,999' )
  FOR I = 1 TO nCols
    nCol = 49+((I-1)*nTam)
    @ Lin+2, nCol SAY TRANSFORM((ATOTAL(i) / nTotal) * 100, ' 999.9 %')
  NEXT I  
  @ Lin+3, 00 SAY REPLICATE('-', nTamLinha)
  @ Lin+4, 10 SAY 'F I N A L    D E S T E     R E L A T O R I O'

  Lin = PROW() + 1
   
RETURN

*===================
function CABECSTATUS
*===================
  
  SELE STATUS    
  SET ORDER TO 2
  GO TOP
  DO WHILE ! EOF()
    MCOLU = COLU
    MNOME = NOME
    DO WHILE ! EOF() .AND. MCOLU=COLU
      SKIP 
    ENDDO  
    cCabecStatus = cCabecStatus + '| ' + MNOME + ' '
  ENDDO
  SET ORDER TO 1
  
RETURN

*REPR UF         R E G I A O                 | ATIVO | INATI | POSSI | DESAT | OUTRO | TOTAL
*==== == ====================================|=======|=======|=======|=======|=======|=======
*                                            | 99.999| 99.999| 99.999| 99.999| 99.999| 99,999 