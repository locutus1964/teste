DIMENSION AVenc(3), AParc(3), APe(3)
SET TALK OFF
CLEAR
MESBASE = 28
VALOR = 1000
Taxa = 10  && a.m.
AVenc(1) = 0
AVenc(2) = 50
AVenc(3) = 60
APe(1) = 10
APe(2) = 50
APe(3) = 40
FOR i = 1 TO 3
  AParc(i) = Valor * APe(i) / 100
NEXT i  
nValPres = DespFin()
@ 01, 00 SAY 'Taxa............: ' + TRANSFORM( Taxa, [ 99.99 %] )
@ 02, 00 SAY 'Valor Financiado: ' + TRANSFORM( Valor, '99,999.99' )
@ 03, 00 SAY 'Valor Presente..: ' + TRANSFORM( nValPres, '99,999.99' )
@ 04, 00 SAY 'Total...........: ' + TRANSFORM( AParc(1) + AParc(2) + AParc(3), '99,999.999' )
@ 05, 00 SAY 'Porc.Parcelas...: ' + STR(APe(1),4,1) + ' - ' + STR(APe(2),4,1) + ' - ' + STR(APe(3),4,1)
@ 06, 00 SAY 'Valor Parcelas..: ' + STR(AParc(1),8,2) + ' - ' + STR(AParc(2),8,2) + ' - ' + STR(AParc(3),8,2)
@ 07, 00 SAY 'Vencimentos.....: ' + STR(AVenc(1),2) + ' - ' + STR(AVenc(2),2) + ' - ' + STR(AVenc(3),2)
SET TALK ON

*===============
FUNCTION DESPFIN
*===============
  PRIVATE nDespesa, nDif, nValPres, nMult 
  
  nValPres = VALORPRES( Valor, Taxa, AVenc(1), AParc(1),;
                                     AVenc(2), AParc(2),;
                                     AVenc(3), AParc(3),;
                                     0,        0 )
  nMult = Valor / nValPres
  FOR i = 1 TO 3
    AParc(i) = AParc(i) * nMult
  NEXT i  
  nValPres = VALORPRES( Valor, Taxa, AVenc(1), AParc(1),;
                                     AVenc(2), AParc(2),;
                                     AVenc(3), AParc(3),;
                                     0,        0 )

RETURN nValPres

*=================
FUNCTION VALORPRES
*=================
PARAMETER nValor, nTaxa, nCPag1, nParc1, nCPag2, nParc2, nCPag3, nParc3, nCPag4, nParc4
  PRIVATE ACondPag, nValorPres
  
  DIMENSION ACondPag(4), AParc(4)
  ACondPag(1) = nCPag1
  ACondPag(2) = nCPag2
  ACondPag(3) = nCPag3
  ACondPag(4) = nCPag4
  AParc(1)= nParc1
  AParc(2)= nParc2
  AParc(3)= nParc3
  AParc(4)= nParc4
  nValorPres = 0
  FOR I = 1 TO 4
    IF AParc(i) > 0
      nValorPres = nValorPres + (AParc(i) * FAC( nTaxa, ACondPag(i) / MESBASE))
    ENDIF   
  NEXT I

RETURN nValorPres
