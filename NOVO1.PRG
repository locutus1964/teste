clear
set talk off
SET PRINT ON
SET DEVICE TO PRINTER
SET PRINTER TO ARQ.PRN
FOR MES = 1 TO 12
  FOR DIA = 1 TO 31
    IF NOVO1() <> NOVO2() 
      ? DIA, MES 
    ENDIF  
  NEXT DIA   
NEXT MES  
SET PRINT OFF
SET PRINTER TO
SET DEVICE TO SCREEN
set talk on

*===========
FUNCTION NOVO1
*===========
 IF cpd = 'DFD'
   IF dia < 10
      INICIO = (DATE()) + (10-dia)
   ELSE
      IF mes = 02
         INICIO = (DATE()) + (28-dia)
      ELSE
         INICIO = (DATE()) + (30-dia)
      ENDIF
   ENDIF
 ENDIF
 IF cpd = 'DFQ'
   IF dia < 15
      INICIO = (DATE()) + (15-dia)
   ELSE
      IF mes = 02
         INICIO = (DATE()) + (28-dia)
      ELSE
         INICIO = (DATE()) + (30-dia)
      ENDIF
   ENDIF
 ENDIF
 IF cpd = 'DFM'
   IF mes = 02
      INICIO = (DATE()) + (28-dia)
   ELSE
      INICIO = (DATE()) + (30-dia)
   ENDIF
 ENDIF

RETURN INICIO

*=============
FUNCTION NOVO2
*=============
 
 INICIO = DATE()
 
 IF cpd = 'DFD' .OR. cpd = 'DFQ' .OR. cpd = 'DFM'
   nDiasFora = 0
   DO CASE
     CASE cpd = 'DFD' 
       nDiasFora = 10
     CASE cpd = 'DFQ' 
       nDiasFora = 15
     CASE cpd = 'DFM' 
       nDiasFora = 30
   ENDCASE
   
   IF DIA >= nDiasFora 
     nDiasFora = IIF( MES = 2, 28, 30 ) 
   ENDIF
   
   INICIO = DATE() + nDiasFora - DIA      
   
 ENDIF   
 
RETURN INICIO