CLS
APE = ARRAY(4)
APMT = ARRAY(4)
APE[1] = 10
APE[2] = 90
APE[3] =  0
APE[4] =  0
PV = 200.00

APMT = CALCULA( PV, 0.1, APE )

SOMAPV = 0
FOR i = 1 TO 4
  PPV = APMT[i] / (1.1^i)
  SOMAPV += PPV
  ? APMT[i], PPV, APMT[i] - PPV
NEXT I
?
? SOMAPV

****************
FUNCTION CALCULA( PV, Taxa, APE )
****************
  LOCAL SOMAFT

  SOMAFT = 0
  FOR I = 1 TO 4
    SOMAFT += APE[i] / ((1 + Taxa)^i)
  NEXT i

  FOR i = 1 TO 4
    APMT[i] = (PV * APE[i]) / SOMAFT
  NEXT i

RETURN APMT

  
