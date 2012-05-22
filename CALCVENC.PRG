* ANTIGO AV07421 - CALCULO DO VALOR E VENCIMENTO DE NOTAS FISCAIS
* SISTEMA ADMINISTRACAO DE VENDAS
* JAIR GON€ALVES
*
PRIVATE MES, DIA, MDIA, MMES, AVencEsp, NumVenc, ACondPag, NumPag,;
        APePag, AVencDup, AValDup, ValorNota, QPARC

NumPag   = 4
NVencEsp = 6
DIMENSION AVencEsp(NVencEsp)
DIMENSION ACondPag(NumPag), APePag(NumPag), AVencDup(NumPag), AValDup(NumPag)

STOR 0 TO DPAR, MVDP, VDUP1, VDUP2, VDUP3, VDUP4, VPAR, AVPAR

QDIA = 0
FORM = SPACE(02)
STOR SPACE(08) TO VENC1, VENC2, VENC3, VENC4
STOR SPACE(08) TO VENDUP1, VENDUP2, VENDUP3, VENDUP4

DIA = DAY(DATE())
MES = MONTH(DATE())

INICIO = DATE()

* Copia as Condicoes do pedido para um array
ACondPag(1) = pedic2->cp1
ACondPag(2) = pedic2->cp2
ACondPag(3) = pedic2->cp3
ACondPag(4) = pedic2->cp4

* Copia para um array os percentuais por duplicatas do pedido
APePag(1) = pedic2->pe1
APePag(2) = pedic2->pe2
APePag(3) = pedic2->pe3
APePag(4) = pedic2->pe4

IF pedic2->cpd = 'VIS' .OR. pedic2->cp1 = 0
  INICIO = DATE()
ENDIF

IF pedic2->cpd = 'DFD' .OR. pedic2->cpd = 'DFQ' .OR. pedic2->cpd = 'DFM'
  nDiasFora = 0
  DO CASE
    CASE pedic2->cpd = 'DFD' 
      nDiasFora = 10
    CASE pedic2->cpd = 'DFQ' 
      nDiasFora = 15
    CASE pedic2->cpd = 'DFM' 
      nDiasFora = 30
  ENDCASE
  IF DIA >= nDiasFora 
    nDiasFora = IIF( MES = 2, 28, 30 ) 
  ENDIF
  INICIO = DATE() + nDiasFora - DIA      
ENDIF   

MAIOR_DIA = 0

* Calcula o Total de Parcelas
QPARC = 0
FOR i = 1 TO NumPag
  IF APePag(i) > 0 
    QPARC = QPARC + 1
  ENDIF
NEXT i     

ValorNota = GTOT + GIPI
DPAR = AcumDifDupl( ValorNota, MPE1, MPE2, MPE3, MPE4 )

* Transfere para um array os dias de vencimento especial
FOR i = 1 TO NVencEsp
  AVencEsp(i) = VAL(SUBSTR(MVES, i + ((i * 2) - 2), 2))
NEXT i

* Procura pelo ultimo de dia de vencimento especial
FOR i = 1 TO NVencEsp
  IF AVencEsp(i) > 0
     MAIOR_DIA = AVencEsp(i)
  ENDIF
NEXT i

DIA = SPACE(2)
MES = SPACE(2)

* Zera as datas de vencimento e valores de duplicatas
FOR i = 1 TO NumPag
  AVencDup(i) = SPACE(10)
  AValDup(i) = 0
NEXT i

FOR NPAG = 1 TO QPARC
  CPA = STR(NPAG,2)
  VENC = INICIO + ACondPag(NPAG)
  MDIA = DAY(VENC)
  MMES = MONTH(VENC)
  AValDup(NPAG) = INT(ValorNota * (APePag(NPAG) / 100))
  IF NPAG = 1
    AValDup(NPAG) = AValDup(NPAG) + DPAR
    IF pedic2->cpd = 'ANT' .OR. pedic2->cpd = 'VIS'
      AVencDup(NPAG) = DTOC(DATE())
    ELSE
      AVencDup(NPAG) = DTOC(VENC)
    ENDIF
  ELSE
    IF pedic2->cpd = 'ANT'
      AVencDup(NPAG) = 'ANTECIPADO'
    ELSE
      AVencDup(NPAG) = DTOC(VENC)
    ENDIF
  ENDIF
NEXT I

VDUP1   = AValDup(1)
VDUP2   = AValDup(2) 
VDUP3   = AValDup(3)
VDUP4   = AValDup(4)
VENDUP1 = AVencDup(1)
VENDUP2 = AVencDup(2)
VENDUP3 = AVencDup(3)
VENDUP4 = AVencDup(4)
return

***************
PROCEDURE MAIOR
***************
  PRIVATE i, c
  
  IF MAIOR_DIA > 0
     * Se o dia de Vencimento > que o maior dia venc.especial
     IF MDIA > MAIOR_DIA
        DIF  = NDIAMES(venc)
        VENC = VENC + ((DIF + AVencEsp(1)) - MDIA)
     ELSE
        FOR i = 1 TO 6
           IF AVencEsp(i) >= MDIA
              VENC = VENC + (AVencEsp(i) - MDIA)
              FOR c = i+1 TO 6
                 AVencEsp(c) = 0
              NEXT c
              EXIT  
           ENDIF   
        NEXT i
     ENDIF
  ENDIF
RETURN
