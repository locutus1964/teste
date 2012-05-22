* AV07421 - CALCULO DO VALOR E VENCIMENTO DE NOTAS FISCAIS
* SISTEMA ADMINISTRACAO DE VENDAS
* ADAO ELI PEREIRA - 14-07-90  -  14/12/91
*
PRIVATE MES, DIA, MDIA, MMES, AVencEsp, NumVenc, ACondPag, NumPag,;
        APePag, AVencDup, AValDup

NumPag = 4
NVencEsp = 6
DIMENSION AVencEsp(NVencEsp)
DIMENSION ACondPag(NumPag), APePag(NumPag), AVencDup(NumPag), AValDup(NumPag)

STOR 0 TO DPAR, MVDP, VDUP1, VDUP2, VDUP3, VDUP4, VPAR, AVPAR
STOR 0 TO QPARC, QDIA
FORM = SPACE(02)
STOR SPACE(08) TO VENC1, VENC2, VENC3, VENC4
STOR SPACE(08) TO VENDUP1, VENDUP2, VENDUP3, VENDUP4

DIA = DAY(DATE())
MES = MONTH(DATE())

INICIO = DATE()

IF TPNF->EDUP <>  'S'
   RETURN
ENDIF

IF pedic2->cp1 = 0 .OR. pedic2->cpd = 'VIS'
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
         nDiasFora = 0
   ENDCASE
   
   IF DIA < nDiasFora 
      INICIO = DATE() + (nDiasFora - DIA)
   ELSE
      IF MES = 02
         INICIO = (DATE()) + (28-DIA)
      ELSE
         INICIO = (DATE()) + (30-DIA)
      ENDIF
   ENDIF  
ENDIF   

MAIOR_DIA = 0
QPARC  = 1
CPAC  = 1

* Copia as Condicoes do pedido para um array
ACondPag(1) = pedic2->cp1
ACondPag(2) = pedic2->cp2
ACondPag(3) = pedic2->cp3
ACondPag(4) = pedic2->cp4

* Calcula o Total de Parcelas
FOR i = 1 TO NumPag
  IF ACondPag(i) > 0 
     QPARC = QPARC + 1
  ENDIF
NEXT i     

DPAR = AcumDifDupl( GTOT, MPE1, MPE2, MPE3, MPE4 )
* CDPAR = INT(GTOT*(pedic2->PE1/100))+INT(GTOT*(pedic2->PE2/100))+INT(GTOT*(pedic2->PE3/100))+INT(GTOT*(pedic2->PE4/100))
* IF CDPAR <> GTOT
*   DPAR = (GTOT - CDPAR)
* ENDIF

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

* Copia para um array os percentuais por duplicatas do pedido
APePag(1) = pedic2->pe1
APePag(2) = pedic2->pe2
APePag(3) = pedic2->pe3
APePag(4) = pedic2->pe4

* Zera as datas de vencimento e valores de duplicatas
FOR i = 1 TO NumPag
  AVencDup(i) = SPACE(10)
  AValDup(i) = 0
NEXT i

DO WHILE QPARC > 0
   CPA = STR(CPAC,2)
   IF ACondPag(CPAC) > 0
      VENC = INICIO + ACondPag(CPAC)
      MDIA = DAY(VENC)
      MMES = MONTH(VENC)
*     DO MAIOR
      AValDup(CPAC) = GIPI + INT(GTOT * (APePag(CPAC) / 100))
      IF CPAC = 1
         AValDup(CPAC) = AValDup(CPAC) + DPAR
         IF pedic2->cpd = 'ANT' .AND. pedic2->cpd = 'VIS'
            AVencDup(CPAC) = DTOC(DATE())
         ELSE
            AVencDup(CPAC) = DTOC(VENC)
         ENDIF
      ELSE
         IF pedic2->cpd = 'ANT'
            AVencDup(CPAC) = 'ANTECIPADO'
         ELSE
            AVencDup(CPAC) = DTOC(VENC)
         ENDIF
      ENDIF   
   ENDIF   
   CPAC = CPAC + 1
   QPARC = QPARC - 1
ENDDO
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
        DIF  = NDIAMES(VENC)
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
