* AV05920 - CALCULO DOS VALORES E IMPRESSAO DA CURVA ABC DE MATERIAL
* SISTEMA ADMINISTRATIVO DE VENDAS
* ADAO ELI PEREIRA - 26/11/91 - 03/10/92
*
DO TESTIMP2
  IF CP = 'N'
    RETURN
  ENDIF
*
  ? JANELA(14,15,18,77)
  ? BORDA(14,15,18,77)
*
MACTOT = 0
MENS = 'Informe at‚ que percentual sobre o total imprimir '
@ 16,18 SAY MENS get mactot PICT '999'
  READ
    IF MACTOT > 100 .OR. MACTOT = 0
      ? MENSAG('Percentual n„o pode ser ZERO ou maior que 100 - ENTER')
      =INKEY(0)
      RETURN
    ENDIF

    MENS = DADCORR
    DO PEGRESP WITH [SN]
      IF MC = 'N'
        RETURN
      ENDIF
*
STOR 0 TO PAGCTR, MACU, MVAA, MPR1, MITE, MLIN, VALT, TITE, VALAC
ACTOT  = 0.00
MREF   = SPACE(04)
MUNI   = SPACE(02)
TAMREL = 2
*
? MENSAG('Calculando e Classificando os valores para Curva ABC... Aguarde')
*
USE CURGERAL
  DO WHILE !EOF()
    STOR (VALACU + VALT) TO VALT
    STOR (TITE   + 1   ) TO TITE
      SKIP
  ENDDO
    CLOS DATA
*
  USE CURGERAL
    INDEX ON VALMES TO CURVAS.IDX
    CLOS DATA
*
SELE 0
  USE PROD INDEX PROD.IDX
SELE 0
  USE CURGERAL INDEX CURVAS.IDX
  GO BOTT
*
? MENSAG('Imprimindo Relat¢rio Curva ABC... Aguarde')

DO DEFSAIDA
*
SELE A
DO WHILE !BOF()
  ? CABEC('AV05920','CURVA ABC P/MATERIAL (VENDAS ATE O MES - VALOR MEDIO)',131)
    @ 3,110 SAY 'P E R C E N T U A I S'
    @ 4,1   SAY 'REF'
    @ 4,06  SAY 'DESCRICAO DO PRODUTO'
    @ 4,54  SAY 'UM'
    @ 4,59  SAY 'QUANTID.'
    @ 4,69  SAY 'PREC UNIT'
    @ 4,83  SAY 'TOT ITEM'
    @ 4,95  SAY 'ACUMULADO'
    @ 4,105 SAY 'ITEM'
    @ 4,111 SAY 'S/ITEM'
    @ 4,118 SAY 'S/ACUM'
    @ 4,125 SAY 'S/TOTA'
    @ 5,1   SAY REPLICATE('-',131)
      LIN = 6
        DO WHILE !BOF() .AND. LIN < 58
          @ LIN,1   SAY REF
            STOR REF TO MREF
              SELE PROD
                SEEK MREF
                  IF .NOT. (EOF() .OR. BOF())
                    @ LIN,06  SAY SUBS(DES,1,48)
                    @ LIN,54  SAY UNI
                  ELSE
                    @ LIN,13 SAY 'MATERIAL NAO CADASTRADO'
                  ENDIF
              SELE CURGERAL
          @ LIN,57  SAY QTDACU PICT '9999,999'
          @ LIN,68  SAY (VALACU/QTDACU) PICT '9999,999.99'
          @ LIN,78  SAY VALACU PICT '9999,999,999'
            STOR (MITE + 1) TO MITE
            STOR (VALAC + VALACU) TO VALAC
          @ LIN,91 SAY VALAC PICT '9999,999,999'
          @ LIN,104 SAY MITE PICT '99999'
          @ LIN,111 SAY ((MITE / TITE) * 100) PICT '999.99'
          @ LIN,118 SAY ((VALACU / VALT) * 100) PICT '999.99'
            STOR ((VALAC / VALT) * 100) TO ACTOT
            IF ACTOT > MACTOT .OR. EOF()
              CLOS DATA
              @ LIN+1,1 SAY REPLICATE('-',131)
              @ LIN+2,10 SAY 'F I N A L   D E S T E    R E L A T O R I O'
              DO IMPSAIDA
              RETURN
            ENDIF
          @ LIN,125 SAY ACTOT PICT '999.99'
            STOR LIN + 1 TO LIN
            SKIP - 1
            IF BOF()
              @ LIN,1 SAY REPLICATE('-',131)
              @ LIN+1,10 SAY 'F I N A L   D E S T E    R E L A T O R I O'
            ENDIF
        ENDDO
ENDDO
*
DO IMPSAIDA
USE CADWORK
ZAP
USE CURTOTAL
ZAP
RETURN
