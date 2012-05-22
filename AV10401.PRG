* AV10401 - CALCULO E IMPRESSAO DAS VENDAS DO MES POR CLIENTE
* SISTEMA ADMINISTRACAO DE EMPRESAS
* ADAO ELI PEREIRA - 02-08-90

DO TESTIMP2
IF CP = 'N'
   RETURN
ENDIF

Mini = CTOD('')
mfim = CTOD('')
MMOEDA  = 0

? CABTELA(0,0,24,79)
MENS = 'Vendas efetuadas por Cliente'
? MENSAGR(MENS)
@ 01,POS SAY MENS

@ 08,17 say 'Data Inicial....... '
@ 10,17 say '     Final......... '
@ 12,17 say 'Codigo da Moeda CR$ '
@ 14,17 say 'Nome de Guerra..... '

? MENSAG('Informe a data inicial do relatorio no formato (DD/MM/AAAA)')
@ 08,38 get mini 
READ
? MENSAG('Informe a data final do relatorio no formato (DD/MM/AAAA)')
@ 10,38 get mfim 
READ
? mensag('Informe o codigo da moeda equivalente ao CR$')
do while .t.
  @ 12,38 get mmoeda pict '999'
  read

  if lastkey() = 27
    clos data
    return
  endif

  if mmoeda = 0
    ? MENSAG(CNUMER)
    loop
 endif
 USE CADMOEDA INDE CADMOEDA
 seek str(mmoeda,3)
 if (eof() .or. bof())
    ? MENSAG('Moeda n„o cadastrada. Corrija ou (ESC) para encerrar')
    loop
 else
    exit
 endif
enddo

mguerra = space(15)
? mensag('Para relat¢rio sint‚tico n„o informe o Nome de Guerra do Cliente.')
use clien ORDER X_GUE
do while .t.
   @ 14,38 get mguerra pict '@!'
   read
   if mguerra = space(10)
      return
   endif
   seek mguerra
   if eof()
      ? MENSAG('Cliente nao cadastrado.')
      =INKEY(0)
      loop
   endif
   mcgc = cgc
   exit
enddo

? MENSAG('Selecionando Notas Fiscais de Venda... Aguarde')
USE DUPL
COPY TO lixo1 FOR emis >= mini .and.;
                  emis <= mfim .and.;
                  EMPTY(DTCANCEL) .and.;
                  cgc = mcgc
? MENS= 'Classificando Vendas por fornecedor... Aguarde'
? MENSAGR(MENS)
@ 16,POS SAY MENS
USE lixo1
INDEX ON CGC + DTOS(emis) + STR(NDUP,6) TO lixo1 
CLOS DATA

? MENSAG('Imprimindo Relatorio... Aguarde')
PAGCTR = 0
TAMREL = 2

TITULO = 'VENDAS EFETUADAS DE ' + DTOC(MINI) + ' A ' + DTOC(MFIM)

DO DEFSAIDA

*** As variaveis que comecam com F, sao para acumular o mes ***

STOR 0.00 TO Tdifer,Gdifer,Fdifer,MVDP
STOR 0    TO TVDP,TMUSE,TMUSV
STOR 0    TO GVDP,GMUSE,GMUSV

MCGC = SPACE(16)

SELE C
USE CADMOEDA INDEX CADMOEDA
sELE B
USE CLIEN ORDER P_CGC
SELE A
USE lixo1 INDEX lixo1

SELE A
IF BOF()
SET DEVI TO SCRE
  ? MENSAG('Sem vendas no Per¡odo - ENTER')
  =INKEY(0)
  CLOS DATA
  RETUR
ENDIF

DO WHILE .NOT. EOF()
   ? CABEC('AV10401',TITULO,132)
   @ 04,001 SAY '---------RAZAO SOCIAL DO CLIENTE--------'
   @ 04,042 SAY '-N.F.-'
   @ 04,049 SAY 'DT EMIS.'
   @ 04,058 SAY 'DP'
   @ 04,061 SAY 'DT VENC.'
   @ 04,070 SAY '-VALOR EM CR$--'
   @ 04,086 SAY '--US$ DT EMIS--'
   @ 04,102 SAY '--US$ DT VENC--'
   @ 04,118 SAY '-PERDA EM US$--'
   @ 05,001 SAY '----------------------------------------'
   @ 05,042 SAY '------'
   @ 05,049 SAY '--------'
   @ 05,058 SAY '--'
   @ 05,061 SAY '--------'
   @ 05,070 SAY '---------------'
   @ 05,086 SAY '---------------'
   @ 05,102 SAY '---------------'
   @ 05,118 SAY '---------------'
   LIN = 6

   DO WHILE .NOT. EOF() .AND. LIN < 60
      SELE A
      IF CGC <> MCGC
         CCGC = CGC
         SELE B
         SEEK CCGC
         IF !(EOF() .OR. BOF())
            @ LIN,01 SAY RAZ
         ELSE
            @ LIN,01 SAY 'CLIENTE NAO CADASTRADO'
         ENDIF
         MCGC = CGC
         LIN = LIN + 1
         sele a
      ENDIF

     * GVDP   = (GVDP + MVDP)

      MMOEDA = MOEDA
      IF MMOEDA = 0
        MMOEDA = 1
      ENDIF

      IF MMOEDA <> 1      && SE DIF DE 1 DEVE SER URV
        XVENC = EMIS
      ELSE
        XVENC = VENC
      ENDIF

      MMOEDA = 1           && FORCA MOEDA CRUZEIRO


      @ LIN,42 SAY NDUP PICT '999999'
      @ LIN,49 SAY EMIS
      @ LIN,58 SAY PDUP PICT '9'
      @ LIN,61 SAY VENC
      MVENC = SUBS(XVENC,7,2)+'/'+SUBS(XVENC,4,2)+'/'+SUBS(XVENC,1,2)
      MEMIS = SUBS(EMIS,7,2)+'/'+SUBS(EMIS,4,2)+'/'+SUBS(EMIS,1,2)

      MVDP  = (VDP - DESC)
      @ LIN,70 SAY MVDP PICT '9999,999,999.99'
      TVDP   = (TVDP + MVDP)
      GVDP   = (GVDP + MVDP)
      SELE C
      SEEK STR(MMOEDA,3) + MEMIS
      IF !(EOF() .OR. BOF())
         MCDOLAR = CDOLAR
      ELSE
         SEEK STR(MMOEDA,3) + SUBS(MEMIS,1,5)
         IF !(EOF() .OR. BOF())
            MCDOLAR = CDOLAR
         ELSE
            MCDOLAR = 1.00
         ENDIF
      ENDIF
      MUSE = (MVDP / MCDOLAR)
      @ LIN,86 SAY MUSE PICT '9999,999,999.99'

      SEEK STR(MMOEDA,3) + MVENC
      IF !(EOF() .OR. BOF())
         MCDOLAR = CDOLAR
      ELSE
         SEEK STR(MMOEDA,3) + SUBS(MVENC,1,5)
         IF !(EOF() .OR. BOF())
            MCDOLAR = CDOLAR
         ELSE
            MCDOLAR = 1.00
         ENDIF
      ENDIF
      MUSV = (MVDP / MCDOLAR)
      @ LIN,102 SAY MUSV PICT '9999,999,999.99'

      TMUSE = (TMUSE + MUSE)
      TMUSV = (TMUSV + MUSV)
      GMUSE = (GMUSE + MUSE)
      GMUSV = (GMUSV + MUSV)

      difer = muse - musv
      @ LIN,118 SAY difer PICT '9999,999,999.99'
      Tdifer = (Tdifer + difer)
      Gdifer = (Gdifer + difer)
      LIN = LIN + 1

      sele a
      SKIP
      IF CGC <> MCGC .OR. EOF()
         @ LIN,10 SAY 'TOTAIS DO FORNECEDOR'
         @ LIN,70 SAY TVDP PICT '9999,999,999.99'
         @ LIN,86 SAY TMUSE PICT '9999,999,999.99'
         @ LIN,102 SAY TMUSV PICT '9999,999,999.99'
         @ LIN,118 SAY Tdifer PICT '9999,999,999.99'

         STOR 0.00 TO TVDP,TMUSE,TMUSV,Tdifer
         LIN = LIN + 1
         LIN = LIN + 1
      ENDIF
      IF EOF()
         @ LIN,10 SAY 'TOTAIS DAS VENDAS'
         @ LIN,70 SAY GVDP PICT ' 9999,999,999.99'
         @ LIN,86 SAY GMUSE PICT ' 9999,999,999.99'
         @ LIN,102 SAY GMUSV PICT '9999,999,999.99'
         @ LIN,118 SAY Gdifer PICT '9999,999,999.99'
         LIN = LIN + 1
      ENDIF
   ENDDO
ENDDO

DO IMPSAIDA

erase lixo1.dbf
erase lixo1.IDX
RETURN
