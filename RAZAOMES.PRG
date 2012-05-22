* RAZAOMES - RAZAO GERAL DE CLIENTES
* Imprime os clientes que tiveram pagamentos ou emissao de documentos no
* mes de competencia  ou  cujo saldo inicial seja <> de zero.
* Se o mes for encerrado, ira gerar os saldos iniciais para o mes atual
* de competencia.

DO TESTIMP2
IF CP = 'N'
   RETURN
ENDIF

? CABTELA(0,0,24,79)
MENS = 'Raz„o Geral do Mes por Cliente'
? MENSAGR(MENS)
@ 01,POS SAY MENS

MCOMPET = SPACE(7)
MCGC    = SPACE(16)
@ 08,10 SAY 'Informe (MES/ANO) p/emiss„o             'get MCOMPET pict '99/9999'
read

MENS = DADCORR
DO PEGRESP WITH [SN]
IF MC = 'N'
   RETURN
ENDIF

DCOMPET = SUBSTR(DTOS(CTOD('01/' + MCOMPET)),1,6)

ANO = VAL(SUBS(MCOMPET,4,4))
MES = VAL(SUBS(MCOMPET,1,2))
MESANT = (MES - 1)
MDD = SPACE(01)

IF MESANT = 0
   MESANT  = 12
   ANOANT  = (ANO - 1)
ELSE
   MESANT = MESANT
   ANOANT = ANO
ENDIF

UCOMPET = STR(ANOANT,2)+'/'+PADL(MESANT,2,'0')

@ 10,10 say 'Preparando os Arquivos... Aguarde '

IF MES = 1 .OR. MES = 3 .OR. MES = 5 .OR. MES = 7 .OR. MES = 8 .OR. MES = 10 ;
           .OR. MES = 12
  UDIA = 31
ELSE
  IF MES = 4 .OR. MES = 6 .OR. MES = 9
    UDIA = 30
  ELSE
    STOR YEAR(hoje) / 4 TO BIS
    IF (INT(BIS) - BIS) = 0.00000
      UDIA = 29
    ELSE
      UDIA = 28
    ENDIF
  ENDIF
ENDIF

* SOMA AS DUPLICATAS DESCONTADAS NA VARIAVEL VALDESC

DI = STR(UDIA,2)+'/'+MCOMPET      && ULTIMO DIA DO MES DE COMPETENCIA

USE DUPL
* SUM VDP TO VALDESC FOR EMIS <= CTOD(DI)
 SUM (VDP-desc) TO VALDESC FOR EMIS <= CTOD(DI) .AND. ! EMPTY(DTDESCTO) .AND.;
                               ! EMPTY(BXDESCTO) .AND. ! EMPTY(PAG)

USE DUPL
COPY TO CADWORK FOR EMPTY(DTCANCEL) .AND. YEAR(EMIS) = MCOMPET .OR.;
                    EMPTY(DTCANCEL) .AND. YEAR(PAG) = MCOMPET
USE CADWORK
INDEX ON CGC + STR(NDUP,6) + STR(PDUP,1) TO CADWORK.IDX
CLOS DATA

STOR 0.00 TO TOTVENDA,TOTDEVOL,TOTDESC,TOTJURO,TOTRECEB 
STOR 0.00 TO GERVENDA,GERDEVOL,GERDESC,GERJURO,GERRECEB
STOR 0.00 TO SALDOINI,SALDOFIM,GERFIM,GERINI
STOR 0.00 TO FORVENDA,FORDEVOL,FORDESC,FORJURO,FORRECEB

SELE 0
USE RAZAOCL2
ZAP
SELE 0
USE RAZAOCLI INDE RAZAOCLI.IDX
SELE 0
USE CADWORK  INDE CADWORK.IDX
SELE 0
USE CLIEN    ORDER P_CGC

@ 12,10 say 'Imprimindo Raz„o Geral do mes por Cliente... Aguarde'
PAGCTR = 0
TAMREL = 2
TITULO = 'RAZAO GERAL DO MES '+MCOMPET+' POR CLIENTE'+DI

DO DEFSAIDA

SELE CLIEN
DO WHILE .NOT. (EOF() .or. bof())
   ? CABEC('RAZAOMES',TITULO,131)
   @ 3,001 SAY 'DUPLIC.'
   @ 3,010 SAY 'EMISSAO'
   @ 3,019 SAY 'VENCTO.'
   @ 3,028 SAY 'VALOR DUPLIC'
   @ 3,045 SAY 'VALOR DEVOLUC'
   @ 3,062 SAY 'VALOR DESCTOS'
   @ 3,077 SAY 'VALOR JUROS'
   @ 3,092 SAY 'VAL RECEBIDO'
   @ 3,110 SAY 'SALDO CLIENTE'
   @ 3,130 SAY 'DD'
   @ 4,1 say REPLICATE('-',131)
   lin = 5

   do while .not.(EOF() .or. bof()) .and. lin < 60
      MCGC = CGC
      MRAZ = RAZ
      MSALDOINI = 0.00

      SELE RAZAOCLI
      SEEK UCOMPET + MCGC
      IF !(EOF() .OR. BOF())
         MSALDOINI = SALDO
      ENDIF
      GERINI = (GERINI + MSALDOINI)

      SELE CADWORK
      SEEK MCGC
      IF !(EOF() .OR. BOF()) .OR. MSALDOINI > 0
            @ LIN,001 SAY MRAZ
            @ LIN,110 SAY MSALDOINI PICT '99,999,999,999.99'
            LIN = LIN + 1
*         IF !(EOF() .OR. BOF())
         IF (EOF() .OR. BOF()) .AND. MSALDOINI > 0
            ***** Sem movimento e saldo maior que zero *****
            @ LIN,01 SAY REPLICATE('-',131)
            LIN = LIN + 2
            SELE RAZAOCL2
            APPE BLAN
            REPL CGC WITH MCGC,COMPET WITH MCOMPET,SALDO WITH MSALDOINI

*********** INCLUIDO O SELE CLIEN, O SKIP E O LOOP EM 05/04/93 - ADAO ******
            SELE CLIEN
            SKIP
*************************************
            if !(eof() .or. bof())
               LOOP
            endif
*************************************
*         ENDIF

      ELSE

         DO WHILE CGC = MCGC
            ***** Com movimento *****
            @ LIN,001 SAY NDUP
            @ LIN,010 SAY EMIS
            @ LIN,019 SAY VENC
            IF SUBS(EMIS,4,7) = MCOMPET
               @ LIN,28 SAY VDP PICT '9,999,999,999.99'
               TOTVENDA = (TOTVENDA + VDP)
               GERVENDA = (GERVENDA + VDP)
               FORVENDA = (FORVENDA + VDP)
            ENDIF
            IF SUBS(PAG,4,7) = MCOMPET
               @ LIN,045 SAY VALDEV PICT '9,999,999,999.99'
               TOTDEVOL = (TOTDEVOL + VALDEV)
               GERDEVOL = (GERDEVOL + VALDEV)
               FORDEVOL = (FORDEVOL + VALDEV)
               @ LIN,062 SAY DESC PICT '999,999,999.99'
               TOTDESC = (TOTDESC + DESC)
               GERDESC = (GERDESC + DESC)
               FORDESC = (FORDESC + DESC)
               @ LIN,077 SAY JUROS PICT '999,999,999.99'
               TOTJURO = (TOTJURO + JUROS)
               GERJURO = (GERJURO + JUROS)
               FORJURO = (FORJURO + JUROS)
               LIQUIDO =((VDP) - (VALDEV + DESC))
               @ LIN,092 SAY LIQUIDO PICT '9,999,999,999.99'
               TOTRECEB = (TOTRECEB + LIQUIDO)
               GERRECEB = (GERRECEB + LIQUIDO)
               FORRECEB = (FORRECEB + LIQUIDO)

               IF !EMPTY(DTDESCTO) .AND. EMPTY(BXDESCTO) .AND. EMPTY(PAG)
                 MDD = '**'
               ELSE
                 MDD = SPACE(2)
               ENDIF

            ENDIF
            SKIP
            IF EOF() .OR. bof() .or. CGC <> MCGC
               SALDOFIM = (MSALDOINI + TOTVENDA - TOTDEVOL ;
                          - TOTDESC - TOTRECEB)
               @ LIN,110 SAY SALDOFIM PICT '99,999,999,999.99'
               @ LIN,130 SAY MDD
               LIN = LIN + 1
               @ LIN,005 SAY 'TOTAIS CLIENTE'
               @ LIN,027 SAY FORVENDA PICT '99,999,999,999.99'
               @ LIN,045 SAY FORDEVOL PICT '9,999,999,999.99'
               @ LIN,062 SAY FORDESC PICT '999,999,999.99'
               @ LIN,077 SAY FORJURO PICT '9,999,999,999.99'
               @ LIN,094 SAY FORRECEB PICT '99,999,999,999.99'
               LIN = LIN + 1
               @ LIN,001 SAY REPLICATE('-',131)
               LIN = LIN + 1
               FORVENDA = 0
               FORDEVOL = 0
               FORDESC  = 0
               FORJURO = 0.00
               FORRECEB = 0.00

               SELE RAZAOCL2
               APPE BLAN
               REPL CGC WITH MCGC,COMPET WITH MCOMPET,SALDO WITH SALDOFIM
               TOTVENDA = 0
               TOTDEVOL = 0
               TOTDESC  = 0
               TOTJURO  = 0.00
               TOTRECEB = 0
               SALDOFIM = 0.00
            ENDIF

            SELE CADWORK
            LIN = LIN + 1
            IF LIN > 60
               @ LIN,001 SAY REPLICATE('-',131)
               EJECT
               ? CABEC('RAZAOMES',TITULO,131)
               @ 3,001 SAY 'DUPLIC.'
               @ 3,010 SAY 'EMISSAO'
               @ 3,019 SAY 'VENCTO.'
               @ 3,028 SAY 'VALOR DUPLIC'
               @ 3,045 SAY 'VALOR DEVOLUC'
               @ 3,062 SAY 'VALOR DESCTOS'
               @ 3,077 SAY 'VALOR JUROS'
               @ 3,092 SAY 'VAL RECEBIDO'
               @ 3,110 SAY 'SALDO CLIENTE'
               @ 4,1 say REPLICATE('-',131)
               lin = 5
            ENDIF
         ENDDO

        ENDIF

      ELSE
         ***** Saldo <= 0 e sem movimento *****
         SELE RAZAOCL2
         APPE BLAN
         REPL CGC WITH MCGC,COMPET WITH MCOMPET,SALDO WITH MSALDOINI
      ENDIF

      SELE CLIEN
      SKIP
      IF EOF() .or. bof()
         @ LIN,001 SAY REPLICATE('-',131)
         LIN = LIN + 1
         @ LIN,005 SAY 'TOTAIS DO MES'
         @ LIN,028 SAY GERVENDA PICT '99,999,999,999.99'
         @ LIN,045 SAY GERDEVOL PICT '9,999,999,999.99'
         @ LIN,062 SAY GERDESC PICT '999,999,999.99'
         @ LIN,077 SAY GERJURO PICT '9,999,999,999.99'
         @ LIN,094 SAY GERRECEB PICT '99,999,999,999.99'
         GERFIM = (GERINI+GERVENDA-GERDEVOL-GERDESC-GERRECEB)
         @ LIN,112 SAY GERFIM PICT '999,999,999,999.99'
         LIN = LIN + 1
         @ LIN,001 SAY REPLICATE('-',131)
         LIN = LIN + 1
         IF LIN > 50
            EJECT
            ? CABEC('RAZAOMES',TITULO,131)
            LIN = 10
         ELSE
            LIN = LIN + 2
         ENDIF
         ?? CHR(30) + '0'
         @ LIN,01 SAY 'RESUMO DO MES'
         @ LIN,20 SAY '    SALDO INICIAL     '
         @ LIN,52 SAY GERINI PICT '99,999,999,999.99'
         LIN = LIN + 1
         @ LIN,20 SAY '(+) VENDAS DO MES'
         @ LIN,52 SAY GERVENDA PICT '99,999,999,999.99'
         LIN = LIN + 1
         @ LIN,20 SAY '(-) DEVOLUCOES DE CLIENTES'
         @ LIN,52 SAY GERDEVOL PICT '99,999,999,999.99'
         LIN = LIN + 1
         @ LIN,20 SAY '(-) DESCONTOS CONCEDIDOS'
         @ LIN,52 SAY GERDESC PICT '99,999,999,999.99'
         LIN = LIN + 1
         @ LIN,20 SAY '(-) LIQUIDO RECEBIDO'
         @ LIN,52 SAY GERRECEB PICT '99,999,999,999.99'
         LIN = LIN + 1
         @ LIN,50 SAY '-------------------'
         LIN = LIN + 1
         @ LIN,20 SAY '(=) SALDO FINAL DE CLIENTES'
         @ LIN,51 SAY GERFIM PICT '999,999,999,999.99'
         LIN = LIN + 2
         @ LIN,20 SAY '(-) DUPLICATAS DESCONTADAS'
         @ LIN,52 SAY VALDESC PICT '99,999,999,999.99'
         LIN = LIN + 2
         @ LIN,20 SAY '(=) SALDO LIQUIDO DE CLIENTES'
         @ LIN,52 SAY (GERFIM - VALDESC) PICT '99,999,999,999.99'
         LIN = LIN + 2
         @ LIN,20 SAY '    JUROS/CORRECAO RECEBIDOS'
         @ LIN,52 SAY GERJURO PICT '99,999,999,999.99'
      ENDIF
   ENDDO
ENDDO

DO IMPSAIDA

KEYCLI = SPACE(21)
MENS = 'Mes encerrado corretamente? Pode atualizar saldo final ?'
DO PEGRESP WITH [SN]

IF MC = 'S'

   SELE RAZAOCL2

   DO WHILE !EOF()
      MCOMPET = COMPET
      MCGC    = CGC
      MSALDO  = SALDO

      SELE RAZAOCLI
      SEEK SUBS(MCOMPET,4,4)+'/'+SUBS(MCOMPET,1,2) + MCGC
      IF (EOF() .OR. BOF())
         APPE BLAN
         REPL CGC WITH MCGC,COMPET WITH MCOMPET,SALDO WITH MSALDO
      else
         @ lin,10 say 'Erro, competencia nao gravada '+mcgc+' '+STR(msaldo,14,2)+' '+mcompet
         LIN = LIN + 1
      ENDIF

      SELE RAZAOCL2
      SKIP
   ENDDO
ENDIF

USE RAZAOCL2 EXCL
ZAP
RETURN
