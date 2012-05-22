* RCONDPAG - LISTA AS CONDICOES DE PAGAMENTO
* ADAO ELI PEREIRA - 20/02/92
*
DO TESTIMP2
    IF CP = 'N'
      RETURN
    ENDIF
*
MENS = 'Relat¢rio por C¢digo (C) ou Ordem Alfa (A)? '
DO PEGRESP WITH [CA]
  IF MC = 'C'
    SELE 0
    USE CONDPAG INDE CONDPAG.IDX
    TIT = 'CONDICOES DE PAGAMENTO CADASTRADAS - ORDEM DE CODIGO'
  ELSE
    USE CONDPAG
    INDEX ON DESCCPA1 TO CADWORK.IDX
    SELE 0
    USE CONDPAG INDEX CADWORK.IDX
    TIT = 'CONDICOES DE PAGAMENTO CADASTRADAS - ORDEM ALFA'
  ENDIF
*
pagctr = 0
tamrel = 2
*
MENS='Imprimindo Relat¢rio das Condi‡”es de Pagamento cadastradas... Aguarde'
? MENSAG(MENS)
*
do defsaida

do while .not. EOF()
? CABEC('RCONDPAG', TIT,131)
@ 03,01  say 'C P'
@ 03,05  SAY 'DESCRICAO'
@ 03,46  say 'TIP'
@ 03,50  SAY 'VCTO/MOD'
@ 03,60  say 'VC 1'
@ 03,65  say 'VC 2'
@ 03,70  SAY 'VC 3'
@ 03,75  SAY 'VC 4'
@ 03,080 say 'VC 5'
@ 03,085 say 'VC 6'
@ 03,090 SAY 'VC 7'
@ 03,095 SAY 'VC 8'
@ 03,100 SAY 'VC 9'
@ 03,105 SAY 'VC 10'
@ 03,110 SAY 'VC 11'
@ 03,115 SAY 'VC 12'

@ 04,01  SAY REPLICATE('-',131)
*
lin = 6
  do while .not. EOF() .and. lin < 58
@ lin,01 say STR(CONDCPAG,3)
@ lin,05 say DESCCPA1
@ lin,46 say TIPOCPAG
@ lin,50 say "Qt Dias"
@ lin,60 say STR(VENC01,3)
@ lin,65 say STR(VENC02,3)
@ lin,70 say STR(VENC03,3)
@ lin,75 say STR(VENC04,3)
@ lin,080 say STR(VENC05,3)
@ lin,085 say STR(VENC06,3)
@ lin,090 say STR(VENC07,3)
@ lin,100 say STR(VENC09,3)
@ lin,105 say STR(VENC10,3)
@ lin,110 say STR(VENC11,3)
@ lin,115 say STR(VENC12,3)
  LIN = LIN + 1
@ lin,50 say "Percent"
@ lin,60 say STR(PERC01,5,1)
@ lin,65 say STR(PERC02,5,1)
@ lin,70 say STR(PERC03,5,1)
@ lin,75 say STR(PERC04,5,1)
@ lin,080 say STR(PERC05,5,1)
@ lin,085 say STR(PERC06,5,1)
@ lin,090 say STR(PERC07,5,1)
@ lin,095 say STR(PERC08,5,1)
@ lin,100 say STR(PERC09,5,1)
@ lin,105 say STR(PERC10,5,1)
@ lin,110 say STR(PERC11,5,1)
@ lin,115 say STR(PERC12,5,1)
  LIN = LIN + 2
*
    skip
      IF EOF()
        @ LIN-1,1 SAY REPLICATE('-',131)
        @ LIN,10 SAY 'F I N A L    D E S T E     R E L A T O R I O'
      ENDIF
  enddo
enddo

do impsaida  
  
return


