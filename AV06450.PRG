* AV06450 - ATUALIZACAO DE OBSERVACOES A SEREM IMPRESSAS EM PEDIDOS
* SISTEMA ADMINISTRACAO DE VENDAS - 28/03/88
* ADAO ELI PEREIRA - 24-04-90  -  07/12/91
*
? CABTELA(0,0,24,79)
*
USE OBSPED
  IF BOF()
    APPE BLAN
  ENDIF
*
DO WHILE .T.
*
MENS = 'Observa‡”es a imprimir no Pedido'
? MENSAGR(MENS)
@ 01,POS SAY MENS
*
PUBLIC md01,md02,md03,md04,md05
*
STOR SPACE(70) TO md01, md02, md03, md04, md05
*
@ 7,5 SAY D01
  MD01 = D01
@ 9,5 SAY d02
  MD02 = D02
@ 11,5 SAY d03
  MD03 = D03
@ 13,5 SAY d04
  MD04 = D04
@ 15,5 SAY d05
  MD05 = D05
*
  MENS = 'Inclui Observa‡„o ou Altera a existente ?'
  DO PEGRESP WITH [SN]
    if mc = 'N'
      clear
      return
    endif
*
@ 23,01 SAY SPACE(78)
do while .t.
  @ 7,5 get md01
    read
      if !empty(md01)
        exit
      endif
        ? MENSAG('Primeira linha da Observa‡„o n„o pode ser brancos')
enddo
*
@ 09,5 get md02
@ 11,5 get md03
@ 13,5 get md04
@ 15,5 get md05
read
*
  MENS = DADCORR
  DO PEGRESP WITH [SN]
*
    if mc = 'N'
      loop
    endif
*
repl d01 with md01,d02 with md02,d03 with md03,d04 with md04,d05 with md05

exit

enddo
*
return
