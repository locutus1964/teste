* AV4300 - LISTA OS CLIENTES CADASTRADOS - ORDEM DE RAZAO SOCIAL - 27/11/86
* SISTEMA ADMINISTRACAO DE VENDAS
* ADAO ELI PEREIRA - 22-04-90
*
DO TESTIMP2
  IF CP = 'N'
    RETURN
  ENDIF
*
  MENS = 'Classificado por: C¢digo (C), R.Social (R) ou N.Guerra (G)? ' 
  DO PEGRESP WITH [CRG]
    IF MC = 'C'
      USE CLIEN ORDER P_CGC
      TIT = 'CLIENTES CADASTRADOS - POR CODIGO'
    ELSE
      IF MC = 'G'
        USE CLIEN ORDER X_GUE
        TIT = 'CLIENTES CADASTRADOS - POR NOME GUERRA'
      ELSE
        ? MENSAG('Classificando Clientes por Ordem de Raz„o Social... Aguarde')
        USE CLIEN
        INDEX ON RAZ TO CADWORK.IDX
        USE CLIEN INDE CADWORK.IDX
        TIT = 'CLIENTES CADASTRADOS - POR RAZAO SOCIAL'
      ENDIF
    ENDIF
*
pagctr = 0
tamrel = 2
*
? MENSAG ('Imprimindo Relat¢rio dos Clientes cadastrados... Aguarde')
*
DO DEFSAIDA
*
do while !EOF()
*
  ? CABEC('AV04300',TIT,131)
*
  @ 3,01   say 'COD/LOJA/N.GUER'
  @ 3,22   say 'R.SOCIAL/ENDERECO/CONTATO'
  @ 3,53   say 'CIDADE/VENCTOS'
  @ 3,74   say 'UF/CEP/CM'
  @ 3,84   SAY 'I.E/SUF/CTA'
  @ 3,99   say 'TEL/FAX'
  @ 3,113  SAY 'M/E/C'
  @ 3,121  SAY 'V/T/CP'
  @ 4,1    SAY REPLICATE('-',131)
*
  lin = 6
  
  do while .not. EOF() .and. lin < 58
    @ lin,01 say CGC
    @ lin,22 say LEFT(RAZ,30)
    @ lin,53 say CID
    @ lin,74 say EST
    @ lin,84 say INS
    @ lin,99 say DDD1
    @ lin,104 say FON1
    @ lin,122 say VEN
*
    LIN = LIN + 1
*
    @ lin,05 say GUE
    @ lin,22 say RTRIM(END)+' - '+RTRIM(BAI)
*    @ lin,53 say BAI
    @ LIN,74 SAY CEP
    @ lin,84 say ISU
    @ lin,99 say DDD2
    @ lin,104 say FON2
    @ LIN,114 SAY ENT
    @ lin,122 say TRA
*
    LIN = LIN + 1
    @ lin,22 SAY CONTATO
    @ LIN,53 SAY VES
    @ LIN,74 SAY CMU
    @ LIN,84 SAY CON
    @ LIN,99 SAY DDD_FAXC
    @ LIN,104 SAY NUM_FAXC
    @ LIN,122 SAY BOX
    LIN = lin+2
    skip
      IF EOF()
        @ LIN-1,1 SAY REPLICATE('-',131)
        @ LIN,10 SAY 'F I N A L    D E S T E     R E L A T O R I O'
      ENDIF
  enddo
enddo

DO IMPSAIDA

return
