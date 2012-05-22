* RMOETAXA - RELATORIO DAS PARIDADES
* SISTEMA CONTROLE IMPORTACAO
* ADAO ELI PEREIRA  V 1.4  10/03/91
*
clos data
do TESTIMP2
  IF CP = 'N'
    RETURN
  ENDIF
?cabtela(0,0,22,79)
mens = 'Relatorio de Moedas Cadastradas'
?mensagr(mens)
@ 01,pos say mens

MMOEDA  = 0
MINICIO = CTOD('')
MFIM    = CTOD('')
MSEL = ' '
@ 09,10 say '       Codigo da Moeda         '
@ 11,10 SAY 'Informe (DD/MM/AA) para Inicio '
@ 13,10 SAY '                         Final '

sele 0
  use nommoeda inde nommoeda.IDX
sele 0
  use cadmoeda inde cadmoeda.IDX

? MENSAG('Se desejado todas as Moedas, entre com Moeda 0')
sele nommoeda
do while .t. 
   @ 09,42 get mmoeda pict '999'
   read
   if lastkey() = 27
      close data
      return
   endif   
   if mmoeda = 0    
      exit
      MSEL = 'N'
   else
      MSEL = 'S'
      if !seek(str(mmoeda,3))
         ?mensag('Moeda nao Cadastrada - Enter')
         =INKEY(0)
         loop
      else
         exit
        endif
   endif
enddo         
         
@ 11,42 GET MINICIO 
@ 13,42 GET MFIM 
read

mc = ' '
mens = dadcorr
do pegresp with [SN]

IF MC = 'N'
   RETURN
ENDIF

sele CADMOEDA
IF MMOEDA = 0 
   COPY TO CADWORK FOR DATA >= MINICIO .AND. DATA <= MFIM
ELSE
   COPY TO CADWORK FOR MOEDA = MMOEDA .AND. DATA >= MINICIO;
           .AND. DATA <= MFIM)
ENDIF   

sele 0
USE CADWORK
INDEX ON STR(MOEDA,3)+DTOS(DATA) TO CADWORK.IDX

pagctr = 0
TAMREL = 1
*
  MENS = 'Imprimindo relat¢rio das Paridades em ordem de Data... Aguarde'
  ? MENSAG(MENS)

 DO DEFSAIDA
*
sele CADWORK
set ORDER to CADWORK
  
do while .not. EOF()
  pagctr = pagctr + 1
@ 1,1 say mrel
@ 1,27 say 'RMOETAXA'
@ 1,40 say 'PARIDADE DAS MOEDAS'
@ 1,64 say DTOC(hoje)
@ 1,74 say 'PAG '+STR(PAGCTR,3)
@ 2,1 SAY REPLICATE('-',82)
@ 3,1  say 'COD'
@ 3,7  say 'DESCRICAO '
@ 3,30 SAY 'ABREVIAT'
@ 3,44 SAY 'ANO/MES'
@ 3,58 SAY '  DOLARES'
@ 3,73 SAY 'CRUZEIROS'
@ 4,1 SAY REPLICATE('-',82)
*
lin = 5
SELE NOMMOEDA

do while .not. EOF() .and. lin < 56
*
    IF MSEL = 'S'
       SEEK(STR(MMOEDA,3))  
      @ lin,1 say MOEDA
        MMOEDA = MOEDA
      @ lin,7 say DMOEDA
      @ LIN,34 SAY AMOEDA
    ELSE
       IF MOEDA <> MMOEDA .OR. LIN = 6 
         @ lin,1 say MOEDA
           MMOEDA = MOEDA
         @ lin,7 say DMOEDA
         @ LIN,34 SAY AMOEDA
       endif
    ENDIF
    SELE CADWORK
    IF SEEK(STR(MMOEDA,3))
      DO WHILE MOEDA = MMOEDA      
        @ lin,45 say DATA
        @ lin,55 say str(cdolar,12,6)
        @ lin,70 say str(ccz,12,6)
        mmoeda = moeda
        lin = lin + 1
        skip
      ENDDO
    ELSE
      LIN = LIN + 2  
    ENDIF  
        SELE NOMMOEDA
        SKIP  
        if eof()
          @ lin,1 say replicate('-',82)
          @ lin+1,10 say 'F I N A L    D E S T E    R E L A T O R I O'
        endif  
  enddo
  
  enddo
  
  DO IMPSAIDA
  
return





