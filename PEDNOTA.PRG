* PEDNOTA - SELECAO/CRIACAO E IMPRESSAO DE PEDIDOS E AS NOTAS FISCAIS
* SISTEMA ADMINISTRACAO DE VENDAS
* ADAO ELI PEREIRA - 6/5/90
*
DO TESTIMP2

  IF CP = 'N'
    RETURN
  ENDIF

STOR SPACE(8) TO INICIO,FIM
? JANELA(15,44,20,78)
? BORDA(15,44,20,78)

@ 17,46 SAY 'Data Inicial p/sele‡„o' GET INICIO PICT '99/99/99'
  READ
@ 18,46 SAY 'Data Final p/sele‡„o  ' GET FIM PICT '99/99/99'
  READ
  MENS = DADCORR
  DO PEGRESP WITH [SN]
   IF MC = 'N'
     RETURN
   ENDIF
*
? MENSAG('Selecionando as Notas Fiscais... Aguarde')
CLOS DATA
USE CADNF
  COPY TO CADWORK FOR CTOD(DEMI) >= CTOD(INICIO) .AND. CTOD(DEMI) ;
         <= CTOD(FIM) .AND. LEFT(CANCELNF,2) = '  ' .AND. PED > 0
  
USE CADWORK
INDEX ON str(NNF,6) TO CADWORK.IDX
CLOS DATA
USE CADWORK INDE CADWORK.IDX

TOTAL TO CADWORK2 ON NNF FIELDS VAL,IPI

USE CADWORK2
INDEX ON STR(PED,6) TO CADWORK.IDX

? MENSAG('Totalizando os Pedidos selecionados... Aguarde')
    
CLOS DATA
*
SELE 0
  USE CLIEN ORDER P_CGC
SELE 0
  USE CADWORK2 INDE CADWORK.IDX
*
pagctr = 0
TAMREL = 2
*
DO DEFSAIDA

stor space(20) to mcgc
*
do while .not. EOF()
  stor pagctr+1 to pagctr
@ 1,1    say MREL
@ 1,37   SAY 'PEDNOTA'
@ 1,50 say 'PEDIDOS ATENDIDOS COM NOTAS FISCAIS DE '+INICIO+' A '+FIM
@ 1,110  say DTOC(hoje)
@ 1,120  say 'PAG '+str(pagctr,3)
*
@ 2,1    SAY REPLICATE('-',131)
@ 3,1    say 'PEDIDO'
@ 3,8    SAY 'CLIENTE'
@ 3,57   SAY 'CIDADE'
@ 3,83   SAY 'UF'
@ 3,86   SAY 'VEND'
@ 3,104  SAY 'VALOR S/IPI'
@ 3,119  SAY 'VALOR C/IPI'
@ 4,79   SAY 'N.FISC'
@ 4,86   SAY 'EMISSAO'
@ 4,95   SAY 'ENTREGA'
@ 4,104  SAY 'VALOR DO IPI'

@ 5,1    SAY REPLICATE('-',131)
MPED = 0

stor 5 to linha
  do while .not. EOF() .and. linha < 62
  
  IF LINHA = 5 .OR. PED <> MPED
    LINHA = LINHA + 1
    @ linha,1 say PED PICT '999999'
    MCGC = CGC
    SELE CLIEN
    IF SEEK(MCGC)
    
      @ linha,08 say RAZ
      @ linha,57 say CID
      @ linha,83 say EST
      @ linha,86 say VEN
    ENDIF
  ENDIF
  
  SELE CADWORK2  
  @ linha,104 say VAL PICT '999,999,999.99'
  @ LINHA,119 SAY (VAL + IPI) PICT '999,999,999.99'
  
  linha = linha + 1  
  @ linha,079 say NNF PICT '999999'
  @ linha,086 say DEMI
  @ linha,095 say ENT
  @ linha,104 say IPI PICT '999,999,999.99'
    MPED = PED

  stor linha+1 to linha
    skip
      IF EOF()
        @ LINHA,1 SAY REPLICATE('-',131)
        @ LINHA+1,10 SAY 'F  I  N  A  L     D E S T E     R E L A T O R I O'
      ENDIF
  enddo
enddo

DO IMPSAIDA

return
