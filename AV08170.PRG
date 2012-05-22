* AV08600 - SELECAO/CRIACAO E IMPRESSAO DE DADOS DAS NOTAS FISCAIS
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
  COPY TO CADWORK FOR CTOD(DEMI) >= CTOD(INICIO) .AND. CTOD(DEMI) <= CTOD(FIM) 
  
USE CADWORK

  MENS = 'Classifica por CGC (C), Nota Fiscal (N) ou Tipo (T) '
  DO PEGRESP WITH [CNT]

    ? MENSAG('Classificando as Notas selecionadas... Aguarde')
    
      IF MC = 'C'
        INDEX ON CGC + STR(NNF,6) + REF TO CADWORK.IDX
      ELSE
        IF MC = 'N'
          INDEX ON STR(NNF,6) + REF TO CADWORK.IDX
        ELSE
          INDEX ON TIP + STR(NNF,6) + REF TO CADWORK.IDX
        ENDIF
      ENDIF
*
CLOS DATA
*
SELE C
  USE PROD INDEX PROD.IDX
SELE A
  USE CLIEN ORDER P_CGC
SELE D
  USE CADWORK INDEX CADWORK.IDX
*
pagctr = 0
TAMREL = 2

  IF MC = 'C'
    ? MENSAG('Imprimindo Notas Selecionadas p/CGC... Aguarde')
  ELSE
    IF MC = 'N'
      ? MENSAG('Imprimindo Notas Selecionadas p/N.Fiscal... Aguarde')
    ELSE
      ? MENSAG('Imprimindo Notas Selecionadas p/Tipo de NF... Aguarde')
    ENDIF
  ENDIF
*
DO DEFSAIDA
stor space(20) to mcgc
*
do while .not. EOF()
  stor pagctr+1 to pagctr
  @ 1,1    say MREL
  @ 1,37   SAY 'AV08600'
  IF MC = 'C'
    @ 1,50 say 'NOTAS SELECIONADAS - ORDEM CGC '+INICIO+' A '+FIM
  ELSE
    IF MC = 'N'
      @ 1,50 SAY 'NOTAS SELECIONADAS - ORDEM NF '+INICIO+' A '+FIM
    ELSE
      @ 1,50 SAY 'NOTAS SELECIONADAS - ORDEM TIPO '+INICIO+' A '+FIM
    ENDIF
  ENDIF
  @ 1,110  say DTOC(HOJE)
  @ 1,120  say 'PAG '+str(pagctr,3)
  *
  @ 2,1    SAY REPLICATE('-',131)
  @ 3,1    say 'CGC CLIENTE/LOJA'  
  @ 3,22   SAY 'NR NF'
  @ 3,40   SAY 'REF'
  @ 3,52   SAY 'EMISSAO'
  @ 3,62   SAY 'VALOR ITEM'  
  @ 3,80   SAY 'VALOR ICM'
  @ 3,94   SAY 'C PAG'
  @ 3,100  SAY 'VENCIMENTOS'
  @ 3,117  SAY 'VAL EMBAL'

  @ 4,1   say 'RAZAO SOCIAL'
  @ 4,42   say 'QUANTID'
  @ 4,52   say 'ENTREGA'
  @ 4,62   say ' VALOR IPI'
  @ 4,80   say 'DESC ICM'
  @ 4,94   say 'N.OPE'
  @ 4,100  SAY 'TIPO NF'
  @ 4,108  SAY 'CANCELADA'
  @ 5,1    SAY REPLICATE('-',131)

  stor 6 to linha
  do while .not. EOF() .and. linha < 62
    @ linha,1 say CGC
    @ linha,22 say NNF
    * @ linha,30 say STR(NFI,6)
    @ linha,43 say REF
    @ linha,52 say DEMI
    @ linha,62 say VAL PICT '999,999,999.99'
    @ linha,80 say ICM PICT '99,999,999.99'
    @ linha,94 say CPD
    IF CP1 > 0
      @ linha,100 say STR(CP1,3)
    ENDIF
    IF CP2 > 0
      @ LINHA,104 SAY STR(CP2,3)
    ENDIF
    IF CP3 > 0
      @ LINHA,108 SAY STR(CP3,3)
    ENDIF
    IF CP4 > 0
      @ LINHA,112 SAY STR(CP4,3)
    ENDIF
    linha = Linha + 1
    MCGC = CGC
    SELE A
    SEEK MCGC
    IF .NOT. EOF() .OR. BOF()
      @ linha,1 say LEFT(RAZ,38)
    ELSE
      @ LINHA,1 SAY 'Cliente nao cadastrado'
    ENDIF
    SELE D
    @ linha,40 say QTD PICT '99,999,999'
    @ linha,52 say ENT
    @ linha,62 say IPI PICT '999,999,999.99'
    @ linha,80 say DICM PICT '99,999,999.99'
    @ linha,94 say NOPE
    @ LINHA,102 SAY TIP
    @ LINHA,108 SAY CANCELNF

    stor linha+2 to linha
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