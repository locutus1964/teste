* AV05996 - LISTA OS CODIGOS DE PRECOS CADASTRADOS (POR TABELA)
* SISTEMA ADMINISTRACAO DE VENDAS
* ADAO ELI PEREIRA - 03/07/90
*
DO TESTIMP2
  IF CP = 'N'
    RETURN
  ENDIF
*
CLOS DATA

MTABFIN = SPACE(05)
MPARCELAS = 0
MDIAS_0DE = 0
MDIAS_1DE = 0
MDIAS_2DE = 0
MDIAS_3DE = 0
MDIAS_4DE = 0
*
  ? JANELA(07,39,15,76)
  ? BORDA(07,39,15,76)
*
  SELE 0
    USE PROD   INDEX PROD    
  SELE 0
    USE TABREF INDEX TABREF
    SET RELATION TO REF INTO PROD
  SELE 0
    USE TABFIN2 INDEX TABFIN2

DO WHILE .T.

  ? MENSAG(ECODIGO)
  
  SELE TABFIN2
  
  @ 09,42 SAY 'Codigo da Tabela    'GET MTABFIN PICT '99/99'
    READ
 
    IF LASTKEY() = 27
      RETURN
    ENDIF     

    SELE TABREF    
    IF !SEEK(MTABFIN)
      ? MENSAG('Tabela de Precos nao cadastrada - ENTER')
      =INKEY(0)
      LOOP
    ENDIF  
    
  SELE TABFIN2
  
  @ 10,42 SAY 'Quantidade de Parcelas 'GET MPARCELAS PICT '9'
  @ 11,42 SAY 'Dias p/fat a prazo     'GET MDIAS_1DE PICT '999'
     READ
     IF MDIAS_1DE > 0
       IF !SEEK(MTABFIN + STR(MPARCELAS,1) + STR(MDIAS_1DE,3))
         ? MENSAG('Sem Tabela de Acrescimo Financeiro para estas Parcelas/Dias - ENTER')
         =INKEY(0)
         LOOP
       ENDIF
     ELSE
       ? MENSAG('Dias para Faturamento a Prazo n„o pode ser ZERO - ENTER')
       =INKEY(0)
       LOOP  
     ENDIF    
     
  @ 12,42 SAY '                       'GET MDIAS_2DE PICT '999'
     READ
     IF MDIAS_2DE > 0
       IF !SEEK(MTABFIN + STR(MPARCELAS,1) + STR(MDIAS_2DE,3))
         ? MENSAG('Sem Tabela de Acrescimo Financeiro para estas Parcelas/Dias - ENTER')
         =INKEY(0)
         LOOP
       ENDIF
     ENDIF    
  
  
  @ 13,42 SAY '                       'GET MDIAS_3DE PICT '999'
     READ
     IF MDIAS_3DE > 0
       IF !SEEK(MTABFIN + STR(MPARCELAS,1) + STR(MDIAS_3DE,3))
         ? MENSAG('Sem Tabela de Acrescimo Financeiro para estas Parcelas/Dias - ENTER')
         =INKEY(0)
         LOOP
       ENDIF
     ENDIF    

  @ 14,42 SAY '                       'GET MDIAS_4DE PICT '999'
     READ
     IF MDIAS_3DE > 0
       IF !SEEK(MTABFIN + STR(MPARCELAS,1) + STR(MDIAS_4DE,3))
         ? MENSAG('Sem Tabela de Acrescimo Financeiro para estas Parcelas/Dias - ENTER')
         =INKEY(0)
         LOOP
       ENDIF
     ENDIF    
  
     MENS = DADCORR
     DO PEGRESP WITH [SN]
       IF MC = 'S'
         EXIT
       ENDIF
ENDDO
*

? MENSAG('Imprimindo Tabela dos C¢digos de Pre‡os... Aguarde')
*
pagctr = 0
tamrel = 2
*
DO DEFSAIDA

SELE TABREF
SEEK MTABFIN
  
do while !EOF() .AND. TABREF = MTABFIN

  ? CABEC('TAB_PREC','TABELA DE PRECOS '+MTABFIN+' - 1 PARCELA',135)

  @ 3,01 say 'REF'
  @ 3,07 SAY 'EMB'
  @ 3,12 say 'TIPO '
  @ 3,24 SAY '%IPI'
  @ 3,33 say 'A VISTA'
  @ 3,42 SAY 'D DUP'
  
  IF MDIAS_1DE > 0
    @ 3,51 SAY STR(MDIAS_1DE,3)+' DIAS'
    @ 3,61 SAY 'D DUP'
  ENDIF
      
  IF MDIAS_2DE > 0
    @ 3,70 SAY STR(MDIAS_2DE,3)+' DIAS'
    @ 3,80 SAY 'D DUP'
  ENDIF
  
  IF MDIAS_3DE > 0
    @ 3,89 SAY STR(MDIAS_3DE,3)+' DIAS'
    @ 3,99 SAY 'D DUP'
  ENDIF
      
  IF MDIAS_4DE > 0
    @ 3,108 SAY STR(MDIAS_4DE,3)+' DIAS'
    @ 3,118 SAY 'D DUP'
  ENDIF
  
  @ 4,1  SAY REPLICATE('-',130)
*
  stor 5 to linha
  
  do while .not. EOF() .and. linha < 64 .and. tabref = mTABFIN
    @ linha,01 say TABREF.REF
    @ LINHA,07 SAY PROD.EMBALAGEM
    @ linha,12 say PROD.TIPO
    @ linha,24 SAY PROD.IPI PICT '99.9'
    @ linha,29 say TABREF.PRECO PICT '9999,999.99'
      MPRECO = TABREF.PRECO
    
    SELE TABFIN2
    
      IF SEEK(MTABFIN + STR(MPARCELAS,1) + STR(MDIAS_0DE,3)) .AND. DESC_DUP > 0
        @ linha,42 SAY DESC_DUP PICT '99.99'
      ENDIF
    
    IF MDIAS_1DE > 0
      IF SEEK(MTABFIN + STR(MPARCELAS,1) + STR(MDIAS_1DE,3))
        @ linha,48 SAY (MPRECO + ((MPRECO * PERFIN) / 100)) PICT '9999,999.99' 
        IF DESC_DUP > 0
          @ linha,61 SAY DESC_DUP PICT '99.99'
        ENDIF  
      ENDIF
        
      IF MDIAS_2DE > 0
        IF SEEK(MTABFIN + STR(MPARCELAS,1) + STR(MDIAS_2DE,3))
          @ linha,67 SAY (MPRECO + ((MPRECO * PERFIN) / 100)) PICT '9999,999.99' 
          IF DESC_DUP > 0
            @ linha,80 SAY DESC_DUP PICT '99.99'
          ENDIF  
        ENDIF  
      ENDIF
  
      IF MDIAS_3DE > 0
        IF SEEK(MTABFIN + STR(MPARCELAS,1) + STR(MDIAS_3DE,3))
          @ linha,86 SAY (MPRECO + ((MPRECO * PERFIN) / 100)) PICT '9999,999.99' 
          IF DESC_DUP > 0
            @ linha,99 SAY DESC_DUP PICT '99.99'
          ENDIF  
        ENDIF  
      ENDIF
    
      IF MDIAS_4DE > 0
        IF SEEK(MTABFIN + STR(MPARCELAS,1) + STR(MDIAS_4DE,3))
          @ linha,105 SAY (MPRECO + ((MPRECO * PERFIN) / 100)) PICT '9999,999.99' 
          IF DESC_DUP > 0
            @ linha,118 SAY DESC_DUP PICT '99.99'
          ENDIF  
        ENDIF  
      ENDIF
    
      SELE TABREF    
      stor linha + 1 to linha
      skip
      IF EOF()
        @ LINHA,1 SAY REPLICATE('-',130)
        @ LINHA+1,10 SAY 'F I M     D E S T E     R E L A T O R I O'
      ENDIF
    ENDIF  
  enddo
enddo

DO IMPSAIDA

IF FILE("CADWORK.DBF")
  ERASE CADWORK.DBF
ENDIF
return





