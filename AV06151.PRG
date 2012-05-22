* AV06151  -  ALTERACAO DE ITENS DE PEDIDOS - 22/11/86
* SISTEMA DE ADMINISTRACAO DE VENDAS
* ADAO ELI PEREIRA - 24-04-90   -    fox 5/10/92 - 19/06/93
*
  mped = 0
  mdel = 0
  maut = space(01)
*
? CABTELA(0,0,24,79)
MENS = 'Altera‡„o de Cabe‡alho de Pedidos'
? MENSAGR(MENS)
@ 01,POS say MENS
@ 03,03 say 'Nome Guerra'
@ 03,34 say 'Vend'
@ 03,44 say 'Ped Sis'
@ 03,60 say 'Emiss„o'
@ 05,03 say 'Ped Int'
@ 05,20 say 'Ped Cli'
@ 05,44 say 'Dat p/Entr'
@ 05,66 say 'Transpt'
@ 07,03 say '% Desc'
@ 07,18 say 'Tipo NF'
@ 07,31 say 'Setor'
@ 07,46 say 'C Pag'
@ 07,69 say 'Dias'
@ 08,40 say 'Perc'
@ 09,03 say 'V.Desc'
@ 09,23 say 'Frete'
@ 09,43 say 'Embal'
@ 09,63 say 'Moeda'
@ 10,01 say replicate('-',78)
@ 11,02 say 'It'
@ 11,05 say 'Referˆncia'
@ 11,17 say 'Edi‡„o'
@ 11,24 say 'Clas.Fisc.'
@ 11,35 say 'UM'
@ 11,38 say '%IPI'
@ 11,43 say 'Quantidade'
@ 11,55 say 'desct'
@ 11,61 say 'bonus'
@ 11,67 say 'Pr.Unitar.'
*
DO WHILE .T.
*
? MENSAG(ECODIGO)
*
USE PEDIC ORDER P_PED
@ 03,52 get mped pict '999999'
  read
  if lastkey() = 27
    if mdel = 1
      MC = ' '
      MENS = 'Houve exclus”es. Compacta arquivo de Pedidos ?'
      DO PEGRESP WITH [SN]
      IF MC = 'S'
        ? MENSAG('Limpando as exclus”es do arquivo... Aguarde')
        USE PEDIC ORDER P_PED
        pack
        USE PEDID ORDER X_PEDREF, P_PEDID
        pack
      ENDIF
    endif
    clos data
    clear
    return
  endif
  
  if  mped = 0
    ? MENSAG(CNUMER)
    loop
  endif

  IF !SEEK(MPED)
    ? MENSAG(CORRIJA)
    =INKEY(0)
  ELSE
    MCGC = CGC
    USE CLIEN ORDER P_CGC
      IF SEEK(MCGC)
        @ 03,16 SAY GUE  COLOR &CORVER
        MGUE = GUE
        PGUE = GUE
        @ 03,16 SAY MGUE COLOR &CORVER
      ENDIF  
      
    USE PEDIC ORDER P_PED
    SEEK MPED
    @ 03,39 SAY VEN pict '999' COLOR &CORVER
    @ 03,69 SAY DEM pict '99/99/99' COLOR &CORVER
    @ 05,11 SAY PINT                COLOR &CORVER  
    @ 05,28 SAY PCL                 COLOR &CORVER
    @ 05,55 say den pict '99/99/99' COLOR &CORVER
    @ 05,74 say tra                 COLOR &CORVER
    @ 07,10 say dge pict '999.99'   COLOR &CORVER
    @ 07,26 say tip pict '999'      COLOR &CORVER
    @ 07,37 say setor               COLOR &CORVER 
    @ 07,52 say cp1 pict '999'      COLOR &CORVER
    @ 08,52 say pe1 pict '999'      COLOR &CORVER
    @ 07,56 say cp2 pict '999'      COLOR &CORVER
    @ 08,56 say pe2 pict '999'      COLOR &CORVER
    @ 07,60 say cp3 pict '999'      COLOR &CORVER
    @ 08,60 say pe3 pict '999'      COLOR &CORVER
    @ 07,64 say cp4 pict '999'      COLOR &CORVER
    @ 08,64 say pe4 pict '999'      COLOR &CORVER
    @ 07,74 say cpd pict '!!!'      COLOR &CORVER
    @ 09,10 say desconto pict '99999999.99'  COLOR &CORVER
    @ 09,30 say fre pict '99999999.99'       COLOR &CORVER
    @ 09,50 say emb pict '99999999.99'       COLOR &CORVER
    @ 09,70 say moeda pict '999'             COLOR &CORVER
  ENDIF    
* --------------------------------------------------------------------------
*       ALTERA ITENS DOS PEDIDOS
* --------------------------------------------------------------------------
? MENSAG(ECODIGO)
  @ 12,01 CLEAR TO 21,78
  lin   = 12

do while .T.
*
  mite = 0
  mref = space(11)
  medi = space(06)
  mccl = space(10)
  muni = space(02)
  mipi = 0.0
  mqtd = 0
  qtdped = 0
  mdit = 0.00
  mbon = 0.00
  mpre = 0.00
  mdescricao = space(40)
  mdescraux1 = space(40)
  mdescraux2 = space(40)
  mordem     = space(07)
*
  USE PEDID ORDER X_PEDREF
    ? MENSAG('Informe o ¡tem do Pedido a ser alterado/excluido')
  @ lin,02 get mite pict '99'
    read

      incluite = space(01)    
      if seek(str(mped,6) + str(mite,2))
        MENS = '[E] Exclui item do Pedido, [A] Altera item e [ESC] sai '
        DO PEGRESP WITH [AE]
          
        IF MC = 'E'
          DELE
          LOOP
        ENDIF

        mref = ref            
        medi = edi
        mccl = ccl
        muni = uni
        mipi = ipi
        mqtd = qtd
        qtdped = qtd
        mdit = dit
        mbon = bon
        mpre = pre
        mdescricao = descricao
        mordem     = ordem
        mdescraux1 = descraux1
        mdescraux2 = descraux2
      else
        incluite = '*'
        MENS = 'N„o cadastrado no Pedido. Inclui ?'
        DO PEGRESP WITH [SN]
          IF MC = 'N'
            LOOP
          ENDIF
      endif      
    
    
  IF LASTKEY() = 27
    EXIT
  ENDIF

  USE PROD INDE PROD        
  ? MENSAG(ECODIGO)
  @ lin,05 get mref
    read
    
      IF LASTKEY() = 27
        EXIT
      ENDIF  
          
      if EMPTY(mref)
        ? MENSAG(CALPHA)
        loop
      endif
 
        if !seek(mref)
          MENS = 'N„o cadastrado no Estoque. Aceita para o Pedido ?'
          DO PEGRESP WITH [SN]
          IF MC = 'N'
            LOOP
          ENDIF
        else
          if incluite = '*'
            mdescricao = des
          endif  
          mccl = ccl
          muni = uni
          mipi = ipi
        endif    
      
        ? mensag('Informe a Edi‡„o do Produto, quando definida')     
        @ LIN,17 GET MEDI
          
        ? mensag('Informe a Classifica‡„o Fiscal do IPI')
        @ LIN,24 GET MCCL
          
        ? mensag('Informe a Unidade de Medida')  
        @ LIN,35 GET MUNI PICT '@!'
          
        ? mensag('Informe o Percentual do IPI se destacado na N.Fiscal')  
        @ LIN,38 GET MIPI PICT '99.9'
          
        ? mensag('Informe a quantidade a faturar')  
        @ LIN,43 GET MQTD PICT '99999999.99'
          
          dqtd = (mqtd - qtdped)
        ? MENSAG('Informe o percentual de desconto do ¡tem, se concedido')
        @ LIN,55 GET MDIT PICT '99.99'
          
        ? MENSAG('Informe o percentual de bonifica‡„o do ¡tem, se concedido')
        @ LIN,61 GET MBON PICT '99.99'
          
        ? mensag('Informe o Pre‡o Unit rio Bruto')  
        @ LIN,67 GET MPRE PICT '99999999.99'
          
          lin = lin + 1
        ? mensag('Informe a Descri‡„o do Produto/Material a faturar')  
        @ lin,17 get mdescricao
        
        ? mensag('Informe o n£mero da O.S. quando existente')
        @ lin,60 say 'O.S. 'get mordem
          read
          
          SAVE SCRE TO TELA1
          
          IF MDESCRAUX1 <> SPACE(20)
            @ 06,08 CLEAR TO 09,51
            @ 06,08 TO 09,51 DOUBLE
            @ 07,10 say mdescraux1
            @ 08,10 say mdescraux2
            MENS = 'Altera ou limpa a Descri‡„o complementar do ¡tem ?'
            DO PEGRESP WITH [AL] 
            IF MC = 'L'
              mdescraux1 = space(40)
              mdescraux2 = space(40)
            ELSE  
              @ 07,10 get mdescraux1
              @ 08,10 get mdescraux2
                read
            ENDIF
          ENDIF  
            
          IF MDESCRAUX1 = SPACE(20)
            MENS = 'Complementa a Descri‡„o do ¡tem ?'
            DO PEGRESP WITH [SN] 
            IF MC = 'S'
              @ 06,08 CLEAR TO 09,51
              @ 06,08 TO 09,51 DOUBLE
              @ 07,10 get mdescraux1
              @ 08,10 get mdescraux2
                read
            ENDIF
          ENDIF  
            
            MENS = DADCORR
            DO PEGRESP WITH [SN]
              IF MC = 'N'
                LIN = LIN - 1
                LOOP
              ENDIF
              
*        REST SCRE FROM TELA1             
*
          IF SEEK(MREF)
            repl vam with (vam + dqtd)
            repl vaa with (vaa + dqtd)
            repl pea with (pea + dqtd)
          ENDIF  
*
          use PEDID ORDER X_PEDREF, P_PEDID
            IF !SEEK(STR(MPED,6) + STR(MITE,2))
              appe blan
            ENDIF  
            repl ped       with mped
            repl cgc       with mcgc
            repl ite       with mite
            repl ref       with mref
            repl edi       with medi
            repl ccl       with mccl
            repl uni       with muni
            repl ipi       with mipi
            repl qtd       with mqtd
            repl sal       with mqtd
            repl dit       with mdit
            repl bon       with mbon
            repl pre       with mpre
            repl descricao with mdescricao
            repl descraux1 with mdescraux1
            repl descraux2 with mdescraux2
            repl ordem     with mordem

            REST SCREEN FROM TELA1
            
            lin = LIN + 1
            if lin > 20
              @ 12,01 CLEAR TO 21,78
              lin = 12
            endif
            ? MENSAG(ECODIGO)

enddo
enddo
clos data
return
