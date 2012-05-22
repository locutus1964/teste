* AV06101  -  ALTERACAO DE CABECALHO DE PEDIDOS - 22/11/86
* SISTEMA DE ADMINISTRACAO DE VENDAS
* ADAO ELI PEREIRA - 24-04-90   -    fox 5/10/92 - 18/06/93
***
SELE 0
  USE TRANS INDEX TRANS
SELE 0
  USE PEDIC INDEX PEDIC
SELE 0  
  USE PEDID INDE PEDIDM,PEDID,PEDIDNF
SELE 0
  USE CLIEN ORDER P_CGC
SELE 0
  USE VEND INDE VEND
SELE 0  
  USE TPNF INDE TPNF
SELE 0  
  USE CADMOEDA INDE CADMOEDA
  
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
@ 05,44 say 'Transpt'
@ 07,03 say 'Descto Cliente'
@ 07,32 say 'Tipo NF'
@ 07,46 say 'C Pag'
@ 07,69 say 'Dias'
@ 08,46 say 'Perc'
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
SELE PEDIC 
@ 03,52 get mped pict '999999'
  read
  if lastkey() = 27
    if mdel = 1
      MENS = 'Houve exclus”es. Compacta arquivo de Pedidos ?'
      DO PEGRESP WITH [SN]
        IF MC = 'S'
          ? MENSAG('Limpando as exclus”es do arquivo... Aguarde')
          SELE PEDIC
            pack
          SELE PEDID
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
    LOOP
  ELSE
    MCGC = CGC
    SELE CLIEN 
      IF SEEK(MCGC)
        MGUE = GUE
        PGUE = GUE
        MTRA_OPC = TRA_OPC
        @ 03,16 SAY MGUE COLOR &CORVER
      ELSE
        ? MENSAG('Cliente n„o cadastrado - Anotar para corre‡„o - ENTER')
        =INKEY(0)
      ENDIF  
      
  SELE PEDIC 
  IF SEEK(MPED)
    @ 03,39 SAY VEN  COLOR &CORVER
      MCVR = VEN
    @ 03,69 SAY DEM pict '99/99/99' COLOR &CORVER
      MDEM = DEM
    @ 05,11 SAY PINT COLOR &CORVER
      MPINT = PINT
    @ 05,28 SAY PCL COLOR &CORVER
      MPCL  = PCL
    @ 05,52 say tra COLOR &CORVER
      MTRA  = TRA
      SELE TRANS 
      IF SEEK(MTRA)
        @ 5,56 SAY GUETRA COLOR &CORVER
      ENDIF
      IF MTRA_OPC <> SPACE(03)
        @ 6,52 SAY MTRA_OPC COLOR &CORVER
        IF SEEK(MTRA_OPC)
          @ 6,56 SAY GUETRA COLOR &CORVER
        ENDIF
      ENDIF    
      SELE PEDIC   
    @ 07,18 say descto1 pict '99.99' COLOR &CORVER
      MDESCTO1 = DESCTO1
    @ 07,24 say descto2 pict '99.99' COLOR &CORVER
      MDESCTO2 = DESCTO2
    @ 07,40 say tip pict '999' COLOR &CORVER
      MTIP  = TIP
    @ 07,52 say cp1 pict '999' COLOR &CORVER
      MCP1  = CP1
    @ 08,52 say pe1 pict '999' COLOR &CORVER
      MPE1  = PE1       
    @ 07,56 say cp2 pict '999' COLOR &CORVER
      MCP2  = CP2
    @ 08,56 say pe2 pict '999' COLOR &CORVER
      MPE2  = PE2
    @ 07,60 say cp3 pict '999' COLOR &CORVER
      MCP3  = CP3
    @ 08,60 say pe3 pict '999' COLOR &CORVER
      MPE3  = PE3
    @ 07,64 say cp4 pict '999' COLOR &CORVER
      MCP4  = CP4
    @ 08,64 say pe4 pict '999' COLOR &CORVER
      MPE4  = PE4
    @ 07,74 say cpd pict '!!!' COLOR &CORVER
      MCPD  = CPD
    @ 09,10 say desconto pict '99999999.99' COLOR &CORVER
      MDESCONTO = DESCONTO
    @ 09,30 say fre pict '99999999.99' COLOR &CORVER
      MFRE = FRE
    @ 09,50 say emb pict '99999999.99' COLOR &CORVER
      MEMB = EMB
    @ 09,70 say moeda pict '999' COLOR &CORVER
      MMOEDA = MOEDA
      
    @ 12,14 TO 20,70 DOUBLE
    @ 13,16 SAY 'Observa‡”es a imprimir no Pedido:'
    @ 14,16 say O1
      MO1 = O1
    @ 15,16 say O2
      MO2 = O2
    @ 16,16 say O3
      MO3 = O3
    @ 17,16 say O4
      MO4 = O4
    @ 18,16 say O5
      MO5 = O5
    @ 19,16 say O6
      MO6 = O6
  ENDIF
  
  MENS = 'Altera Cabe‡alho [A], exclui Pedido [E] ou sai [ESC] ? '
  DO PEGRESP WITH [AE]

    IF MC = 'E'
      MENS = 'Tem certeza que pode excluir o Pedido ?'
      DO PEGRESP WITH [SN]
        IF MC = 'N'
          LOOP
        ENDIF  
      MDEL = 1  
      DELE
      SELE PEDID
      IF SEEK(STR(MPED,6))
        DO WHILE PED = MPED
          DELE
          SKIP
        ENDDO
      ENDIF  
    ENDIF    
  endif  
  
SAVE SCRE TO TELA
*
SELE CLIEN 
  ? MENSAG(ECODIGO)
  @ 03,16 GET MGUE PICT '@!'
    READ

    IF LASTKEY() = 27
      LOOP
    ENDIF   
        
    IF MGUE = SPACE(10)
      ? MENSAG(CALPHA)
      =INKEY(0)
      LOOP
    ENDIF      
    
SELE VEND 
? MENSAG('Se C¢digo do Vendedor n„o for o cadastrado, substitua-o')
do while .t.
  @ 03,39 get MCVR 
    read

    if lastkey() = 27
      clos data
      return
    endif  
        
    if MCVR <> space(3)
      if !seek(MCVR)
        ? MENSAG(CORRIJA)
        LOOP
      endif
    endif
    exit
enddo
*
? MENSAG('Informe a data da emiss„o do Pedido no formato (DD/MM/AA)')
DO WHILE .T.
  @ 03,69 get mdem pict '99/99/99'
  READ
    DO FDATA WITH MDEM
      IF TDATA = ' '
        EXIT
      ENDIF
ENDDO
*
? MENSAG('Informe os n£meros dos Pedidos de Controle Interno e do Cliente')
  @ 05,11 get mpint
  @ 05,28 get mpcl
    read
*
SELE TRANS 
  @ 05,52 say mtra

  IF MTRA_OPC <> SPACE(03)
    if SEEK(MTRA_OPC)
      @ 06,52 SAY MTRA_OPC COLOR &CORVER
      @ 06,56 SAY GUETRA COLOR &CORVER
    ELSE
      @ 06,56 SAY 'NAO CADASTRADA'
    ENDIF    
  ENDIF

  ? MENSAG('Se C¢digo da Transportadora n„o for o do cadastro, substitua-o')
  do while .t.
    @ 05,52 get mtra pict '999'
      read

      if lastkey() = 27
        clos data
        cancel
      endif        
      
      if mtra <> space(3)
        seek mtra
        if (eof() .or. bof())
          ? MENSAG(CORRIJA)
          loop
        endif  
      endif
      exit
  enddo
*
? MENSAG('Informe percentual de Desconto Geral para este Pedido, se concedido')
@ 07,18 get mdescto1 pict '99.99'
@ 07,24 get mdescto2 pict '99.99'
  read
*
SELE TPNF 
? MENSAG('Informe o Tipo de Nota Fiscal a ser emitida - [ESC] encerra')
do while .t.
  @ 07,40 get mtip pict '999'
    read

    if lastkey() = 27
      clos data
      return
    endif  
*
    if mtip = space(3)
      ? MENSAG(CALPHA)
      loop
    endif
*
    if !seek(mtip)
      ? MENSAG(CORRIJA)
    else
      exit
    endif
enddo
*
? MENSAG('Informe DIAS e PERCENTUAIS para vencimento das parcelas')
@ 07,52 get mcp1 pict '999'
@ 08,52 get mpe1 pict '999'
@ 07,56 get mcp2 pict '999'
@ 08,56 get mpe2 pict '999'
@ 07,60 get mcp3 pict '999'
@ 08,60 get mpe3 pict '999'
@ 07,64 get mcp4 pict '999'
@ 08,64 get mpe4 pict '999'
  read
*
SAVE SCREEN TO TELA
? janela(09,50,19,75)
? borda(09,50,19,75)
@ 10,52 SAY 'Modos aceitos:'
@ 11,52 SAY 'VIS - Pagto a Vista'
@ 12,52 SAY 'APR - Contra Apresent.'
@ 13,52 SAY 'DDD - Dias da Data Liq'
@ 14,52 SAY 'DFM - Dias fora o Mes'
@ 15,52 SAY 'DFQ - Dias fora Quinz.'
@ 16,52 SAY 'SDB - Sem D‚bito'
@ 17,52 SAY 'PER - Permuta'
@ 18,52 SAY 'ANT - Pagto Antecipado'

do while .t.
@ 07,74 get mcpd pict '!!!'
  read
    if mcpd = 'VIS' .or. mcpd = 'APR' .or. mcpd = 'DDD' .or. mcpd = 'DFM' ;
      .or. mcpd = 'DFQ' .OR. MCPD = 'SDB' .OR. MCPD = 'ANT'
      exit
    endif
enddo
*
REST SCREEN FROM TELA
@ 07,74 SAY MCPD
*
? MENSAG('Informe o Valor do Desconto Fixo, se concedido')
@ 09,10 get mdesconto pict '99999999.99'
  read
  
? MENSAG('Informe o Valor do Fr‚te, se PAGO pelo Cliente')
@ 09,30 get mfre pict '99999999.99'
  read
  
? MENSAG('Informe o Valor da Embalagem se PAGA pelo Cliente')
@ 09,50 get memb pict '99999999.99'
  read
*
SELE CADMOEDA
do while .t.
  @ 09,70 get mmoeda pict '999'
    read

    if lastkey() = 27
      clos data
      return
    endif  
        
    if mmoeda = 0
      ? MENSAG(CNUMER)
      loop
    endif
*
    if !seek(str(mmoeda,3))
      ? MENSAG('Moeda n„o cadastrada. Corrija ou entre (999) para encerrar')
      loop
    endif
    EXIT       
enddo
*
  MENS = DADCORR
  DO PEGRESP WITH [SN]
    if mc = 'N'
      loop
    endif
*
SELE CLIEN 
  SEEK MCGC
  repl ulc with dtoc(date()),alt with '*'
*
  MENS = 'Pedido com observa‡”es especiais ?'
  DO PEGRESP WITH [SN]
    IF MC = 'N'
      STOR SPACE(53) TO MO1, MO2, MO3, MO4, MO5, MO6
    ELSE     
      @ 12,14 CLEAR TO 20,70
      @ 12,14 TO 20,70 DOUBLE
      @ 13,16 SAY 'Observa‡”es a imprimir no Pedido:'
      DO WHILE .T.
        @ 14,16 GET MO1
        @ 15,16 GET MO2
        @ 16,16 GET MO3
        @ 17,16 GET MO4
        @ 18,16 GET MO5
        @ 19,16 GET MO6
          READ
          MENS = 'Observa‡”es est„o corretas ?'
          DO PEGRESP WITH [SN]
            IF MC = 'S'
              EXIT
            ENDIF
      ENDDO
    ENDIF  

  MENS = DADCORR
  DO PEGRESP WITH [SN]
    IF MC = 'N'
      LOOP
    ELSE
      SELE PEDIC 
      SEEK MPED
      IF PGUE <> MGUE
        DELE
        APPE BLAN  
      ENDIF  
  
      repl cgc with mcgc,ven with MCVR,dem with mdem,pcl with mpcl
      repl ped with mped,pint with mpint,moeda with mmoeda
      repl tra with mtra,descto1 with mdescto1,descto2 with mdescto2,tip with mtip
      repl fre with mfre,emb with memb,aut with maut,cp1 with mcp1
      repl cp2 with mcp2,cp3 with mcp3,cp4 with mcp4,cpd with mcpd
      repl pe1 with mpe1,pe2 with mpe2,pe3 with mpe3,pe4 with mpe4
      repl desconto with mdesconto
      REPL O1 WITH MO1,O2 WITH MO2,O3 WITH MO3
      REPL O4 WITH MO4,O5 WITH MO5,O6 WITH MO6
    ENDIF  
* --------------------------------------------------------------------------
enddo
clos data
return
