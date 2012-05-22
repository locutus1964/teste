* AV06250 - CONSULTA SITUACAO DE PEDIdo POR CLIENTE
* SISTEMA ADMINISTRACAO DE VENDAS
* ADAO ELI PEREIRA - 24-04-90 - 4/12/91
clos data
sele 0
use CLIEN ORDER X_GUE
sele 0
use PEDID ORDER P_PEDID
sele 0
use PEDIC ORDER F_CGC
MCGC = space(20)
MGUE = space(15)
=CABTELA(0,0,24,79)
MENS = 'Consulta Pedido pelo Nome de Guerra do Cliente'
=mensagr(MENS)
@01,POS say MENS
@03,02 say 'N.Guerra'
@05,02 say 'It  Referˆncia    Quantidade       Saldo  D Ite Bonus Tabela Entrega  Num NF'
MGUERRA = space(15)
do WHILE .T.
  do while .t.
    sele clien
    **    MENS = 'Quer selecionar Nome de Guerra pelas iniciais ?'
    **    do PEGRESP WITH [SN]
    **    IF MC = 'S'
    =MENSAG('Entre com o Nome de Guerra ou letras iniciais e pressione ENTER')
    @03,11 get MGUERRA                   pict '@!'
    read
    **      MGUERRA = RTRIM(MGUERRA)
    **      IF seek(MGUERRA)
    **        =MOSTRA(12,01,21,78)
    **        BROWSE FIELDS GUE:R:H='N. Guerra',CGC:R:H='CGC/Loja', ;
    **                  CID:R:H='Cidade',END:R:H='Endere‡o' IN WINDOW TELA
    **        MGUERRA = GUE
    **        do ESCONDE
    **      else
    **        =MENSAG('Nome de Guerra n„o cadastrado - ENTER')
    **        =INKEY(0)
    **        LOOP
    **      endif
    **    endif
    **    @03,11 get MGUERRA
    **      read
    if lastkey() = 27
      close data
      clear
      return
    endif
    if empty(MGUERRA)
      =mensag(CALPHA)
      loop
    endif
    if !seek(mguerra)
      if !seek(alltrim(MGUERRA))
        =mensag(CORRIJA)
        =INKEY(0)
        loop
      else
        ON KEY LABEL ENTER DEAC WINDOW TELA
        ON KEY LABEL ESCAPE DEAC WINDOW TELA
        ? MOSTRA(12,01,21,78)
        BROWSE FIELDS GUE:R:H='N. Guerra',CGC:R:H='CGC/Loja', ;
                      CID:R:H='Cidade',END:R:H='Endere‡o' IN WINDOW TELA
        ON KEY LABEL ENTER
        ON KEY LABEL ESCAPE
        if lastkey() = 27
          close data
          clear
          return
        endif
      endif
    endif
    mcgc    = cgc
    mraz    = raz
    mguerra = gue
    @03,11 say MGUERRA  color &corver
    @03,27 say mraz     color &corver
    sele pedic
    IF ! SEEK( MCGC )
      =MENSAG('Nao ha pedidos para este Cliente' )
      =INKEY(0)
      LOOP
    ENDIF  
    SET ORDER TO P_PED
    do while !eof()
      if mcgc <> cgc
        skip
        if eof()
          =mensag('Final dos Pedidos - ENTER')
          =INKEY(0)
          loop
        endif
        loop
      else
        mped = ped
        if situacao = 1
          msituacao = '(1-Pronto)'
        else
          if situacao = 2
            msituacao = '(2-Expedi‡„o)'
          else
            if situacao = 3
               msituacao = '(3-Cancelado)'
            else
               msituacao = '(0-Pendente)'
            endif
         endif
      endif
      tcgc = mcgc
      sele pedid
*            @03,67 say 'Ped '+str(mped,6)
      @04,02 say padc(' Pedido N§ '+alltrim(str(mped,6))+'     '+msituacao+' ',76,'-')
      seek str(mped,6)
      if (eof() .or. bof())
        =mensag('Pedido sem ¡tens cadastrados - confira - ENTER')
      else
        lin = 6
      endif
      do while ped = mped .and. .not. eof()
        @lin,02 say str(ite,2)
        @LIN,06 say ref
        @lin,18 say qtd                   pict '9,999,999.99'
        @lin,30 say sal                   pict '9,999,999.99'
        @lin,44 say str(dit,5,2)
        @lin,50 say str(bon,5,2)
        @lin,56 say tabref
        @lin,62 say dent
        @lin,72 say str(nnf,6)
        lin = lin + 1
        if lin > 21
          lin = 6
          =mensag('Pressione qualquer tecla para continuar')
          =INKEY(0)
          @06,01 clear TO 21,78
        endif
        skip
        if ped <> mped
          MC = ' '
          MENS = 'Continua a consulta dos Pedidos ?'
          do PEGRESP WITH [SN]
          IF MC = 'N' .or. empty(mc)
            clear
            CLOS DATA
            return
          endif
          @06,01 clear TO 21,78
        endif
      enddo
      sele pedic
      skip
      if eof()
        =mensag('Final dos Pedidos - [ESC] para encerrar')
        =INKEY(0)
        endif
      endif
    enddo
  enddo
enddo
clos data
return

