* AV08310 - CALCULA A POSICAO DE CLIENTES POR BANCO
* SISTEMA ADMINISTRACAO DE VENDAS
* ADAO ELI PEREIRA - 02/03/87
*
do TESTIMP2
if CP # 'N'
   @15,52 clear to 21,78
   @15,52 to 21,78 double
   @17,55 say "Data Inicial :"
   @18,55 say "Data Final :"
   stor space(08) to DTINI, DTFIM
   @17,68 get DTINI pict "99/99/99"
   @18,68 get DTFIM pict "99/99/99"
   read
   if lastkey() # 27
      MB      = ' '
      MNBANCO = 0
      MENS = 'Seleciona Banco ?'
      do PEGRESP WITH [SN]
      MB = MC
      if MB = 'S'
         use BANCOS INDEX BANCOS
         @19,55 say 'Informe o Banco 'get MNBANCO pict '999'
         read
      else
         if empty(mc)
            close data
            return
         endif
      endif
      **--------- PREPARACAO do ARQUIVO COM DUPLICATAS E VENCIMENTOS ----------
      =MENSAG('Classificando duplicatas para emissao do relatorio...Aguarde')
      IF MB = 'N'
        lFiltro = '.T.'
      ELSE
        lFiltro = 'NBANCO = MNBANCO'
      ENDIF  
           
      use DUPL
      COPY TO CADWORK;
        FOR CTOD(VENC) >= CTOD(DTINI) .and. CTOD(VENC) <= CTOD(DTFIM) .and.;
            EMPTY(PAG) .and. EMPTY(DTCANCEL) .AND. &lFiltro

      USE CADWORK
      INDEX ON STR(NBANCO,3) + '/' + str(AGENCIA,4) +;
               DTOS(CTOD(VENC)) + CGC TO POSFIN
      CLOSE DATABASES
      MCGC     = space(16)
      stor space(08) to KEY2, MVENC
      stor space(30) to MRAZ
      stor 0 to MNBANCO, MAGENCIA, VALDEV, VALGER, VALDIA, PAGCTR
      stor 0 to VALBCO, TDESCTO, TVAL, TESTE, TOT_URV, TOT_URVB
      stor 2 to TAMREL
      sele 0
      use BANCOS INDEX BANCOS
      sele 0
      use CLIEN ORDER P_CGC
      sele 0
      use CADWORK INDEX POSFIN
      sele 0
      use CADMOEDA INDEX CADMOEDA
      =MENSAG('Imprimindo Posicao de Clientes por Banco... Aguarde')
      do DEFSAIDA
      sele CADWORK
      *-----------------------------------------------------------------------------
      TITULO = 'POSICAO DE CLIENTES POR BANCO - DE '+DTINI+' ATE '+DTFIM
      do while !eof()
         MDIA = DAY(CTOD(VENC))
         =CABEC(PROGRAM(),TITULO,132)
         @03,001 say 'VENCIMEN RAZAO SOCIAL do CLIENTE          DUPLICATA   EMISSAO       VAL EM CR$        VAL EM URV   VALOR ACUMULADO   ACUMUL do DIA'
         lin = 5
         do while (!eof()) .and. (lin < 60)
            if lin = 5 .or. str(NBANCO,3) + str(AGENCIA,4) <> str(MNBANCO,3) + str(MAGENCIA,4)
               if lin <> 5
                  lin = lin + 1
               endif
               @lin,1 say PADL(NBANCO,3,'0') +'/'+ PADL(AGENCIA,4,'0')
               MNBANCO  = NBANCO
               MAGENCIA = AGENCIA
               sele BANCOS
               if SEEK(str(MNBANCO,3) + str(MAGENCIA,4))
                  @lin,10 say DBANCO
               else
                  @lin,10 say 'BANCO NAO CADASTRADO'
               endif
               lin = lin + 2
            endif
            sele CADWORK
            if !EMPTY(DTCANCEL) .or. PAG <> space(8)
               skip
               loop
            endif
            @lin,001 say VENC
            MCGC = CGC
            sele CLIEN
            if SEEK(MCGC,'clien')
*               @lin,10 say SUBS(clien->RAZ,1,33)
                @lin,10 say clien->gue
            else
               @lin,10 say 'NAO CADASTRADO'
            endif
            sele CADWORK
            @lin,44 say NDUP
            if left(DTDESCTO,2) <> space(2) .and. left(BXDESCTO,2) = space(2)
               STOR (TDESCTO + VDP) TO TDESCTO
               @lin,53 say 'D'
               @lin,53 say 'D'
            endif
            @lin,55 say EMIS
            @lin,65 say VDP pict '9,999,999,999.99'
            if MOEDA = 2
               @lin,88 say QTD_MOEDA pict '999,999.99'
               TOT_URV = (TOT_URV + QTD_MOEDA)
               TOT_URVB = (TOT_URVB + QTD_MOEDA)
            endif
            STOR (TVAL + VDP - (VALDEV)) TO TVAL
            STOR (VALDIA + VDP - (VALDEV)) TO VALDIA
            STOR (VALBCO + VDP - (VALDEV)) TO VALBCO
            STOR (VALGER + VDP - (VALDEV)) TO VALGER
            if VALDEV > 0
               lin = lin + 1
               @lin,40 say 'VALOR DEVOLUCAO--------'
               @lin,65 say VALDEV pict '9,999,999,999.99'
               if MOEDA = 2
                  @lin,88 say (VALDEV / VAL_MOEDA) pict '999,999.99'
                  TOT_URV = (TOT_URV - (VALDEV / VAL_MOEDA))
                  TOT_URVB = (TOT_URVB - (VALDEV / VAL_MOEDA))
               endif
            endif
            if DESC > 0
               lin = lin + 1
               @lin,40 say 'VALOR DESCONTO---------'
               @lin,65 say DESC pict '9,999,999,999.99'
               if MOEDA = 2
                  @lin,88 say (DESC / VAL_MOEDA) pict '999,999.99'
                  TOT_URV = (TOT_URV - (DESC / VAL_MOEDA))
                  TOT_URVB = (TOT_URVB - (DESC / VAL_MOEDA))
               endif
            endif
            @lin,99 say TVAL pict '9999,999,999.99'   &&-- 99,999,999,999.99
            skip
            if substr(VENC,1,2)+substr(VENC,4,2)+substr(VENC,7,2) <> KEY2 .or. eof()
               @lin,115 say VALDIA pict '99,999,999,999.99'
               STOR 0.00 TO VALDIA
               TESTE = 1
            else
               TESTE = 0
            endif
            if str(NBANCO,3)+str(AGENCIA,4) <> str(MNBANCO,3)+str(MAGENCIA,4) .or. eof()
               lin = lin + 2
               @lin,50 say 'TOTAL do BANCO ----------->'
               @lin,86 say TOT_URVB pict '9,999,999.99'
               @lin,115 say VALBCO pict '99,999,999,999.99'
               TOT_URVB = 0.00
               if eof()
                  lin = lin + 1
                  @lin,55 say 'TOTAL GERAL --------->'
                  @lin,86 say TOT_URV pict '9,999,999.99'
                  @lin,115 say VALGER pict '99,999,999,999.99'
                  lin = lin + 1
                  @lin,80 say 'DUPLICATAS DESCONTADAS----->'
                  @lin,115 say TDESCTO pict '99,999,999,999.99'
                  lin = lin + 1
                  @lin,85 say 'TOTAL LIQUIDO--------->'
                  @lin,115 say (VALGER - TDESCTO) pict '99,999,999,999.99'
               endif
               stor 0 to VALDEV, VALDIA, VALBCO
            endif
            lin = lin + 1
            loop
         enddo
      enddo
      *-----------------------------------------------------------------------------
      do IMPSAIDA
   endif
endif
close data
return

