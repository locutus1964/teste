* CONSDUP - CONSULTA SITUACAO DE DUPLICATAS POR CLIENTE
* SISTEMA ADMINISTRACAO DE VENDAS - ADAO ELI PEREIRA - 02/03/87
*
stor 0 to mate
sele 0
use regiao index regiao.IDX
sele 0
use vend index vend.IDX
sele 0
use CLIEN ORDER X_GUE
sele 0
use DUPL order xdupcgc

set filter to EMPTY(DUPL->DTCANCEL)

SELE CLIEN
SET RELATION TO CGC INTO DUPL

=CABTELA(0,0,24,79)
MENS = 'Consulta Duplicatas'
=MENSAGR(MENS)
@01,POS say MENS
@3,03 say 'N.Guerra'
=mensag(ecodigo)
MGUE = space(15)
do while .t.
   @04,01 clear to 06,78
   @08,01 clear to 21,78
   totdeb = 0
   sele CLIEN
   =MENSAG('Informe o Nome de Guerra  ou  ESC  para Terminar')
   @03,13 get mgue pict '@!'
   read
   IF lastkey() = 27
      exit
   ENDIF
   IF mgue = space(10)
      =MENSAG('Nome de Guerra n„o pode brancos')
      loop
   ENDIF
   IF seek(rtrim(mgue))
     MGUE = GUE
     @03,13 say mgue color &corver
   ELSE
     =MENSAG('Cliente n„o est  cadastrado - ENTER')
     =INKEY(0)
     =MOSTRA(12, 01, 21, 78)
     browse FIELDS GUE:R:H='N. Guerra',CGC:R:H='CGC/Loja', ;
                   CID:R:H='Cidade',END:R:H='Endere‡o' IN WINDOW TELA
     =esconde()
     mgue = gue
     loop
   ENDIF
   @03,30 say RAZ
   mraz = raz
   stor (CRE * 100) to MCRE
   MCGC = CGC
   @04,05 say 'Lim.Cred. '+str(MCRE,9)
   @04,27 say 'Debito '
   @04,46 say 'Ult.Compra '+DTOC(ULC)
   mulc = ulc
   @04,68 say 'Tipo '+TIP
   mtip = tip
   sele DUPL
   if EOF('DUPL')
      @23,01 clear to 23,78
      @23,10 say 'Nao existem duplicatas para o Cliente - ENTER'
      =INKEY(0)
      loop
   endif
   icgc=cgc
   debacum=0
   maiordeb=0
   vc=venc
   c=0
   dat = emis
   =MENSAG('Calculando maior debito...')
   *--- Percorre o arquivo de Duplicatas 
   =CalcDebAcum()
   @05,03 say 'Maior Deb '+str(maiordeb,12,2) +'  '+ DTOC(dat)
   @05,46 say 'Ult.N.Fisc ' + DTOC(dat)
   dia = emis
   seek mcgc
   DO TESTIMP2 
   IF cp = 'N'
      LOOP
   ENDIF   
 
   =MENSAG('Imprimindo...') 
   TAMREL=1
   DO DEFSAIDA   
   pagctr  = 00
   TamPag  = 55
   LargPag = 78
   SELE DUPL
   totdeb    = 0
   TOTDUPL   = 0
   totVenc   = 0
   TotReceb  = 0
   nTotJuros = 0
   do while cgc = mcgc .and. ! eof()
      =IMPRCABEC()
      do while cgc = mcgc .AND. ! eof() .AND. lin < TamPag 
        dvenc = VENC
        dpag  = PAG
        * Duplicata/Parcela
        @lin, 00 say str(ndup,6)+'/'+str(pdup,1)
        * Emissao
        @lin, 11 say emis
        * Valor da Duplicata
        @lin, 20 say vdp pict "9,999,999.99"
        * Vencimento
        @lin, 34 say venc 
        if EMPTY(dpag)
          IF date() > dvenc
            totvenc = totvenc + vdp
          ENDIF          
          totdeb = totdeb + vdp - valdev
          * Total Debito
          @lin, 71 say totdeb PICT [99,999.99]          
        else     
          * Data do Pagamento
          @lin, 44 say pag
          * Atraso
          * Verifica se a data do vencimento cai no s bado ou domingo, com
          * pagamento na segunda-feira seguinte dispensando a atraso
          IF dpag > dvenc 
            diasem = DOW(dvenc)
            IF (diasem = 1 .AND. (dpag - dvenc) <= 1) .OR.;
               (diasem = 7 .AND. (dpag - dvenc) <= 2) 
              nAtraso = 0
            ELSE
              nAtraso = dpag - dvenc   
            ENDIF  
            @lin, 54 say nAtraso PICT [999]
          ENDIF   
          * Juros
          nJuros = juros + custas
          @lin, 58 say nJuros PICT [99,999.99]
          * Desconto
          @lin, 71 say desc  PICT [99,999.99]
          nTotJuros = nTotJuros + nJuros
       endif
      
       lin = lin + 1 
         
       if obs <> space(20) .or. nfcli > 0 .or. ! empty(dtnfcli)
         if nfcli > 0 .or. !empty(dtnfcli)
           @lin,45 say 'DEV '+str(nfcli,6)+' '+ dtoc(dtnfcli) +' '+str(valdev,12,2)
           lin = lin + 1
         endif
       endif
         
       TOTDUPL = TOTDUPL + 1
       skip
     enddo   
   enddo
   texto = 'F I N A L   D O   R E L A T O R I O'
   @ lin+1, 00 SAY REPLICATE('-', LargPag )
   @ lin+2, 01 SAY "N.Dupls: " + STR( totDupl,    5, 0 )
   @ lin+2, 16 SAY "Em Aberto: " + TRANSFORM( totDeb,   [9,999,999.99] )
   @ lin+2, 41 SAY "Vencido: "   + TRANSFORM( totVenc,  [9,999,999.99] )
   @ lin+2, 64 SAY "Juros: "     + TRANSFORM( ntotJuros, [99,999.99] )
   @ lin+3, CENTRATXT(texto,LargPag) SAY Texto
   @ lin+4, 00 SAY REPLICATE('-', LargPag )

   DO IMPSAIDA1
enddo
close data
return

*******************
PROCEDURE IMPRCABEC
*******************
  =CABEC80('CONSDUP', 'SITUACAO DE CLIENTES', LargPag) 
  IF PAGCTR = 1 
     @04,01 say 'Lim.Cred.:'+str(MCRE,9)
     @04,27 say 'Debito:'
     @04,46 say 'Ult.Compra:'+DTOC(MULC)
     @04,68 say 'Tipo:'+MTIP
     @05,01 say 'Maior Deb:'+str(maiordeb,12,2) +'  '+ dtoc(dat)
     @05,46 say 'Ult.N.Fisc:'+dtoc(dat)
     Lin = 06
  ELSE
     Lin = 04   
  ENDIF 
  @lin+0,01 say 'RAZAO SOCIAL:' + MRAZ
  @lin+1,01 say 'Dupl/Parc  Emissao  Valor Duplic  Vencimen  Pagament  Atraso Juros   Desct/Acum'
  @lin+2,00 SAY REPLICATE('-',LargPag)
  lin = lin + 3
RETURN

********************
FUNCTION CALCDEBACUM
********************
   go top 
   do while .not. eof() .and. ICgc = cgc
      dat = emis
      if EMPTY(pag) THEN
         vc = venc
         debacum  = debacum + vdp 
         maiordeb =  debacum 
      endif
      skip
   enddo
RETURN 
