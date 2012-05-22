* CONSDUP - CONSULTA SITUACAO DE DUPLICATAS POR CLIENTE
* SISTEMA ADMINISTRACAO DE VENDAS - ADAO ELI PEREIRA - 02/03/87
*
stor 0 to mate
sele 0
use regiao order x_regiao
sele 0
use vend index vend
sele 0
use CLIEN ORDER X_GUE
sele 0
use DUPL index DUPCGC

set filter to EMPTY(DTCANCEL)

=CABTELA(0,0,24,79)
MENS = 'Consulta Duplicatas'
=MENSAGR(MENS)
@01,POS say MENS
@3,03 say 'N.Guerra'
@7,03 say 'Dupl/Parc  Emissao  Valor Duplic  Vencimen  Pagament'
@7,58 say 'Juros    Desct/Acum'
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
   @04,46 say 'Ult.Compra '+ULC
   mulc = ulc
   @04,67 say 'Tipo '+TIP
   mtip = tip
   stor 8 to lin
   sele DUPL
   if !seek(mcgc)
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
   *** Percorre o arquivo de Duplicatas 
   do while .not. eof() .and. ICgc = cgc
     *if vdp > maiordeb 
     *  maiordeb = vdp
     *  dat = emis   
     *endif
     dat = emis
     if pag = space(08)
       vc = venc
       debacum = debacum + vdp 
       maiordeb =  debacum 
     endif
     skip
   enddo
   
   @05,03 say 'Maior Deb '+str(maiordeb,12,2) +'  '+dat
   @05,46 say 'Ult.N.Fisc '+dat
   dia=emis
   seek mcgc
   *
   save screen to vtela
   =janela(11,33,17,48)
   =borda( 11,33,17,48)
   @13,36 prompt '  Na Tela ' message 'Visualizar s¢ na tela'
   @15,36 prompt 'Impressora' message 'Imprimir direto para Impressora'
   menu to opcao
   rest screen from vtela
   do case
      case opcao = 0
         exit
      case opcao = 1
         do while cgc = mcgc .and. .not. eof()
            @lin,3 say str(ndup,6)+'/'+str(pdup,1)
            @lin,14 say emis
            @lin,23 say str(vdp,12,2)
            if pag = space(8)
               totdeb = vdp + totdeb - valdev
            endif
            @lin,37 say venc
            if pag <> space(8)
               @lin,47 say pag
               @lin,56 say str(juros,10,2)
               @lin,67 say str(desc,10,2)
            else
               @lin,65 say str(totdeb,12,2)
            endif
            stor lin + 1 to lin
            if obs <> space(20) .or. nfcli > 0 .or. ! empty(dtnfcli)
               if nfcli > 0 .or. ! empty(dtnfcli)
                  @lin,45 say 'DEV '+str(nfcli,6)+' '+ dtoc(dtnfcli) +' '+str(valdev,12,2)
                  stor lin + 1 to lin
               endif
            endif
            if lin > 20
               @4,33 say str(TOTDEB,12,2)
               MC = ' '
               do while .not. MC$'SN'
                  MC = ' '
                  @23,01 clear to 23,78
                  @23,10 say 'Continua=(S/N)  'GET MC pict '!'
                  read
               enddo
               if MC = 'N'
                  exit
               endif
               @8,1 clear to 21,78
               stor 8 to lin
            endif
            skip
            if cgc <> mcgc
               @4,33 say str(totdeb,12,2)
               @23,01 clear to 23,78
               @23,10 say 'Aperte qualquer tecla para continuar'
               =INKEY(0)
            endif
         enddo
      case opcao = 2
         mens = 'Relat¢rio selecionado est  correto para impress„o ?'
         do pegresp with [SN]
         if mc = 'N' .or. empty(mc)
            exit
         endif
         stor space(01) to cp, srel
         stor space(08) to trel
         defrel = 'I'
         mens = 'Impressora est  ligada e com Papel dispon¡vel ?'
         do pegresp with [SN]
         if mc = 'N' .or. empty(mc)
            exit
         endif
         do RESEIMPR
         set devi to print
         LP=GETENV("LP")
         if empty(lp)
            lp="PRN"
         endif   
         set printer to &LP
         pagctr = 0
         li     = 56
         vcab   = 'SITUACAO DO CLIENTE: ' + alltrim(mgue)
         do while cgc = mcgc .and. .not. eof()
            if li >= 56
               pagctr = pagctr + 1
               @01,001 say mrel
               @01,030 say 'CONSDUP'
               @01,056 say date()
               @01,071 say 'Pag. '+str(pagctr,3)
               @02,001 say padc(vcab,78)
               @03,001 say replicate('-',78)
               @prow()+2, 1       say alltrim(mraz)
               @prow()  , 2 say ' - ' + alltrim(clien->cid) +' - '+ clien->est
               =seek(clien->regiao + STR(clien->idregiao,2),'regiao')
               @prow()+1, 1       say regiao->dregiao
               =seek(clien->ven,'vend')
               @prow()  , 2 say ' - ' + vend->raz
               @prow()+1, 1       say 'Limite de Credito '+ alltrim(str(MCRE,9))
               @prow()  ,10 say 'Ultima Compra ' + mULC
               if !empty(mtip)
                  @prow()  , 2 say 'Tipo ' + mtip
               endif
               @prow()+1,1  say 'Maior Debito ' + alltrim(str(maiordeb,12,2)) +' - '+ dat
               @prow()  ,6  say 'Ultima Nota Fiscal de ' + dat
               @prow()+2,1  say ' Dupl/Parc   Emissao  Valor Duplic  Vencimen  Pagament   Juros    Desct/Acum'
               @prow()+1,1  say '==========   ======== ============  ========  ======== ========== ============'
               li = prow()+1
            endif
            @li,03 say str(ndup,6)+'/'+str(pdup,1)
            @li,14 say emis
            @li,23 say str(vdp,12,2)
            if pag = space(8)
               stor (vdp + totdeb - valdev) to totdeb
            endif
            @li,37 say venc
            if pag <> space(8)
               @li,47 say pag
               @li,56 say str(juros,10,2)
               @li,67 say str(desc,12,2)
            else
               @li,67 say str(totdeb,12,2)
            endif
            li = li + 1
            if obs <> space(20) .or. nfcli > 0 .or. ! empty(dtnfcli)
               if nfcli > 0 .or. ! empty(dtnfcli)
                  @li,45 say 'DEV '+str(nfcli,6)+' '+ dtoc(dtnfcli) +' '+str(valdev,12,2)
                  li = li + 1
               endif
            endif
            skip
         enddo
         set devi to scre
         eject
   endcase
enddo
close data
return

