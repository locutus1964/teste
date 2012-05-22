* AV065001 - IMPRESSAO DE PEDIDO DE CLIENTES
* SISTEMA ADMINISTRACAO DE VENDAS
* Adao Eli Pereira - 24-04-90
*
do testimp2
if  cp = 'N'
   do av065001fim
   return
endif
lTemIpi = .F.
use obsped
md01    = d01
md02    = d02
md03    = d03
md04    = d04
md05    = d05
imprime = '*'
*teste   = space(01)

stor 0 to TVALIQU, TVALIPI, CAB
close data
sele 0
use CADNF    ORDER X_PEDNF
sele 0
use CLI_ENTR index CLI_ENTR.IDX
sele 0
use CLI_COBR index CLI_COBR.IDX
sele 0
use CLIEN    ORDER P_CGC
sele 0
use PEDIC    ORDER P_PED
sele 0
use PEDID    ORDER P_PEDID
sele 0
use PROD     index PROD.IDX
sele 0
use VEND     ORDER P_VEND
sele 0
use TRANS    index TRANS.IDX
sele 0
use NOMMOEDA index NOMMOEDA.IDX
SELE 0
USE TPNF INDEX TPNF.IDX
stor 1 to mcopias
stor 0 to mped, fl, miped, muped, tval, valtot
stor space(40) to mo1, mo2, mo3, mo4, mo5, mo6
=janela(09,28,15,77)
=borda(09,28,15,77)
do while .t.
   stor 0 to iped, uped
   @11,31 say 'N£mero do primeiro Pedido a imprimir.' get iped    ;
   valid av065001val(iped)          pict '999999'
   @12,31 say 'N£mero do £ltimo Pedido a imprimir...' get uped    ;
   valid av065001val(uped)          pict '999999'
   @13,31 say 'C¢pias desejadas.....................' get mcopias ;
   valid mcopias > 0          pict '99'
   read
   if lastkey() = 27
      do av065001fim
      return
   endif
   mens = dadcorr
   do pegresp with [SN]
   if mc = 'S'
      =mensag('Imprimindo os Pedidos solicitados... Aguarde')
      exit
   endif
enddo
*mped   = iped
tamrel = 2
do defsaida
mtotcopias = 1
set deci to 4
do while mtotcopias <= mcopias
stor 0.00 to VALTOT, TVALIQU, TVALIPI
mped   = iped
sele pedid
seek STR(mped,6)
do while MPED <= UPED
   sele PEDID
   if !seek(str(MPED,6))
      set device to SCRE
      =mensag('Pedido sem itens cadastrados nr.: '+mped+' - ENTER')
      =INKEY(0)
      do av065001fim
      return
   endif
   sele PEDIC
   if !seek(MPED)
     set device to SCRE
     =mensag('Pedido n„o est  cadastrado - ENTER')
     =INKEY(0)
     loop
   endif  
   SELE TPNF
   SEEK( PEDIC->TIP )
   lTemIpi = TPNF->IPIT <> '2' .AND. TPNF->IPIT <> '4'
   SELE PEDIC
   MCGC  = CGC
   MIDREPR = IDREPR
   MVEN  = VEN
   MDEN  = DEN
   MTRA  = TRA
   MTABP = TABP
   MTIPNF = PEDIC->TIP
   STORE SPACE(2) TO cCp1, cCp2, cCp3, cCp4
   IF CP1 > 0 
     cCP1 = STR(CP1,2)
   ENDIF   
   IF CP2 > 0 
     cCP2 = STR(CP2,2)
   ENDIF   
   IF CP3 > 0 
     cCP3 = STR(CP3,2)
   ENDIF   
   IF CP4 > 0 
     cCP4 = STR(CP4,2)
   ENDIF   
   MCPD      = CPD
   MPCL      = PCL
   MDTPEDCLI = DTPEDCLI
   MPINT     = PINT
   MFRE      = FRE
   MEMB      = EMB
   MDEM      = DEM
   MO1       = O1
   MO2       = O2
   MO3       = O3
   MO4       = O4
   MO5       = O5
   MO6       = O6
   MDESCTO1  = DESCTO1
   MDESCTO2  = DESCTO2
   MDESCSEM  = DESCSEM
   MCFINANC  = CFINANC
   MCFIN2    = CFIN2
   MCFIN3    = CFIN3
   MCFIN4    = CFIN4
   MDESCONTO = DESCONTO
   MMOEDA    = MOEDA
   MSIGLA    = SIGLA
   sele NOMMOEDA
   if !seek(str(MMOEDA,3))
      set device to SCRE
      =mensag('Pedido sem Moeda cadastrada nr.: '+mped+' - ENTER')
      =INKEY(0)
      do av065001fim
      return
   else
      MAMOEDA = AMOEDA
   endif
   ** --------------------- IMPRESSAO DO PEDIDO ------------------------------
   vlinha = '|    |         |   |    |                                ' + ;
   '                     |             |     |     |     |    |             |'
*   IMP  = '*'
   FL   = 0
*   MITE = 1
*   do while IMP = '*'
      @01,002 say replicate('-',128)
      @02,001 say '|     C E N T R O F I X   INDUSTRIA E COMERCIO LTDA.'
      @02,063 say '| PREPARADOR:                        EMBALADOR:'
      @02,130 say '|'
      @03,001 say '|     Rua Nazareth Rezek Farah, 301 - Vila Santa Catarina'
      @03,063 say '| Data......: ____/____/____         Data.....: ____/____/____'
      @03,130 say '|'
      @04,001 say '|     CEP :  04367-050   -   SAO PAULO   -   SP '
      @04,063 say '| No Reg....: ________               No Reg...: ________'
      @04,130 say '|'
      @05,001 say '|     TEL : (011) 5562.6655   -   FAX : (011) 5562.7313'
      @05,063 say '| Visto.....:                        Visto....:'
      @05,130 say '|'
      @06,001 say '|  C.G.C. : 96.160.650/0001-73 - INSC.EST. : 113.756.369.115'
      @06,063 say '|             ___________________               __________________'
      @06,130 say '|'
      @07,001 say  replicate('-',130)
      @08,001 say  replicate('-',130)
      sele CLIEN
      if seek(MCGC)
         MINS = INS
      else
         MINS = space(13)
      endif
      @09,001 say '| CLIENTE : '+RAZ
      @09,063 say '| NRO. PEDIDO'
      @09,080 say '| EMITIDO EM'
      @09,094 say '|  REVISAO'
      @09,107 say '| SIGLA'
      @09,115 say '| MOEDA'
      @09,130 say '|'
      @10,001 say '| ENDERECO: '+END
      @10,063 say '|'
      @10,067 say MPED          pict '999,999'
      @10,080 say '|'
      @10,083 say MDEM
      @10,094 say '|'
      if date() <> MDEM
         @10,096 say dtoc(date())
      endif
      @10,107 say '|'
      @10,109 say MSIGLA
      @10,115 say '|'
      @10,116 say str(MMOEDA,2)+' - '+MAMOEDA
      @10,130 say '|'
      @11,001 say '| CIDADE  : '+RTRIM(CID)+' - '+BAI
      @11,063 say '|'
      @11,064 say replicate('-',66)
      @11,130 say '|'
      @12,001 say '| ESTADO  : '+EST
      MEST = EST
      @12,017 say 'CEP: '+CEP
      @12,033 say 'TEL: '+DDD1+'- '+FON1+' '+FON2
      ENDENT = space(30)
      CGCENT = space(20)
      INSENT = space(13)
      sele CLI_ENTR
      if seek(SUBS(MCGC,1,16))
         ENDENT = ENDE
         CGCENT = CGCE
         INSENT = INSE
      endif
      xendcob = space(30)
      cidcob = space(50)
      sele CLI_COBR
      if seek(SUBS(MCGC,1,16))
         xendcob = ENDC
         CIDCOB = RTRIM(CIDC)+','+' '+RTRIM(BAIC)+','+' '+ESTC+','+' '+CEPC
      else
         xendcob = " O MESMO"
         CIDCOB = space(01)
      endif
      @12,063 say '| LOC ENT: '+ENDENT
      @12,130 say '|'
      @13,001 say '| COND PAG:'
      MCPI = cCP1 + ' ' + cCP2 + ' ' + cCP3 + '  ' + cCP4
      @13,015 say RTRIM(MCPI)+' '+MCPD
      @13,063 say '| CGC: '+CGCENT
      @13,096 say 'INSC EST: '+INSENT
      @13,130 say '|'
      @14,001 say '| LOC COBR: '+ xendcob
      sele VEND
      seek MIDREPR
      if !(eof() .or. bof())
         MNOME  = RAZ
      else
         MNOME = space(01)
      endif
      @14,063 say '| REPRES: '+FMTREPR(VEND->CVR,VEND->IDREPR)+' - '+MNOME
      @14,130 say '|'
      @15,001 say '| '+CIDCOB
      @15,063 say '| PED REPRES. - NRO: '+MPINT
      @15,100 say 'VAL.DESC.:'
      @15,111 say MDESCONTO          pict '99,999.99'
      @15,130 say '|'
      @16,001 say '| CGC: '+MCGC
      @16,036 say 'INSC EST: '+MINS
      @16,063 say '| % DESCTO CLIENTE :'
      @16,084 say MDESCTO1          pict '99.99'
      @16,089 say '+'
      @16,091 say MDESCTO2          pict '99.99'
      @16,100 say 'TAB.PRECO: '+MTABP
      @16,117 say 'TIPO NF.: '+MTIPNF
      @16,130 say '|'
      @17,001 say '| PED.CLIENTE: '+MPCL
      @17,022 say '- ' + DTOC(MDTPEDCLI)
      @17,036 say 'PRAZO DE ENTREGA: '+DTOC(MDEN)
      **         @17,63  say '| CUSTO FINANCEIRO : '
      **         @17,84  say MCFINANC          pict '99.99'
      **         @17,89  say '+'
      **         @17,91  say MCFIN2            pict '99.99'
      @17,063 say '| CUST FIN.:'
      @17,pcol()  say MCFINANC          pict '99.99'
      @17,pcol()  say '+'
      @17,pcol()  say MCFIN2            pict '99.99'
      @17,pcol()  say '+'
      @17,pcol()  say MCFIN3            pict '99.99'
      @17,pcol()  say '+'
      @17,pcol()  say MCFIN4            pict '99.99'
      @17,100 say 'DESC.SEM.:'
      @17,111 say MDESCSEM          pict '99.99'
      @17,130 say '|'
      @18,001 say replicate('-',130)
      @19,001 say '|ITEM|QUANTIDAD'
      @19,016 say '|EMB|REF | DESCRICAO DO PRODUTO'
      IF lTemIpi
        @19,079 say '|PRECO UNITAR.|D.GRU|PROMO|BONUS|%IPI|VALOR LIQUIDO|'
      ELSE  
        @19,079 say '|PRECO UNITAR.|D.GRU|PROMO|BONUS|    |VALOR LIQUIDO|'
      ENDIF  
      @20,001 say replicate('-',130)
      FL = FL + 1
      lin = 21
      stor 0.00 TO MVALLIQ,MVALTOT,MPRE,MQTD,TEMB,TDIT,TBON,TFRE,TDGE,TIPI
      sele PEDID
      do while MPED = PED .and. !eof()
         @lin,1 say '|'
*        @lin,3 say str(MITE,2)
         @lin,3 say str(ITE,2)
         @lin,6 say '|'
         @lin,7 say str(QTD,8)
         stor REF TO MREF
         sele PROD
         if seek(MREF)
            @lin,16 say '|'
            @lin,17 say EMBALAGEM
            MDESCRICAO = DES
         endif
         @lin,20 say '|'
         sele PEDID
         @lin,21 say MREF
         @lin,25 say '|'
         @lin,26 say MDESCRICAO
         @lin,79 say '|'
         IF .NOT. lTemIpi
           *---- Ignorando o IPI, Imposto Simples
           MIPI = 0
         ELSE  
           MIPI     = IPI
         ENDIF
         MQTD     = QTD
         mdit     = DIT
         mpre     = PRE
         mbon     = BON
         mdescgru = DESCGRU
         mprebru  = PREBRU
         mtabref  = TABREF
         stor 0.00 to VALIQU, VALIPI, VALTOTAL, DGRUPO, DCLIENTE, DCLIENTE1
         stor 0.00 to DCLIENTE2, DMDIT, DBONUS, DSEMANA, DFINANC,VALIQU
         VALIQU = (MQTD * MPRE)
         VALIPI = round(((VALIQU * MIPI) / 100),2)
         TVALIPI = (TVALIPI + VALIPI)
         @lin,80 say (VALIQU / MQTD)             pict '999,999.9999'
         TVALIQU = (TVALIQU + VALIQU)
         @lin,93 say '|'
         if MDESCGRU > 0
            @lin,94 say MDESCGRU                 pict '99.99'
         endif
         @lin,99  say '|'
         if MDIT > 0
            @lin,100 say str(MDIT,5,2)
         endif
         @lin,105 say '|'
         if MBON > 0
            @lin,106 say str(MBON,5,2)
         endif
         @lin,111 say '|'
         if lTemIpi
            @lin,112 say str(MIPI,4,1)
         endif
         @lin,116 say '|'
         @lin,117 say VALIQU          pict '99,999,999.99'
         @lin,130 say '|'
         lin = lin + 1
*         MITE = MITE + 1
*         teste = ' '
         skip
      enddo
*            IMP   = ' '
*            teste = '*'
            if lin < 45
               do while lin < 45
                  @lin,1 say vlinha
                  lin = lin + 1
               enddo
            endif
*            if lin > 44
               @lin,001 say '|'
               @lin,002 say replicate('-',128)
               @lin,130 say '|'
               sele TRANS
               if seek(MTRA)
                  MRAZ = MTRA+' - '+RAZ
                  MEND = RTRIM(END)+' - '+CID
                  MBAI = BAI
                  MEST = EST
                  MTEL = '('+DDD+') ' + TEL + ' (' +DDD2+') ' + TEL2
                  MLOCALIZ = LOCALIZ
               else
                  stor space(25) to MRAZ, MEND, MBAI, MLOCALIZ
                  MEST = space(02)
                  MTEL = space(10)
               endif
               @46,001 say '| TRANSP: '+MRAZ+ ' - LOCAL.: ' + MLOCALIZ
               IF lTemIpi 
                 @46,100 say 'TOTAL S/IPI :'
                 @46,116 say TVALIQU                  pict '999,999,999.99'
               ENDIF  
               @46,130 say '|'
               @47,001 say '| ENDER : '+MEND
               IF lTemIpi
                 @47,100 say 'VALOR DO IPI:'
                 @47,116 say TVALIPI                  pict '999,999,999.99'
               ENDIF  
               @47,130 say '|'
               @48,001 say '| BAIRRO: '+MBAI
               @48,032 say 'ESTADO: '+MEST
               @48,047 say 'TELEFONES: '+MTEL
               *@48,100 say 'TOTAL C/IPI :'
               @48,100 say 'TOTAL.......:'
               @48,116 say TVALIQU + TVALIPI         pict '999,999,999.99'
               @48,130 say '|'
               @49,001 say '|'
               @49,002 say replicate('-',128)
               @49,130 say '|'
               @50,1   say '| '+MD01
               @50,74  say '| '+MO1
               @50,130 say '|'
               @51,1   say '| '+MD02
               @51,74  say '| '+MO2
               @51,130 say '|'
               @52,1   say '| '+MD03
               @52,74  say '| '+MO3
               @52,130 say '|'
               @53,1   say '| '+MD04
               @53,74  say '| '+MO4
               @53,130 say '|'
               @54,1   say '| '+MD05
               @54,74  say '| '+MO5
               @54,130 say '|'
               @55,1   say '| DE ACORDO : ' + CLIEN->CONTATO + ' ' + CLIEN->CONTATO2
               @55,74  say '| '+MO6
               @55,130 say '|'
               @56,001 say '|' + replicate('-',128) + '|'
               @57,01  say '|'
               @57,03  say 'Credito e Cobranca'
               @57,25  say '|'
               @57,27  say 'Depto de Vendas '
               @57,50  say '| Limite de/credito'
               @57,74  say '| Uso interno'
               @57,130 say '|'
               @58,1   say '|'
               @58,25  say '|'
               @58,50  say '|'
               @58,74  say '|'
               @58,130 say '|'
               @59,001 say '|'
               @59,025 say '|'
               @59,050 say '|'
               @59,074 say '|'
               @59,130 say '|'
               @60,001 say '|' + replicate('-',128) + '|'
               @61,001 say '| Pedido atendido c/N.Fiscais/dt emis/dt embar:'
               @61,066 say '| VOLUMES: ________'   
               @61,130 say '|' 
               @62,001 say '|' 
               LinhaPeso = '| PESO BRUTO: ______ Kg     PESO LIQ.: ______ Kg'
               @62,066 say '|'
               @62,130 say '|'                
               sele cadnf
               if seek(str(mped,6))
                  coluna = 2
                  linha  = 62
                  mnnf = 0
                  do while ped = mped
                     if nnf <> mnnf
                        if empty(cancelnf)
                           @linha,coluna say str(nnf,6)+' '+demi+' '+ent
                           coluna = coluna + 27
                        endif
                        if coluna > 59
                           if linha = 63                      
                             @linha, 066 say LinhaPeso    
                           endif  
                           @linha,130 say '|'
                           coluna = 2
                           linha  = linha + 1
                           @linha,001 say '|'
                        endif
                        mnnf = nnf
                     endif
                     skip
                  enddo
                  if linha = 62
                     @62,066 say '|'
                     @62,130 say '|'
                     @63,001 say '|'
                     @63,066 say LinhaPeso
                     @63,130 say '|'
                  endif
               else
                  @63,001 say '|'
                  @63,066 say LinhaPeso
                  @63,130 say '|'
               endif
               @64,001 say '|' + replicate('-',128) + '|'
         MPED = MPED + 1
         stor 0.00 to VALTOT, TVALIQU, TVALIPI
enddo
if defrel = 'I'
   mtotcopias = mtotcopias + 1
else
   exit
endif
enddo
do IMPSAIDA
do av065001fim
return
*ßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßß
procedure av065001fim
set deci to
close data
return
*ßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßß
function av065001val
parameters vpar
vretorna = .t.
save screen to lixo
if vpar <= 0
   if varread() = 'UPED'
      uped = iped
   endif
   =mensag(CNUMER +' - ENTER')
   =INKEY(0)
   vretorna = .f.
else
   if varread() = 'IPED'
      if !seek(iped,'pedic')
         =mensag('Primeiro Pedido n„o est  cadastrado - ENTER')
         =INKEY(0)
         vretorna = .f.
      else
         if !seek(str(iped,6),'pedid')
            =mensag('Pedido n„o tem ¡tens cadastrados - ENTER')
            =INKEY(0)
            vretorna = .f.
         endif
      endif
      uped = iped
   else
      if !seek(uped,'pedic')
         =mensag('Ultimo Pedido n„o est  cadastrado - ENTER')
         =INKEY(0)
         vretorna = .f.
      endif
   endif
endif
rest screen from lixo
return vretorna

