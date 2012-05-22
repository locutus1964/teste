* AV041003 - ATUALIZACAO do CADASTRO DE CLIENTES - LOCAL DE COMPRA
* SISTEMA DE ADMINISTRACAO DE VENDAS
* ADAO ELI PEREIRA -29/07/93
*
*save screen to tela
@10,00 clear to 24,79
=CABTELAC(10,00,24,79)
mens = 'Atualiza‡„o do Endere‡o de Compra'
=MENSAGR(mens)
@11,POS say mens
@13,03 say 'Endere‡o'
@13,51 say 'Cidade'
@16,03 say 'Bairro'
@16,26 say 'Est'
@16,31 say 'CEP'
@16,61 say 'FAX (DDD/N£mero)'
@19,03 say 'DDD1 Telef 1'
@19,18 say 'DDD2 Telef 2'
@19,34 say 'Contato 1'
@19,56 say 'Contato 2'
sele CLI_MATR
if seek(SUBS(MCGC,1,16))
   @14,03 say endm color &corver
   mendm = endm
   @14,51 say cidm color &corver
   mcidm = cidm
   @17,03 say baim  color &corver
   mbaim = baim
   @17,26 say estm color &corver
   mestm = estm
   @17,31 say cepm  color &corver
   mcepm = cepm
   @17,63 say ddd_faxm  color &corver
   mddd_faxm = ddd_faxm
   @17,68 say num_faxm  color &corver
   mnum_faxm = num_faxm
   @20,03 say ddd1m  color &corver
   mddd1m = ddd1m
   @20,08 say fon1m  color &corver
   mfon1m = fon1m
   @20,19 say ddd2m  color &corver
   mddd2m = ddd2m
   @20,24 say fon2m  color &corver
   mfon2m = fon2m
   @20,34 say contat1m  color &corver
   mcontat1m = contat1m
   @20,56 say contat2m  color &corver
   mcontat2m = contat2m
   mens = TESTEINC
   do PEGOPCAO with [123]
   if OPCAO = '1'
      return
   else
      if OPCAO = '3'
         dele
         MDEL = 1
      endif
   endif
endif
if !seek(SUBS(MCGC,1,16))
   mens = TESTECAD
   do pegresp with [SN]
   if MC = 'N' .or. empty(mc)
      return
   else
      mendm  =    SPACE(45)
      mcidm  =    SPACE(25)
      stor spac(20) to mbaim, mcontat1m, mcontat2m
      stor space(09) to mcepm, mfon1m, mfon2m, mnum_faxm
      stor SPACE(04) to mddd1m, mddd2m, mddd_faxm
      mestm  = SPACE(02)
      @14,03 say mendm       color &corver
      @14,51 say mcidm       color &corver
      @17,03 say mbaim       color &corver
      @17,26 say mestm       color &corver
      @17,31 say mcepm       color &corver
      @17,63 say mddd_faxm   color &corver
      @17,68 say mnum_faxm   color &corver
      @20,03 say mddd1m      color &corver
      @20,08 say mfon1m      color &corver
      @20,19 say mddd2m      color &corver
      @20,24 say mfon2m      color &corver
      @20,34 say mcontat1m   color &corver
      @20,56 say mcontat2m   color &corver
   endif
endif
continua = .t.
do while continua
   @14,03 get mendm valid !empty(mendm) pict '@!'
   @14,51 get mcidm valid !empty(mcidm) pict '@!'
   @17,03 get mbaim                     pict '@!'
   @17,26 get mestm valid seek(mestm,'estados') pict '!!'
   @17,31 get mcepm
   @17,63 get mddd_faxm
   @17,68 get mnum_faxm
   @20,03 get mddd1m
   @20,08 get mfon1m
   @20,19 get mddd2m
   @20,24 get mfon2m
   @20,34 get mcontat1m pict '@!'
   @20,56 get mcontat2m pict '@!'
   read
   if lastkey() = 27
      exit
   endif
   mens = DADCORR
   do pegresp with [SN]
   if mc = 'N' .or. empty(mc)
      loop
   endif
   sele CLI_MATR
   if !seek(SUBS(MCGC,1,16))
      appe blan
   endif
   repl cgcm       with mcgc       ,endm       with mendm      ,;
        cidm       with mcidm      ,baim       with mbaim      ,;
        estm       with mestm      ,cepm       with mcepm      ,;
        ddd1m      with mddd1m     ,fon1m      with mfon1m
   repl ddd2m      with mddd2m     ,fon2m      with mfon2m     ,;
        contat1m   with mcontat1m  ,contat2m   with mcontat2m  ,;
        ddd_faxm   with mddd_faxm  ,num_faxm   with mnum_faxm
   continua = .f.     
enddo
return
