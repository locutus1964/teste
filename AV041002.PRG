* AV041002 - ATUALIZACAO DO CADASTRO DE CLIENTES - LOCAL DE COBRANCA
* SISTEMA DE ADMINISTRACAO DE VENDAS
* ADAO ELI PEREIRA -28/07/93
*
SAVE SCREEN TO TELA
@10,00 clear to 24,79
=CABTELAC(10,00,24,79)
MENS = 'Atualiza‡„o do Endere‡o de Cobran‡a'
=MENSAGR(MENS)
@11,POS SAY MENS
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
SELE CLI_COBR
IF SEEK(SUBS(MCGC,1,16))
   @14,03 say endc COLOR &CORVER
   mendc = endc
   @14,51 say cidc COLOR &CORVER
   mcidc = cidc
   @17,03 say baic  COLOR &CORVER
   mbaic = baic
   @17,26 say estc COLOR &CORVER
   mestc = estc
   @17,31 say cepc  COLOR &CORVER
   mcepc = cepc
   @17,63 say ddd_faxc  COLOR &CORVER
   mddd_faxc = ddd_faxc
   @17,68 say num_faxc  COLOR &CORVER
   mnum_faxc = num_faxc
   @20,03 say ddd1c  COLOR &CORVER
   mddd1c = ddd1c
   @20,08 say fon1c  COLOR &CORVER
   mfon1c = fon1c
   @20,19 say ddd2c  COLOR &CORVER
   mddd2c = ddd2c
   @20,24 say fon2c  COLOR &CORVER
   mfon2c = fon2c
   @20,34 say contat1c  COLOR &CORVER
   mcontat1c = contat1c
   @20,56 say contat2c  COLOR &CORVER
   mcontat2c = contat2c
   MENS = TESTEINC
   DO PEGOPCAO WITH [123]
   IF OPCAO = '1'
      RETURN
   ELSE
      IF OPCAO = '3'
         DELE
         MDEL = 1
      ENDIF
   ENDIF
ENDIF
IF !SEEK(SUBS(MCGC,1,16))
   MENS = TESTECAD
   DO PEGRESP WITH [SN]
   IF MC = 'N' .or. empty(mc)
      RETURN
   ELSE
      mcidc  = SPACE(25)
      mbaic  = SPACE(20)
      mendc  = SPACE(45)
      mestc  = SPACE(02)
      mcepc  = space(09)
      mddd1c = SPACE(04)
      mfon1c = SPACE(09)
      mddd2c = SPACE(04)
      mfon2c = SPACE(09)
      mddd_faxc = SPACE(04)
      mnum_faxc = SPACE(09)
      mcontat1c = SPACE(20)
      mcontat2c = SPACE(20)
      @14,03 say mendc       COLOR &CORVER
      @14,51 say mcidc       COLOR &CORVER
      @17,03 say mbaic       COLOR &CORVER
      @17,26 say mestc       COLOR &CORVER
      @17,31 say mcepc       COLOR &CORVER
      @17,63 say mddd_faxc   COLOR &CORVER
      @17,68 say mnum_faxc   COLOR &CORVER
      @20,03 say mddd1c      COLOR &CORVER
      @20,08 say mfon1c      COLOR &CORVER
      @20,19 say mddd2c      COLOR &CORVER
      @20,24 say mfon2c      COLOR &CORVER
      @20,34 say mcontat1c   COLOR &CORVER
      @20,56 say mcontat2c   COLOR &CORVER
   ENDIF
ENDIF
continua = .t.
do while continua
   @14,03 get mendc valid !empty(mendc) pict '@!'
   @14,51 get mcidc valid !empty(mcidc) pict '@!'
   @17,03 get mbaic                     pict '@!'
   @17,26 get mestc valid seek(mestc,'estados') pict '!!'
   @17,31 get mcepc
   @17,63 get mddd_faxc
   @17,68 get mnum_faxc
   @20,03 get mddd1c
   @20,08 get mfon1c
   @20,19 get mddd2c
   @20,24 get mfon2c
   @20,34 get mcontat1c pict '@!'
   @20,56 get mcontat2c pict '@!'
   read
   if lastkey() = 27
      exit
   endif
   MENS = DADCORR
   DO PEGRESP WITH [SN]
   if mc = 'N' .or. empty(mc)
      loop
   endif
   SELE CLI_COBR
   if !SEEK(SUBS(MCGC,1,16))
      appe blan
   endif
   repl cgcc with mcgc, endc with mendc,cidc with mcidc, baic with mbaic
   repl estc with mestc,cepc with mcepc,ddd1c with mddd1c,fon1c with mfon1c
   repl ddd2c with mddd2c,fon2c with mfon2c,contat1c with mcontat1c
   repl contat2c with mcontat2c,ddd_faxc with mddd_faxc,num_faxc with mnum_faxc
   continua = .F.
enddo
return
