* AV04106 - ATUALIZACAO do CADASTRO DE CLIENTES - 11/11/86
*           REFORMULADO EM 17/06/93
* SISTEMA DE ADMINISTRACAO DE VENDAS
* ADAO ELI PEREIRA - 13-04-90
*
close data
SELE 0
USE FORRED ORDER PK_FORRED
SELE 0
USE FORGAN ORDER PK_FORGAN
SELE 0
USE FORCOR ORDER PK_FORCOR
SELE 0
USE FORBUC ORDER PK_FORBUC
sele 0
use bancos index bancos.idx
sele 0
use pedid ORDER X_CGCPEDRF
Sele 0
use CTR_SENH index CTR_SENH.idx
Sele 0
use SIGLAS   index SIGLAS.idx
Sele 0
use CLI_MATR ORDER P_CGC
Sele 0
use CLI_COBR ORDER P_CGC
Sele 0
use CLI_ENTR ORDER P_CGC
Sele 0
use ATIVIDAD index ATIVIDAD.idx
Sele 0
use REGIAO   order x_regiao
Sele 0
use CONCEITO index CONCEITO.idx
Sele 0
use STATUS   index STATUS.IDX
Sele 0
use ESTADOS  index ESTADOS.IDX
Sele 0
use CLIEN    ORDER P_CGC 
Sele 0
use TRANS    index TRANS.IDX,TRAGUE.IDX
sele 0
use VEND     ORDER X_VEND
sele 0
use DUPL     ORDER XDUPCGC
*
SAVE SCREEN TO MAIN2_SCR
*
=CABTELA(0,0,24,79)
*
MOFFSET  = 1
VNOMEPRG = 'AV04106'
@01,mensagr('AtualizaáÑo de Clientes') say 'AtualizaáÑo de Clientes'
@03,MOFFSET + 00 say 'CGC/C¢digo       Nome de Guerra  RegiÑo  Repr.  Status Desctos   Cond.Pgto'
@04,MOFFSET + 59 say '+'
@05,MOFFSET + 00 say 'RazÑo Social                                     Cidade'
@07,MOFFSET + 00 say 'Endereáo                                         Bairro'
@09,MOFFSET + 00 say 'Est CEP       Localiz.       Inscr Estadual    Inscr Sufr     C Postal  D.ICM'
@11,MOFFSET + 00 say 'DDD1  Telef 1      DDD2 Telef 2      CELULAR (DDD/Num)    FAX (DDD/N£mero)'
*0000 0000-0000     0000 0000-0000    0000 0000-0000       0000 0000=0000
@13,MOFFSET + 00 say 'Contato 1            Contato 2            Ativ Transp   Cadast Gan Buc Red Cor'
@15,MOFFSET + 00 say 'Inclus.     Compra1    Compra2   P CrÇdito Devedor  Conceito(1/2) Aceit(+/-)'
@17,MOFFSET + 00 say 'Venctos Especiais      Dt Alter   Resp  Nome Resp. p/£ltima alter.'
@19,MOFFSET + 00 say 'Nß Banco  Agencia     Entregaƒ        Faturamentoƒ        Cobranáaƒ'
@21,MOFFSET + 00 say 'Obs.:' 
MDEL = 0
MCGC = space(16)
MVEN = SPACE(4)
MIDREPR = 0
*

do while  .t.
   tcgc  = space(16)
   mcid  = space(25)
   stor space(20) to mbai, mcontato, mcontato2
   stor space(45) to mraz, mend
   store space(3) to mforred, mforgan, mforcor, mforbuc
   stor space(09) to mcep, mfon1, mfon2, mnum_faxc, mnum_celu
   MLOCALIZ = SPACE(15)
   MCONDPG1 = 0
   MCONDPG2 = 0
   MCONDPG3 = 0
   mbox  = space(05)
   MAVSP = 'N'   
   stor space(17) to mins, mves
*  mcon  = space(10)
   midregiao = 0
   mregiao = EmptyRegiao
   stor space(04) to mddd1, mddd2, mddd_celu, mddd_faxc, matividade, msigla, mclc
   MCVR = EmptyRepr
   stor 0 to mcre, mdeb, acha, magencia, mnbanco
   stor space(02) to mcest, mconceito, sconceito
   stor space(01) to mfiju, mdic, mstatus, mentr, mfatu, mcobr
   mobserv = SPACE(70)
   stor CTOD('') to minic, mdtalt
*  stor space(08) to mcmu
   stor space(03) to mloja
   stor space(04) to mtra, MTRA_OPC
   
   mmgue = space(15)
   mulc   = {}
   mulcsp = {}
   misu = space(13)
   mtip = '*'
   mcgc = space(16)
   stor 0.0 to maceitama, maceitame, mdescto1,    mdescto2
   
   @04,MOFFSET + 17 say MMGUE       color &corver
   @04,MOFFSET + 33 say mregiao     color &corver
   @04,MOFFSET + 41 say mcvr        color &corver
   @04,MOFFSET + 50 say mstatus     color &corver
   @04,MOFFSET + 55 say mdescto1 pict '99.9' color &corver
   @04,MOFFSET + 60 say mdescto2 pict '99.9' color &corver
   @04,MOFFSET + 66 say mcondpg1 pict '99' color &corver
   @04,MOFFSET + 69 say mcondpg2 pict '99' color &corver
   @04,MOFFSET + 72 say mcondpg3 pict '99' color &corver   
   
   @06,MOFFSET + 00 say mraz        color &corver
   @06,MOFFSET + 49 say mcid        color &corver
   
   @08,MOFFSET + 00 say mend        color &corver
   @08,MOFFSET + 49 say mbai        color &corver
   
   @10,MOFFSET + 00 say MCEST       color &corver
   @10,MOFFSET + 03 say mcep        color &corver
   @10,MOFFSET + 13 SAY mlocaliz	 color &corver
   @10,MOFFSET + 29 say mins        color &corver
   @10,MOFFSET + 47 say misu        color &corver
*  @10,MOFFSET + 67 say mcmu       color &corver
   @10,MOFFSET + 64 say mbox        color &corver
   @10,MOFFSET + 74 say mdic        color &corver
   
   @12,MOFFSET + 00 say mddd1       color &corver
   @12,MOFFSET + 05 say mfon1       color &corver
   @12,MOFFSET + 19 say mddd2       color &corver
   @12,MOFFSET + 24 say mfon2       color &corver
   @12,MOFFSET + 37 say mddd_celu   color &corver
   @12,MOFFSET + 42 say mnum_celu   color &corver
   @12,MOFFSET + 58 say mddd_faxc   color &corver
   @12,MOFFSET + 63 say mnum_faxc   color &corver
*  @12,MOFFSET + 64 say mcon        color &corver

   @14,MOFFSET + 00 say mcontato    color &corver
   @14,MOFFSET + 21 say mcontato2   color &corver
   @14,MOFFSET + 42 say matividade  color &corver
   @14,MOFFSET + 47 say mtra        color &corver
   @14,MOFFSET + 52 say MTRA_OPC    color &corver
   @14,MOFFSET + 57 say mclc        pict '@R 99/99' color &corver
   @14,MOFFSET + 63 say mforgan     color &corver
   @14,MOFFSET + 67 say mforbuc     color &corver
   @14,MOFFSET + 71 say mforred     color &corver
   @14,MOFFSET + 75 say mforcor     color &corver
   
   @16,MOFFSET + 00 say minic       color &corver
   @16,MOFFSET + 12 say mulc        color &corver
   @16,MOFFSET + 23 say mulcsp      color &corver
   @16,MOFFSET + 34 say mavsp       color &corver
   @16,MOFFSET + 36 say mcre        color &corver picture [999999]
   @16,MOFFSET + 43 say mdeb        color &corver picture [999999]
   @16,MOFFSET + 57 say mconceito   color &corver
   @16,MOFFSET + 61 say sconceito   color R+/B
   @16,MOFFSET + 68 say maceitama pict '99.9'  color &corver
   @16,MOFFSET + 73 say maceitame pict '99.9'  color &corver
   
   @18,MOFFSET + 00 say mves       color &corver
   @18,MOFFSET + 23 say mdtalt     color &corver
   @18,MOFFSET + 35 say msigla     color &corver
   @18,MOFFSET + 40 say space(30)  color &corver
   
   @20,MOFFSET + 33 say mentr     color &corpisca
   @20,MOFFSET + 54 say mfatu     color &corpisca
   @20,MOFFSET + 72 say mcobr     color &corpisca
   @20,MOFFSET + 03 say mnbanco   pict '999'  color &corver
   @20,MOFFSET + 12 say magencia  pict '9999' color &corver
   
   @21,MOFFSET + 06 say mobserv color &corver
   
   @04,02 say space(16)
   mcgc    = space(16)
   escolha = space(01)
   MENS = 'Quer selecionar Cliente j† cadastrado ?'
   do PEGRESP with [SN]
   if MC = 'S'
      sele CLIEN
      
      MENS = 'Seleciona por Nome de Guerra ou RazÑo Social ou Telefone ?'
      do PEGRESP with [GRT]
      DO CASE 
        CASE MC = 'R'
          SET ORDER TO X_RAZ
          MENS = 'Informe RazÑo Social ou iniciais dela'
          =mensag(MENS)
          @06, MOFFSET+00 get MRAZ pict '@!'
          READ
          if MRAZ = space(10)
             =mensag(CALPHA)
             =INKEY(0)
             loop
          endif
          if !seek(RTRIM(MRAZ))
             MENS = 'NÜo ha Cliente cadastrado com esta RazÑo Social - ENTER'
             =INKEY(0)
             loop
          endif
        CASE MC = 'G'
          SET ORDER TO X_GUE
          MENS = 'Informe Nome de Guerra ou iniciais dele'
          =mensag(MENS)
          @04, MOFFSET+17 get MMGUE pict '@!'
          READ
          if MMGUE = space(10)
            =mensag(CALPHA)
            =INKEY(0)
            loop
          endif
          if !seek(RTRIM(MMGUE))
            MENS = 'NÜo ha Cliente cadastrado com este Nome de Guerra - ENTER'
            =mensag(MENS)
            =INKEY(0)
            loop
          else
            *save screen
            *=MOSTRA(12,01,21,78)
            *BROWSE FIELDS GUE:R:H='N. Guerra',CGC:R:H='CGC/Loja', ;
            *             CID:R:H='Cidade',END:R:H='Endereáo' IN WINDOW TELA
            MCGC  = SUBS(CGC,1,16)
            MMGUE = GUE
            @04,MOFFSET+17 SAY MMGUE COLOR &CORVER
            *deactivate window tela
            *rest screen
            *MMGUE = GUE
            *@04,20 say MMGUE color &corver
          endif
        CASE MC = 'T'   
          MENS = 'Informe o telefone ou parte dele'
          =mensag(MENS)

          SET ORDER TO X_FON1
          
          @12,MOFFSET+05 get MFON1     pict '@!' 
          READ
          IF ! EMPTY(MFON1) .AND. !seek(MFON1) 
            =FoneNExiste()
            LOOP
          ENDIF

          SET ORDER TO X_FON2
          @12,MOFFSET+24 get MFON2     pict '@!'
          READ
          IF ! EMPTY(MFON2) .AND. !seek(MFON2) 
            =FoneNExiste()
            LOOP
          ENDIF

          SET ORDER TO X_CELU
          @12,MOFFSET+42 get MNUM_CELU pict '@!'
          READ
          IF ! EMPTY(MNUM_CELU) .AND. !seek(MNUM_CELU) 
            =FoneNExiste()
            LOOP
          ENDIF
          
        ENDCASE
        MCGC = SUBS(CGC,1,16)
        do while .t.      &&  SUBS(GUE,1,LEN(MMGUE)) = MMGUE
          @23,15 say 'Anterior [], Pr¢ximo [] ou Confirma [ESC] ?' color &LINTELA
          @23,24 say '[]'     color &corarq
          @23,37 say '[]'     color &corarq
          @23,53 say '[ESC]'   color &corarq
          do TELASCLI
          MCGC  = SUBS(CGC,1,16)
          ** MLOJA = SUBS(CGC,18,3)
          ** KEYCGC = MCGC+'/'+MLOJA
          MFIJU   = FIJU
          MMGUE   = GUE
          MRAZ    = RAZ
          MIDREPR = IDREPR
          MVEN    = VEN
          MOBSERV = OBSERVACAO
          FN    = inkey(0)
          if FN = 5
            skip - 1
            if bof()
               =mensag('In°cio do Arquivo - ENTER')
               =INKEY(0)
               loop
            endif
          else
            if FN = 24
               skip
               if eof()
                  =mensag('Fim do Arquivo - ENTER')
                  =INKEY(0)
                  loop
               endif
            else
               MCGC = CGC
               MFIJU = FIJU
               SET ORDER TO P_CGC
               exit
            endif
          endif
       enddo
      ESCOLHA = '*'
   endif
   if ESCOLHA = space(01)
      mens = 'Cliente: Jur°dico [J], F°sico [F], Indefinido [I], encerrar [ESC]'
      do pegresp with [JFI]
      if lastkey() = 27
         if mdel = 1
            MC = ' '
            MENS = 'Exclui os Clientes cancelados ?'
            do PEGRESP with [NS]
            if MC = 'S'
               sele CLIEN
               pack
            endif
         endif
         clos data
         return
      endif
      MFIJU = MC
   endif
   if mfiju = 'F' .and. escolha = space(01)
      @04,MOFFSET+00  get MCGC pict '999.999.999/99' VALID CHKCHAVE()
      read
      if lastkey() = 27
         loop
      endif
   endif
   if MFIJU = 'J' .AND. ESCOLHA = space(01)
      @04,MOFFSET+00 get mcgc pict '99999999/9999-99' VALID CHKCHAVE()
      read
      if lastkey() = 27
         loop
      endif
   endif
   if MFIJU = 'I' .AND. ESCOLHA = space(01)
     @04,MOFFSET+00 get MCGC pict '@!'
     read
     if lastkey() = 27
       loop
     endif
   endif
   INC = ' '
   sele CLIEN
   SET ORDER TO P_CGC
   if !seek(MCGC)
      @04,MOFFSET+17 say MMGUE       color &corver
      @04,MOFFSET+33 say mregiao     color &corver
      @04,MOFFSET+42 say mcvr        color &corver
      @04,MOFFSET+50 say mstatus     color &corver
      @04,MOFFSET+55 say mdescto1 pict '99.9' color &corver
      @04,MOFFSET+60 say mdescto2 pict '99.9' color &corver
      @04,MOFFSET+66 say mcondpg1 pict '99' color &corver
      @04,MOFFSET+69 say mcondpg2 pict '99' color &corver
      @04,MOFFSET+72 say mcondpg3 pict '99' color &corver   
      
      @06,MOFFSET+00 say mraz        color &corver
      @06,MOFFSET+49 say mcid        color &corver
      
      @08,MOFFSET+00 say mend        color &corver
      @08,MOFFSET+49 say mbai        color &corver
      
      @10,MOFFSET+00 say MCEST        color &corver
      @10,MOFFSET+03 say mcep        color &corver
      @10,MOFFSET+13 say mlocaliz    color &corver
      @10,MOFFSET+29 say mins        color &corver
      @10,MOFFSET+47 say misu        color &corver
      @10,MOFFSET+64 say mbox        color &corver
*     @10,62 say mcmu        color &corver
      @10,MOFFSET+74 say mdic        color &corver
      
      @12,MOFFSET+00 say mddd1       color &corver
      @12,MOFFSET+05 say mfon1       color &corver
      @12,MOFFSET+19 say mddd2       color &corver
      @12,MOFFSET+24 say mfon2       color &corver
      @12,MOFFSET+37 say mddd_celu   color &corver
      @12,MOFFSET+42 say mnum_celu   color &corver
      @12,MOFFSET+58 say mddd_faxc   color &corver
      @12,MOFFSET+63 say mnum_faxc   color &corver
*      @12,66 say mcon        color &corver

      @14,MOFFSET+00 say mcontato    color &corver
      @14,MOFFSET+21 say mcontato2   color &corver
      @14,MOFFSET+42 say matividade  color &corver
      @14,MOFFSET+47 say mtra        color &corver
      @14,MOFFSET+52 say MTRA_OPC    color &corver
      @14,MOFFSET+57 say mclc  pict '@R 99/99'       color &corver
      @14,MOFFSET+63 say mforgan     color &corver
      @14,MOFFSET+67 say mforbuc     color &corver            
      @14,MOFFSET+71 say mforred     color &corver
      @14,MOFFSET+75 say mforcor     color &corver            
      
      @16,MOFFSET+00 say minic       color &corver
      @16,MOFFSET+12 say mulc        color &corver
      @16,MOFFSET+23 say ulcsp       color &corver      
      @16,MOFFSET+34 say mavsp       color &corver            
      @16,MOFFSET+36 say mcre        color &corver PICT [999999]
      @16,MOFFSET+43 say mdeb        color &corver PICT [999999]
      @16,MOFFSET+57 say mconceito     color &corver
      @16,MOFFSET+61 say sconceito     color R+/B
      @16,MOFFSET+68 say maceitama pict '99.9'  color &corver
      @16,MOFFSET+73 say maceitame pict '99.9'  color &corver
      
      @18,MOFFSET+00 say mves       color &corver
      @18,MOFFSET+23 say mdtalt     color &corver
      @18,MOFFSET+35 say msigla     color &corver
      @18,MOFFSET+40 say space(30)  color &corver
      do av04101aux
      *@20,35 say mentr      color &corpisca
      *@20,56 say mfatu      color &corpisca
      *@20,74 say mcobr      color &corpisca
      @20,MOFFSET+03 say mnbanco   pict '999'  color &corver
      @20,MOFFSET+12 say magencia  pict '9999' color &corver
      @21,MOFFSET+06 say mobserv color &corver      
      MENS = TESTECAD
      do PEGRESP with [SN]
      if mc = 'N'
         loop
      else
         inc = '*'
      endif
   endif
   opcao = ''
   lAlterar = .F.
   if seek(MCGC)
*      DEFINE WINDOW CLIEN FROM 19,01 TO 22,78 color &corarq
*      MODI MEMO OBSERVACAO WINDOW CLIEN NOwait
      do TELASCLI
      mens = TESTEINC
      do PEGOPCAO with [123]
*      if seek(MCGC)
*         CLOS MEMO OBSERVACAO
*      endif
      if opcao = '1'
         loop
      endif
      if opcao = '3'
         do senha
         sele clien
         dele
         mdel = 1
         loop
      endif
      if opcao = '2'
         lAlterar = .T.
         do senha
         MENS = 'Atualiza Endereáos de Compra, Cobranáa e Entrega ?'
         do PEGRESP with [NS]
         if MC = 'S'
            do ENDERECOS
         endif
         MENS = 'Atualiza outros dados do Cliente ?'
         do PEGRESP with [SN]
         if MC = 'N'
            loop
         endif
      endif
   endif
   sele clien
   if SUBS(MCGC,1,5) = space(05)
      =mensag('C¢digo do Cliente nÑo pode ser brancos - ENTER')
      =INKEY(0)
      loop
   endif
   if empty(opcao)
      @04,MOFFSET+17 get MMGUE pict '@!' valid av04101gue(mmgue)
      read
   endif
   Sele REGIAO
   do LIMPA23
   do while .t.
      @04,MOFFSET+33 get mregiao pict [9999.99] valid (mregiao <> EmptyRegiao) .AND. chkregiao(mregiao, .T.)
      read
      if lastkey() = 27
         clos data
         return
      endif
      midregiao = REGIAO->IDREGIAO         
      exit
   enddo
   @23,01 say space(77)
   sele VEND
   cOldRepr = mcvr   
   do while .t.
      @04, MOFFSET+41 get mcvr PICT [9999.99] VALID MCVR = EmptyRepr .OR. ChkRepr(MCVR, .T.)
      read
      if lastkey() = 27
         clos data
         return
      endif
      IF MCVR <> cOldRepr .AND. lAlterar .AND. ! SenhaRepr() 
         MCVR = cOldRepr
         LOOP
      ENDIF
      MIDREPR = ExtractRepr(MCVR)
      MVEN    = ExtractVen(MCVR)  
      EXIT
   enddo
   sele STATUS
   do while .t.
      @04,MOFFSET+50 get mstatus
      read
      if mstatus <> space(1)
         if !seek(mstatus)
            =mensag('Status nÑo cadastrado - ENTER')
            loop
         endif
      endif
      exit
   enddo
   =mensag('Informe os Percentuais de Desconto oferecidos ao Cliente')
   @04,MOFFSET+55 get mdescto1 pict '99.9'
   @04,MOFFSET+60 get mdescto2 pict '99.9'
   @04,MOFFSET+66 get mcondpg1 pict '99'
   @04,MOFFSET+69 get mcondpg2 pict '99'
   @04,MOFFSET+72 get mcondpg3 pict '99'   
   read
   @23,01 say space(78)
   do while .t.
      @06,MOFFSET+00 get mraz pict '@!'
      read
      if mraz <> space (30)
         exit
      endif
      do mensag with CALPHA
   enddo
   @23,01 say space(78)
   do while .t.
      @06,MOFFSET+49  get mcid pict '@!'
      read
      if mcid <> space(20)
         exit
      endif
      do mensag with CALPHA
   enddo
   @23,01 say space(78)
   do while .t.
      @08,MOFFSET+00 get mend pict '@!'
      read
      if mend <> space (30)
         exit
      endif
      do mensag with CALPHA
   enddo
   MENSP = 'Preencher os campos a seguir sempre que poss°vel...'
   do mensag with MENSP
   @08,MOFFSET+49 get mbai pict '@!'
   read
   sele ESTADOS
   @23,01 say space(78)
   do while .t.
      @10,MOFFSET+00 get MCEST pict '!!'
      read
      if !seek(MCEST)
         do mensag with CORRIJA
      else
         exit
      endif
   enddo
   do mensag with MENSP
   @10,MOFFSET+03 get mcep
   READ

   =mensag('Informe a Localizaá∆o do Cliente do GUIA de SP')
   @10,MOFFSET+13 get mlocaliz
   READ
   =mensag('Informe InscriáÑo, C¢d. Mun. e Desconto ICM qdo Cliente Zona Franca')      
   do while .t.
      @10,MOFFSET+29 get mins
      read
      if mins <> space(13)
         exit
      endif
      do mensag with CALPHA
   enddo
   @10,MOFFSET+47 get misu
   *@10,62 get mcmu
   @10,MOFFSET+64 get mbox
   @10,MOFFSET+74 get mdic pict '9'
   read
   =mensag( MENSP )
   @12,MOFFSET+00 get mddd1
   @12,MOFFSET+05 get mfon1
   @12,MOFFSET+19 get mddd2
   @12,MOFFSET+24 get mfon2
   @12,MOFFSET+37 get mddd_celu
   @12,MOFFSET+42 get mnum_celu
   @12,MOFFSET+58 get mddd_faxc
   @12,MOFFSET+63 get mnum_faxc
   *@12,66 get mbox
   *@12,66 get mcon
   @14,MOFFSET+00 get mcontato pict '@!'
   @14,MOFFSET+21 get mcontato2 pict '@!'
   read
   Sele ATIVIDAD
   do LIMPA23
   do while .t.
      @14,MOFFSET+42 get matividade
      read
      if matividade <> space(04)
         if !seek(matividade)
            =mensag('Atividade nÑo cadastrada - ENTER')
            =INKEY(0)
            loop
         endif
      endif
      exit
   enddo
   sele TRANS
   do while .t.
      if mtra = space(04)
         SET ORDER TO TRAGUE
      endif
      @14,MOFFSET+47 get mtra pict '@!'
      read
      if lastkey() = 27
         clos data
         return
      endif

      SET ORDER TO TRANS
      if MTRA <> space(04)
         if !seek(mtra)
            =mensag('NÑo cadastrada. Corrija ou entre (ESC) para encerrar')
            =INKEY(0)
            loop
         endif
      endif
      exit
   enddo
   do while .t.
      if MTRA_OPC = space(04)
         SET ORDER TO TRAGUE
      endif
      @14,MOFFSET+52 get MTRA_OPC pict '@!'
      read
      if lastkey() = 27
         clos data
         return
      endif
      SET ORDER TO TRANS
      if MTRA_OPC <> space(04)
         if !seek(MTRA_OPC)
            =mensag('NÑo cadastrada - Corrija ou entre (ESC) para encerrar')
            =INKEY(0)
            loop
         endif
      endif
      exit
   enddo
   MENS = 'ClassificaáÑo do Cliente: A, B, C, etc'
   do mensag with MENS
   @14,MOFFSET+57 get mclc    pict '@R 99/99'
   @14,MOFFSET+63 get mforgan color &corver
   @14,MOFFSET+67 get mforbuc color &corver   
   @14,MOFFSET+71 get mforred color &corver
   @14,MOFFSET+75 get mforcor color &corver   
   
   @16,MOFFSET+00 get minic   
   @16,MOFFSET+12 get mulc    
   @16,MOFFSET+23 get mulcsp  
   @16,MOFFSET+34 get mavsp   pict '@!'  VALID mavsp $ [SN]
   @16,MOFFSET+36 get mcre    pict '999999'
   READ
   MDEB = 0.00
   @16,MOFFSET+43 say space(9)
   sele DUPL
   if seek(MCGC)
      do while CGC = MCGC
         if EMPTY(DTCANCEL) .AND. EMPTY(PAG) .AND. VENC <= DATE()
            MDEB = (MDEB + (VDP - DESC))
         endif
         skip
      enddo
   endif
   if MDEB > 0
      @16,MOFFSET+43 say MDEB pict '999999.99'  color R+/B
   endif
   do limpa23
   sele CONCEITO
   do while .t.
      @16,MOFFSET+57 get mconceito pict '@!'
      read
      if mconceito <> space(2)
         if !seek(mconceito)
            =mensag('Conceito nÑo cadastrado - ENTER')
            loop
         endif
      endif
      exit
   enddo
   @16,MOFFSET+61 get sconceito pict '!!' color R+/B
   =mensag('Informe o percentual de aceitaáÑo pelo Cliente (+/-)')
   @16,MOFFSET+68 get maceitama pict '99.9'
   @16,MOFFSET+73 get maceitame pict '99.9'
   read
*
   MENS = 'Preencher os campos a seguir sempre que poss°vel/necess†rio...'
   do mensag with MENS
*
   @18,MOFFSET+00 get mves pict '99/99/99/99/99/99'
   read
   do limpa23
   do while .t.
      mdtalt = date()
      @18,MOFFSET+23 say mdtalt color &corver
      @18,MOFFSET+35 get msigla pict '@!'
      read
      if !seek(msigla,'siglas')
         =mensag('Sigla do Funcion†rio autorizado nÑo cadastrado - ENTER')
         =INKEY(0)
         loop
      else
         @18,MOFFSET+40 say siglas->nome  color &corver
         exit
      endif
   enddo
   do while .t.
      @20,MOFFSET+03 get mnbanco   pict '999'
      @20,MOFFSET+12 get magencia  pict '9999'
      read
      if !seek(str(mnbanco,3)+str(magencia,4),'bancos')
         =mensag('Nß e/ou Agància nÑo cadstrados - ENTER')
         =INKEY(0)
         loop
      else
         exit
      endif
   enddo
   @21,MOFFSET+06 get mobserv 
   read
   MENS = DADCORR
   do PEGRESP with [SN]
   if mc = 'N' .or. empty(mc)
      limp = 'N'
      loop
   else
      limp = 'S'
   endif
   sele CLIEN
   if !seek(mcgc)
      opcao = 'I'
      appe blan
   endif
*   =mensag('Sair sem alterar (ESC), Gravar/alterar conte£do (Crtl+W)')
*   DEFINE WINDOW CLIEN FROM 19,01 TO 22,78 color &corarq
*   MODI MEMO OBSERVACAO WINDOW CLIEN SAVE
*
   repl cgc  with mcgc
   repl fiju with mfiju
   repl raz  with mraz
   repl end with mend
   repl descto1   with mdescto1
   repl descto2 with mdescto2
   repl cid with mcid
   repl bai with mbai
   repl est with MCEST
   repl cep with mcep
   REPL LOCALIZ WITH MLOCALIZ
   repl ins with mins
   repl ddd1 with mddd1
   repl fon1 with mfon1
   repl ddd2 with mddd2
   repl fon2 with mfon2
   repl contato with mcontato
   repl contato2 with mcontato2
   repl box with mbox
   repl cre with mcre
   repl deb with mdeb
   repl clc with mclc
   repl forgan with mforgan
   repl forbuc with mforbuc
   repl forred with mforred
   repl forcor with mforcor
   repl tra with mtra
   repl tra_opc with MTRA_OPC
   repl ven    with MVEN
   repl idrepr with MIDREPR
   repl gue with MMGUE
   repl dic with mdic
   repl isu with misu
   repl ddd_celu with mddd_celu
   repl num_celu with mnum_celu
   repl ddd_faxc with mddd_faxc
   repl num_faxc with mnum_faxc
   repl ves with mves
   repl alt with '*'
   repl inic with minic
   repl ulc with mulc
   repl ulcsp with mulcsp
   REPL AVSP  WITH MAVSP = 'S'
   repl atividade with matividade
   repl conceito with mconceito
   repl conceito2 with sconceito
   repl aceitama  with maceitama
   repl aceitame  with maceitame
   REPL IDREGIAO  WITH MIDREGIAO
   repl regiao    with SUBSTR(mregiao,1,4)
   repl estatus   with mstatus
   repl dtalt     with mdtalt
   repl sigla     with msigla
   repl nbanco    with mnbanco
   repl agencia   with magencia
   repl condpg1   with mcondpg1
   repl condpg2   with mcondpg2
   repl condpg3   with mcondpg3
   repl observacao with mobserv
 * repl con with mcon
 * cmu with mcmu

   if OPCAO = 'I'
      MENS = 'Atualiza Endereáos de Compra, Cobranáa e Entrega ?'
      do PEGRESP with [SN]
      if MC = 'S'
         do ENDERECOS
      endif
   endif
   if limp = 'S'
      do limpar
   endif
enddo
close data
return

*************************************************************
procedure enderecos
  mens = 'Atualiza Endereáo de Compra ?'
  do PEGRESP with [SN]
  if MC = 'S'
    @23,01 say space(77)
    SAVE SCRE TO TELA
    do AV041003
    REST SCRE FROM TELA
  endif
  MENS = 'Atualiza Endereáo de Cobranáa ?'
  do PEGRESP with [SN]
  if MC = 'S'
    @23,01 say space(77)
    SAVE SCRE TO TELA
    do AV041002
    REST SCRE FROM TELA
  endif
  MENS = 'Atualiza Endereáo de Entrega ?'
  do PEGRESP with [SN]
  if MC = 'S'
    @23,01 say space(77)
    SAVE SCRE TO TELA
    do AV041001
    REST SCRE FROM TELA
  endif
return

*******************************************************************
PROCEDURE TELASCLI
  @04,MOFFSET+00 say SUBS(CGC,1,16)  color &corver
  MCGC = SUBS(CGC,1,16)
  @04,MOFFSET+17 say gue       color &corver
  MMGUE = gue
  mregiao  = FmtRegiao(regiao, idregiao)
  midregiao = idregiao
  @04,MOFFSET+33 say mregiao    color &corver  
  midrepr = idrepr
  mven = ven
  mcvr = FMTREPR(ven,idrepr)
  @04,MOFFSET+41 say MCVR  color &corver
  @04,MOFFSET+50 say estatus color &corver
  mstatus = estatus
  @04,MOFFSET+55 say descto1 pict '99.9' color &corver
  mdescto1 = descto1
  @04,MOFFSET+60 say descto2 pict '99.9' color &corver
  mdescto2 = descto2
  @04,MOFFSET+66 say condpg1 pict '99' color &corver
  mcondpg1 = condpg1
  @04,MOFFSET+69 say condpg2 pict '99' color &corver
  mcondpg2 = condpg2
  @04,MOFFSET+72 say condpg3 pict '99' color &corver   
  mcondpg3 = condpg3
  @06,MOFFSET+00 say raz color &corver
  mraz = raz
  @06,MOFFSET+49 say cid color &corver
  mcid = cid
  @08,MOFFSET+00 say end color &corver
  mend = end
  @08,MOFFSET+49 say bai  color &corver
  mbai = bai
  @10,MOFFSET+00 say est color &corver
  MCEST = est
  @10,MOFFSET+03 say cep  color &corver
  mcep = cep
  @10,MOFFSET+13 say localiz  color &corver
  mlocaliz = localiz
  @10,MOFFSET+29 say ins  color &corver
  mins = ins
  @10,MOFFSET+47 say isu  color &corver
  misu = isu
  *@10,62 say cmu  color &corver
  *mcmu = cmu
  @10,MOFFSET+64 say box  color &corver
  mbox = box
  @10,MOFFSET+74 say dic  color &corver
  mdic = dic
  @12,MOFFSET+00 say ddd1  color &corver
  mddd1 = ddd1
  @12,MOFFSET+05 say fon1  color &corver
  mfon1 = fon1
  @12,MOFFSET+19 say ddd2  color &corver
  mddd2 = ddd2
  @12,MOFFSET+24 say fon2  color &corver
  mfon2 = fon2
  @12,MOFFSET+37 say ddd_celu color &corver
  mddd_celu = ddd_celu
  @12,MOFFSET+42 say num_celu color &corver
  mnum_celu = num_celu
  @12,MOFFSET+58 say ddd_faxc  color &corver
  mddd_faxc = ddd_faxc
  @12,MOFFSET+63 say num_faxc  color &corver
  mnum_faxc = num_faxc
  *@12,66 say con  color &corver
  *mcon = con
  @14,MOFFSET+00 say contato  color &corver
  mcontato = contato
  @14,MOFFSET+21 say contato2  color &corver
  mcontato2 = contato2
  @14,MOFFSET+42 say atividade color &corver
  matividade = atividade
  @14,MOFFSET+47 say tra  color &corver
  mtra = tra
  @14,MOFFSET+52 say tra_opc  color &corver
  MTRA_OPC = tra_opc
  @14,MOFFSET+57 say clc pict '@R 99/99'  color &corver
  mclc = clc
  @14,MOFFSET+63 say forgan color &corver
  mforgan = forgan
  @14,MOFFSET+67 say forbuc color &corver
  mforbuc = forbuc
  @14,MOFFSET+71 say forred color &corver
  mforred = forred
  @14,MOFFSET+75 say forcor color &corver
  mforcor = forcor
  @16,MOFFSET+00 say inic  color &corver
  minic = inic
  @16,MOFFSET+12 say ulc  color &corver
  mulc = ulc
  @16,MOFFSET+23 say ulcsp  color &corver
  mulcsp = ulcsp
  @16,MOFFSET+34 say avsp  color &corver
  mavsp = IIF( avsp, 'S', 'N' )
  @16,MOFFSET+36 say str(cre,6)  color &corver
  mcre = cre
  MCONCEITO = CONCEITO
  MDEB = 0.00
  @16,MOFFSET+43 say space(6)
  sele DUPL
  if seek(MCGC)
    do while CGC = MCGC
      if EMPTY(DTCANCEL) .AND. EMPTY(PAG) .AND. VENC <= DATE()
         MDEB = (MDEB + (VDP - DESC))
      endif
      skip
    enddo
  endif
  if MDEB > 0
    @16,MOFFSET+43 say MDEB pict '999999.99'  color R+/B
  endif
  mcolorir = ""
  if MCONCEITO <> space(02)
    sele CONCEITO
    if seek(MCONCEITO)
      mcolorir = colorIR
    endif
  endif
  if mcolorir = '*'
    @16,MOFFSET+57 say MCONCEITO color R+/B
  else
    @16,MOFFSET+57 say mconceito  color &corver
  endif
  sele CLIEN
  IF EMPTY(conceito2)
    mcor = corver
  ELSE
    mcor = 'R+/B'
  ENDIF  
  @16,MOFFSET+61 say conceito2 color &mcor
  sconceito = conceito2
  @16,MOFFSET+68 say aceitama pict '99.9'  color &corver
  maceitama = aceitama
  @16,MOFFSET+73 say aceitame pict '99.9'  color &corver
  maceitame = aceitame
  @18,MOFFSET+00 say ves  color &corver
  mves = ves
  @18,MOFFSET+23 say dtalt color &corver
  mdtalt = dtalt
  @18,MOFFSET+35 say sigla color &corver
  msigla = sigla
  if msigla <> space(04)
    if seek(msigla,'siglas')
      @18,MOFFSET+40 say siglas->nome color &corver
    endif
  else
    @18,MOFFSET+40 say space(30)
  endif
  @20,MOFFSET+03 say nbanco   pict '999'  color &corver
  mnbanco = nbanco
  @20,MOFFSET+12 say agencia  pict '9999' color &corver
  magencia = agencia
  @21,MOFFSET+06 say observacao color &corver
  mobserv = observacao
  do av04101aux
  sele CLIEN
return

***********************************************************
procedure LIMPAR                    && limpa as variaveis
  MMGUE  = space(15)
  tcgc  = space(16)
  mcid  = space(25)
  mbai  = space(20)
  mraz  = space(45)
  mend  = space(45)
  MCEST  = space(02)
  mcep  = space(09)
  mbox  = space(05)
  mins  = space(17)
  mddd1 = space(04)
  mfon1 = space(09)
  mddd2 = space(04)
  mfon2 = space(09)
  mves  = space(17)
  *mcon  = space(10)
  stor space(04) to mddd_faxc, mddd_celu
  stor space(09) to mnum_faxc, mnum_celu
  mcontato = space(20)
  mcontato2 = space(20)
  matividade = space(04)
  mregiao    = EmptyRegiao
  stor 0 to mcre, mdeb, acha
  stor space(16) to mmat
  mconceito = space(02)
  sconceito = space(02)
  stor space(01) to mfiju, mdic, mstatus
  STOR CTOD('') TO MINIC, mulc, mulcsp
  *stor space(08) to mcmu
  mtra = space(04)
  MTRA_OPC = space(4)
  stor space(04) to mclc
  store space(3) to mforred, mforgan, mforbuc, mforcor
  mcvr = EmptyRepr
  mloja = space(03)
  MMGUE = space(15)
  misu = space(13)
  mtip = '*'
  mcgc = space(16)
  mloja = space(03)
  maceitama = 0.0
  maceitame = 0.0
  mdtalt    = CTOD('')
  msigla   = space(04)
  mnbanco = 0
  magencia = agencia
  mdescto1 = 0.0
  mdescto2 = 0.0
  mcondpg1 = 0
  mcondpg2 = 0
  mcondpg3 = 0  
return

********************
procedure av04101aux
********************

  stor space(01) to mentr, mfatu, mcobr
  varea = select()
  sele DUPL
  seek clien->cgc
  do while !eof() .and. cgc = clien->cgc
    if empty(dtcancel)
      if empty(entrega)
         mentr = 'X'
       endif
       if !empty(entrega) .and. empty(pag)
         mcobr = 'X'
       endif
     endif
     skip
  enddo
  sele PEDID
  seek clien->cgc
  if found()
     do while !eof() .and. cgc = clien->cgc
       if sal # 0
         mfatu = 'X'
         exit
       endif
       skip
     enddo
  endif
  @20,MOFFSET+33 say mentr     color &corpisca
  @20,MOFFSET+54 say mfatu     color &corpisca
  @20,MOFFSET+72 say mcobr     color &corpisca
  sele (varea)
return

*******************
function av04101gue
*******************
  parameters xgue
  private xorde, xreto, xreto
  save screen
  xreto = .t.
  if empty(xgue)
     xreto = .f.
     =mensag('NOME INVALIDO   -   Qualquer tecla para continuar')
     =INKEY(0)
  else
     xorde = order()
     set order to X_GUE
     if seek(xgue)
        xreto = .f.
        =mensag('ESTE NOME DE GUERRA Jè EXISTE   -   Qualquer tecla para continuar')
        =INKEY(0)
     endif
     set order to xorde
  endif
  restore screen
return xreto

*********************
PROCEDURE FONENEXISTE
*********************
   MENS = 'NÜo ha Cliente cadastrado com este telefone - ENTER'
   =mensag(MENS)
   =INKEY(0)
RETURN

******************
PROCEDURE CHKCHAVE
******************
  PRIVATE Retorno, nDbf
  
  Retorno = .F.
  nDbf = SELECT()
  DO CASE 
    CASE MFIJU = 'J'
      Retorno = CHKCGC()   
    CASE MFIJU = 'F'
      Retorno = CHKCPF()
    CASE MFIJU = 'I'
      Retorno = MCGC <> space(16)
      if ! Retorno
        =mensag('C¢digo do Cliente nÑo pode ser brancos - ENTER')
      endif
  ENDCASE

  IF Retorno = .F.
    =INKEY(0)
  ENDIF  
  SELECT( nDbf )
  
RETURN Retorno

***************
FUNCTION CHKCGC
***************
  PRIVATE Indice, lValido
  
  lValido = .F.
  if MCGC = space(08)
    =mensag('Cliente Pessoa Jur°dica com CGC exigido')
  ELSE  
    * Se o CGC ja estiver cadastrado
    SELE CLIEN
    Indice = ORDER()
    SET ORDER TO P_CGC
    IF SEEK( MCGC )
  *    =mensag('CGC j† esta sendo utilizado. ENTER para entrar novamente')
      lValido = .T.
    ELSE
      lValido = FCGC( MCGC )  
      IF .NOT. lValido
        =MENSAG( MENSCGC )
        ?? CHR(7)
      ENDIF  
    ENDIF
    SET ORDER TO Indice
  ENDIF
 
RETURN lValido

***************
FUNCTION CHKCPF
***************
  PRIVATE Indice 
  
  IF MCGC = space(03) 
    =mensag('Cliente Pessoa F°sica com CPF exigido - ENTER')
    Return .F.
  ENDIF
     
  SELECT CLIEN
  Indice = ORDER()
  SET ORDER TO P_CGC
  
  IF SEEK( MCGC ) 
    =mensag('CPF j† esta sendo utilizado. ENTER para entrar novamente')
    SET ORDER TO Indice
    RETURN .F.
  ENDIF
  SET ORDER TO Indice
    
  CPFT = SUBS(MCGC,1,3)+SUBS(MCGC,5,3)+SUBS(MCGC,9,3)+SUBS(MCGC,13,2)
  do FCPF with CPFT
  if MC = 'N'
    =mensag('CPF do Cliente incorreto. ENTER para entrar novamente')
    RETURN .F.
  endif
  
RETURN .T.


*=================
FUNCTION SENHAREPR
*=================
  PRIVATE lValida, cSenha, nCont

  lValida = .F.
  nCont = 3
  DO WHILE nCont > 0 .AND. ! lValida 
    =MENSAG('')
    cSenha = SPACE(10)  
    @23, 01 SAY 'SENHA:' COLOR 'W+/R'
    @23, 08 GET cSenha COLOR W+/B,X
    READ
  
    IF LASTKEY() = 27
      EXIT
    ENDIF  
    
    lValida = ALLTRIM(cSenha) = '2451' 
    nCont = nCont - 1    
    
    IF ! lValida 
      IF nCont = 1 
        =MENSAG( 'Senha Invalida, ultima tentativa !!!' )
      ELSE  
        =MENSAG( 'Senha Invalida !!!' )
      ENDIF  
      =INKEY(0)
    ENDIF   

  ENDDO
  =MENSAG('')

RETURN lValida
