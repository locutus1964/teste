* AV03100 -  ATUALIZACAO DO CADASTRO DE VENDEDORES/REPRESENTANTES - 20/11/86
* SISTEMA DE ADMINISTRACAO DE VENDAS
* ADAO ELI PEREIRA - 12-04-90
*
Sele 0
  use ESTADOS index ESTADOS
Sele 0
  use VEND index VEND
*
? cabtela(0,0,24,79)
*
MDEL = 0
MENS = 'Atualiza‡„o de Vendedores/Representantes'
? MENSAGR(MENS)
@ 01,POS SAY MENS
@ 05,05 say 'C¢digo Vend/Repr'
@ 05,45 say 'F¡s (F)/Jur¡d (J)'
@ 07,05 say 'CGC Representante'
@ 07,45 say 'CPF Vendedor'
@ 09,05 say 'Nome/Raz„o Social'
@ 11,05 say 'Endere‡o'
@ 13,05 say 'Cidade'
@ 13,50 say 'Estado'
@ 13,63 say 'CEP'
@ 15,05 say 'Telefone (DDD)'
@ 15,50 say 'N£mero'
@ 17,05 say 'Comiss„o'
@ 17,50 say 'Qtde Listas de Pre‡os'
@ 19,05 say 'Area de a‡„o'
@ 19,50 say 'Conta'
mcvr = space(04)
*
do while .T.
*
mlis  = 0
mfiju = space(01)
mcgc  = space(16)
mcpf  = space(14)
mraz  = space(30)
mend  = space(30)
mcid  = space(20)
mare  = space(20)
mcest  = space(02)
mcep  = space(09)
mddd  = space(04)
mfon  = space(08)
mcon  = space(10)
mcom  = 0.00
*
? MENSAG(ECODIGO)

@ 5,25  GET MCVR PICT '@!'
  read
  
  if lastkey() = 27
    if mdel = 1
      DO MENSAG WITH  'Houve exclus„o de Vendedores. Compactando Cadastro, aguarde...'
      sele vend
      pack
    endif
      close databases
      clear
      return
  endif
*
sele vend
inc = ' '
seek MCVR
  if (eof() .or. bof())
 
    @ 05,63 say mfiju COLOR &CORVER
    @ 07,25 say mcgc  COLOR &CORVER
    @ 07,63 say mcpf  COLOR &CORVER
    @ 09,25 say mraz  COLOR &CORVER
    @ 11,25 say mend  COLOR &CORVER
    @ 13,25 say mcid  COLOR &CORVER
    @ 13,58 say mcest COLOR &CORVER
    @ 13,68 say mcep  COLOR &CORVER
    @ 15,25 say mddd  COLOR &CORVER
    @ 15,59 say mfon  COLOR &CORVER
    @ 17,25 say str(mcom,5,2) COLOR &CORVER
    @ 17,75 say str(mlis,2)   COLOR &CORVER
    @ 19,25 say mare  COLOR &CORVER
    @ 19,67 say mcon  COLOR &CORVER
  
      MENS = TESTECAD
      DO PEGRESP WITH [SN]
          if mc = 'N'
            loop
          else
            inc = '*'
          endif
  endif
*
IF !(EOF() .OR. BOF())
@ 5,63  say fiju COLOR &CORVER
  mfiju = fiju
@ 7,25  say cgc  COLOR &CORVER
  mcgc  = cgc
@ 7,63  say cpf  COLOR &CORVER
  mcpf  = cpf
@ 9,25  say raz  COLOR &CORVER
  mraz  = raz
@ 11,25  say end COLOR &CORVER
  mend  = end
@ 13,25  say cid COLOR &CORVER
  mcid  = cid
@ 13,58  say est COLOR &CORVER
  mcest  = est
@ 13,68  say cep COLOR &CORVER
  mcep  = cep
@ 15,25  say ddd COLOR &CORVER
  mddd  = ddd
@ 15,59  say fon COLOR &CORVER
  mfon  = fon
@ 17,25 say str(com,5,2)  COLOR &CORVER
  mcom  = com
@ 17,75 say str(lis,2)    COLOR &CORVER
  mlis  = lis
@ 19,25 say are   COLOR &CORVER
  mare  = are
@ 19,67 say con   COLOR &CORVER
  mcon  = con
*
  MENS = TESTEINC
  DO PEGOPCAO WITH [123]
      if opcao = '1'
        loop
      endif
        if opcao = '3'
          delete
          MDEL = 1
          loop
        endif
ENDIF
*
? MENSAG('Aceitos: F¡sico (F) ou Jur¡dico (J)')
@ 5,63  get mfiju pict '!' VALID(MFIJU$'FJ')
  read
*
  if mfiju = 'F'
    mcgc = space(16)
      @ 7,25 say mcgc
      DO MENSAG WITH 'CPF preenchido para Vendedor F¡sico - (ESC) cancela'
        do while .t.
          @ 7,63  get mcpf pict '999.999.999/99'
            read
              if lastkey() = 27
                clos data
                return
              endif
              if mcpf = space (3)
                loop
              endif
                CPFT = SUBS(MCPF,1,3)+SUBS(MCPF,5,3)+SUBS(MCPF,9,3)+SUBS(MCPF,13,2)
              * ------------------------------------------------------------------
                DO FCPF WITH CPFT                
              * ------------------------------------------------------------------
          if mc = 'N'
            DO MENSAG WITH 'CPF incorreto. Entre novamente'
            =INKEY(0)
            loop
          else
            exit
          endif
        enddo
   else
     mcpf = space(14)
     DO MENSAG WITH 'CGC preenchido para Vendedor Jur¡dico - (ESC) cancela'
       @ 7,63 say mcpf
         do while .t.
           @ 7,25  get mcgc pict '99999999/9999-99'
             read
               if lastkey() = 27
                 clos data
                 return
               endif  
               if mcgc = space (8)
                 loop
               endif
                 CGCT = SUBS(MCGC,1,8)+SUBS(MCGC,10,4)+SUBS(MCGC,15,2)
               * ----------------------------------------------------------
                 DO FCGC WITH CGCT
               * ----------------------------------------------------------
           if mc = 'N'
             DO MENSAG WITH 'CGC do Vendedor incorreto. Entre novamente'
           else
             exit
           endif
         enddo
   endif
*
   @ 23,01 say space(78)
   do while .t.
     @ 9,25  get mraz pict '@!'
       read
         if mraz = space (30)
           do mensag with calpha
           loop
         endif
     exit
   enddo
*
   @ 23,01 say space(78)
   do while .t.
     @ 11,25  get mend pict '@!'
       read
         if mend = space (20)
           loop
         endif
       exit
   enddo
*
   @ 23,01 say space(78)
   do while .t.
     @ 13,25  get mcid pict '@!'
       read
         if mcid = space (20)
           do mensag with calpha
           loop
         endif
       exit
   enddo
*
 sele estados

   @ 23,01 say space(78)
   do while .t.
     @ 13,58  get mcest pict '!!'
       read
       
         if mcest = space (2)
           do mensag with calpha
           loop
         endif
             seek mcest
               if (eof() .or. bof())
                 DO MENSAG WITH CORRIJA
               else
                 exit
               endif
   enddo
*
   DO MENSAG WITH 'Campos opcionais. Informe, sempre que poss¡vel:'
*
   @ 13,68 get mcep pict '@!'
*
   @ 15,25 get mddd pict '@!'
*
   @ 15,59 get mfon pict '@!'
*
   @ 17,25 get mcom pict '99.99'
*
   @ 17,75 get mlis pict '99'
*
   @ 19,25 get mare
*
   @ 19,67 get mcon
     read
*
sele vend

  MENS = DADCORR
  DO PEGRESP WITH [SN]
      if mc = 'N'
        loop
      endif
*
if inc = '*'
   appe blank
endif
*
repl cvr with mcvr,fiju with mfiju,cgc with mcgc,cpf with mcpf
repl raz with mraz,end with mend,cid with mcid,est with mcest,cep with mcep
repl ddd with mddd,fon with mfon,com with mcom,lis with mlis,are with mare
repl con with mcon
*
enddo
RETURN
