* AV08300 - CONSULTA SITUACAO DE DUPLICATAS POR CLIENTE
* SISTEMA ADMINISTRACAO DE VENDAS - ADAO ELI PEREIRA - 02/03/87
*
DO ABREVGA
=CABTELA45(0,0,49,79)
CLOS DATA
SELE 0
USE CLIEN ORDER X_GUE
SELE 0
USE DUPL  ORDER XDUPCGC
*
MGUE = SPACE(15)
MENS = 'Consulta Duplicatas pelo Nome de Guerra do Cliente'
@ 1,MENSAGR(MENS) SAY MENS
@ 03,03 SAY 'Nome Guerra'
@06,01 say 'Dupl/Par   Valor Duplic.   Venct§   Pagt§               Observa‡”es'
DO WHILE .T.
   SELE CLIEN
   =mensag(ECODIGO)
   @ 07,01 clear to 46,78
   @ 03,15 get MGUE PICT '@!'
   read

   IF LASTKEY() = 27
      CLOS DATA
      DO FECHAVGA
      RETURN
   ENDIF

   if empty(MGUE)
      =MENSAG(CALPHAN)
      loop
   endif

   SEEK MGUE
   @ 03,32 SAY RAZ
   STOR (CRE * 100) TO MCRE
   @ 04,03 SAY 'Cr‚d.'
   @ 04,09 SAY MCRE PICT '9,999,999,999'
   @ 04,25 SAY 'D‚b.'
   @ 04,46 SAY 'Ult.Compra ' + dtoc(CLIEN->ULC)
*   @ 04,67 SAY 'Tipo '+TIP
   MCGC = CGC
   stor 8 to lin
   sele DUPL
   SEEK MCGC
   IF (eof() .or. bof())
      =MENSAG49('N„o existem duplicatas para o Cliente - ENTER')
      =INKEY(0)
      loop
   ENDIF
   TOTDEB = 0.00
   do while cgc = mcgc .and. !eof()
      @ lin,01      say subs(str(1000000 + ndup,7),2,6) +'/'
      @ lin,col()   say pdup pict '9'
      @ lin,col()+1 say vdp pict '9,999,999,999.99'
      if EMPTY(PAG) .and. empty(dtcancel)
         stor (vdp + totdeb) to totdeb
      endif
      @ lin,col()+1 say venc
      @ lin,col()+1 say pag
      IF ! EMPTY(DTCANCEL)
         @ lin,col()+1 SAY 'Dupl. cancel. em '+ DTOC(DTCANCEL)
      ELSE
         @ lin,col()+1 say subs(obs,1,30)
      ENDIF
      stor lin + 1 to lin
      if lin > 46
         =MENSAG49('Pressione qualquer tecla para continuar ou ESC para abortar')
         =INKEY(0)
         if lastkey() = 27
            exit
         endif
         @ 08,01 CLEAR TO 46,78
         STOR 8 TO LIN
      ENDIF
      skip
      if cgc <> mcgc
         @ 04,30 say totdeb pict '9999,999,999.99'
         =MENSAG49('Fim do Cliente. ENTER para continuar')
         =INKEY(0)
         LOOP
      endif
   enddo
enddo
return

