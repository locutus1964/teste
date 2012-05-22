*** ALTFOLHA - ALTERA O NUMERO DA ULTIMA FOLHA DO REGISTRO DE SAIDAS ***

   ? JANELA(16,45,20,77)
   ? BORDA(16,45,20,77)
   USE PARAM

   MENS = 'Ultima Folha Impressa do Registro de Saidas = '+PADL(UFOLHA,4,'0')
   ? mensag(mens)
   mfolha = 0
   
   mens = 'N£mero Ultima Folha '
   @ 18,48 say mens get mfolha pict '9999'
   read

   IF LASTKEY() = 27
      CLOSE DATA
      RETURN
   ENDIF

   MC = ' '
   mens=DADCORR
   ? mensag(mens)
   @ 23,pos say mens get mc pict'!' valid(mc$'SN')
   read

   IF MC = 'N'
      CLOSE DATA
      RETURN
   ENDIF

   REPL UFOLHA WITH MFOLHA
   CLOSE DATA
   RETURN

