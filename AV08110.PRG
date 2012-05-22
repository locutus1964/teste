* AV08100 - CALCULO DO VALOR E CRIACAO DO ARQUIVO DE DUPLICATAS
* SISTEMA ADMINISTRACAO DE VENDAS -27/01/87
* ADAO ELI PEREIRA - 9/6/90
*
MENS = 'Impressora preparada para imprimir Duplicatas ?'
DO PEGRESP WITH [SN]
IF MC = 'N' .or. empty(mc)
   RETURN
ENDIF
stor 0 to MNDUPI, MNDUPF, vano, vano2
CLOS DATA
SELE 0
USE CLI_COBR ORDER P_CGC
SELE 0
USE CLIEN ORDER P_CGC
SELE 0
USE DUPL order xdupano
=NOMEMOD( 'AV08110')
? JANELA(14,35,20,76)
? BORDA(14,35,20,76)
DO WHILE .T.
   ? MENSAG(ECODIGO)
   @16,38 say 'N£mero da primeira duplicata ' GET MNDUPI PICT '999999'
   @17,38 say 'Ano da emiss„o               ' get vano   pict '9999'
   READ
   IF LASTKEY() = 27
      CLOS DATA
      RETURN
   ENDIF
   IF MNDUPI = 0
      ? MENSAG(CNUMER)
      =INKEY(0)
      LOOP
   ENDIF
   SEEK STR(MNDUPI,6) + '1' + STR(vano,4)
   IF EOF() .OR. BOF()
      ? MENSAG(CORRIJA)
      =INKEY(0)
      LOOP
   ENDIF
   @ 18,38 SAY 'N£mero da £ltima duplicata   ' GET MNDUPF PICT '999999'
   @ 19,38 say 'Ano de amiss„o               ' get vano2  pict '9999'
   READ
   IF LASTKEY() = 27
      CLOS DATA
      RETURN
   ENDIF
   IF MNDUPF < MNDUPI
      ? MENSAG('N£mero da £ltima duplicata n„o pode ser < que da primeira - ENTER')
      =INKEY(0)
      LOOP
   ENDIF
   IF !SEEK(STR(MNDUPF,6)+'1'+str(vano,4))
      ? MENSAG(CORRIJA)
      =INKEY(0)
      LOOP
   else

      do while !eof() .and. ndup = mndupf
         if YEAR(emis) = vano
            vparcela = str(pdup,1)
         endif
         skip
      enddo

   ENDIF
   mens = DADCORR
   DO PEGRESP WITH [SN]
   if mc = 'S'
      exit
   else
      loop
   endif
ENDDO
*
? MENSAG('Imprimindo as duplicatas... Aguarde')

SET DEVI TO PRINT
SET PRINT ON
??? CHR(27)+CHR(64)+CHR(27)+CHR(67)+CHR(31)
SEEK STR(MNDUPI,6) + '1' + str(vano,4)
DO WHILE str(NDUP,6)+'1'+str(vano,4) <= str(MNDUPF,6)+vparcela+str(vano2,4) .AND. !EOF()
   STOR CGC TO M.CGC
   STOR NDUP TO M.NDUP
   STOR PDUP TO M.PDUP
   STOR VDP  TO M.VDP
   STOR EMIS TO M.EMIS
   STOR VENC TO M.VENC
   STOR NBANCO TO M.NBANCO
   STOR AGENCIA TO M.AGENCIA
   STOR DTCANCEL TO MDTCANCEL
   SELE CLIEN
   IF SEEK(M.CGC)
      STOR RAZ TO M.RAZ
      STOR END TO M.END
      STOR CEP TO M.CEP
      STOR CID TO M.CID
      STOR EST TO M.EST
      STOR INS TO M.INS
      SELE CLI_COBR
      IF SEEK(M.CGC)
         endcob = TRIM(ENDC)+', '+TRIM(CIDC)+', '+ESTC+', '+CEPC
      ELSE
         endcob = SPACE(60)
      ENDIF
   ENDIF
   **------------------  IMPRESSAO DA DUPLICATA -----------------------------
   IF !EMPTY(MDTCANCEL)
      @ 08,15 SAY 'D U P L I C A T A   C A N C E L A D A   E M ' + dtoc(MDTCANCEL)
   ENDIF
   @ 10,60 SAY M.EMIS
   @ 14,17 SAY SUBS(STR((1000000 + M.NDUP),7),2,6)
   @ 14,30 SAY M.VDP PICT '99,999,999,999.99'
   @ 14,50 SAY SUBS(STR((1000000 + M.NDUP),7),2,6)
   @ 14,56 SAY '/'
   @ 14,57 SAY STR(M.PDUP,1)
   @ 14,66 SAY M.VENC
   @ 17,10 SAY M.RAZ
   @ 19,10 SAY M.END
   @ 19,65 SAY M.CEP
   @ 20,10 SAY M.CID
   @ 20,65 SAY M.EST

   IF endcob <> SPACE(20)
      @ 24,10 SAY endcob
   ENDIF
   @ 25,10 SAY M.CGC
   @ 25,50 SAY M.INS


   VREXT = SPACE(60)
   ? EXTENSOP()
   AUX = SUBSTR(VREXT,1,54)
   IF LEN(RTRIM(SUBSTR(VREXT,55,10))) > 0
      A = 54
      DO WHILE A > 1
         IF SUBSTR(AUX,A,1) = " " .OR. SUBSTR(AUX,A,1) = ","
            A = 0
         ELSE
            AUX = SUBSTR(AUX,1,A-1)
         ENDIF
         A = A - 1
      ENDDO
   ENDIF
   @ PROW()+2,10 SAY AUX
   A = LEN( RTRIM( SUBSTR(VREXT,LEN(AUX)+1,62 ) ) )
   IF A < 1
      @ PROW(),PCOL()+1 SAY REPLICATE("*",54-LEN(AUX))
   ENDIF
   @ PROW()+2,02 SAY RTRIM(SUBS(VREXT,LEN(AUX)+1,62))+" "+REPLICATE("*",61-A)

   EJECT
   SELE DUPL
   SKIP
ENDDO

??? CHR(27)+CHR(64)
SET PRINT OFF
SET DEVI TO SCRE
CLOSE DATABASES
RETURN
