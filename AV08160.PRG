* AV08500 - REIMPRESSAO DE DUPLICATAS
* SISTEMA ADMINISTRACAO DE VENDAS
* ADAO ELI PEREIRA - 28/02/87
*
MENS = 'Impressora preparada para imprimir Duplicatas ?'
DO PEGRESP WITH [SN]
IF MC = 'N'
   RETURN
ENDIF

PUBLIC MVDP,VLR,VREXT
*
SELE C
USE CLIEN ORDER P_CGC
SELE A
USE DUPL ORDER xdupano
*
? CABTELA(0,0,24,79)
*
MENS = 'Reimpress„o de Duplicatas'
? MENSAGR(MENS)
@ 01,POS SAY MENS
@  6,10 SAY 'N£mero da Duplicata/Parcela/Ano'
@  9,10 SAY 'Valor da NF/Duplicata'
@ 12,10 SAY 'Emiss„o/Vencimento (DD/MM/AAAA)'
@ 15,10 say 'CGC do Cliente'
*
STOR  0  TO MDEL,TPAR
STOR 0.00 TO MVAL
*
dO WHILE .T.
   STOR 0 TO MNDUP
   stor 0 to vano
   STOR 0 TO MPDUP
   STOR 0 TO MNNF
   STOR SPACE(8)  TO MPAG,ATE
   MVENC = CTOD('')
   STOR 0.00      TO MVDP
   ? MENSAG(ECODIGO)
   @ 6,54 get MNDUP PICT '999999'
   @ 6,62 get MPDUP PICT '9'
   @ 6,64 get vano pict '9999'
   READ
   IF LASTKEY() = 27
      CLOSE DATA
      RETURN
   ENDIF
   SEEK STR(MNDUP,6) + STR(MPDUP,1) + str(vano,4)
   IF (EOF() .OR. BOF())
      ? MENSAG('Duplicata/Parcela n„o encontrada - ENTER')
      =INKEY(0)
      LOOP
   ENDIF
*
   @ 09,51 SAY STR(VDP,12,2)
   STOR VDP TO MVDP
   @ 12,55 SAY VENC
   STOR VENC TO MVENC
   @ 15,43 SAY CGC
   STOR CGC TO MCGC
   @ 12,43 SAY EMIS
   MDTCANCEL = DTCANCEL
*
   MENS =  'Reimprime a Duplicata ?'
   DO PEGRESP WITH [SN]
   MATU = MC
*
   IF MATU = 'N'
      SELE A
      LOOP
   ENDIF
*
   @ 09,51 GET MVDP PICT '999999999.99'
   @ 12,55 GET MVENC 
   @ 15,43 GET MCGC
   READ
*
   MENS = DADCORR
   DO PEGRESP WITH [SN]
   IF MC = 'N'
      LOOP
   ENDIF
*
   SELE A
*
   REPL VDP WITH MVDP,;
        VENC WITH MVENC
*
   SET PRINT ON
   ?? CHR(27)+CHR(64)+CHR(27)+CHR(67)+CHR(31)
   SET DEVI TO PRINT
*
* IMPRESSAO DA DUPLICATA
*
   IF ! EMPTY(DTCANCEL)
      @ 06,15 SAY 'D U P L I C A T A   C A N C E L A D A   E M '+DTCANCEL
   ENDIF

   @ 8,60 SAY 'RODOVIARIO'
   @ 10,63 SAY EMIS
*
*    @ 14,07 SAY TOTNF PICT '999,999,999.99'
*
   @ 14,27 SAY MNDUP PICT '999,999'
*
   @ 14,37 SAY MVDP PICT '999,999,999.99'
   M.VDP = MVDP
*
   @ 14,55 SAY MNDUP PICT '999,999'
   @ 14,62 SAY '/'
   @ 14,63 SAY MPDUP PICT '9'
*
   @ 14,70 SAY MVENC
*
   SELE C
   SEEK MCGC
   IF !(EOF() .OR. BOF())
      @ 20,10 SAY RAZ
      @ 22,10 SAY END
      @ 22,55 SAY CEP
      @ 23,10 SAY CID
      @ 23,50 SAY EST
      STOR SPACE(60) TO ENDCOB
      STOR COB TO MCOB
      IF SUBS(MCOB,1,4) <> SPACE(4)
         STOR SUBS(MCGC,1,9)+MCOB TO CGCCOB
         SEEK CGCCOB
         IF .NOT. (EOF() .OR. BOF())
            STOR TRIM(END)+', '+TRIM(CID)+', '+EST+', '+STR(CEP,5) TO ENDCOB
         ENDIF
      ENDIF
   ENDIF

   IF ENDCOB <> SPACE(20)
      @ 24,10 SAY ENDCOB
   ENDIF

   @ 25,10 SAY MCGC
   @ 25,35 SAY INS
*
*
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
*
   EJECT
*
   SELE A
*
   ?? CHR(27)+CHR(64)
   SET PRINT OFF
   SET DEVI TO SCRE
*
ENDDO
*
CLOSE DATA
RETURN

