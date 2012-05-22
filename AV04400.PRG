* AV04500 - LISTA OS CLIENTES NA TELA
* ADAO ELI PEREIRA - 20/02/92
*
  MENS = 'Por C¢digo (C), Ordem Alfa (A) ou Nome Guerra (G) ? '
  DO PEGRESP WITH [CAG]
    IF MC = 'C'
      USE CLIEN ORDER P_CGC
      TIT = 'Clientes - Ordem de C¢digo'
    ELSE
      IF MC = 'A'
        USE CLIEN
        USE CLIEN INDEX CADWORK.IDX
        TIT = 'Clientes - Ordem Raz„o Social'
      ELSE
        USE CLIEN ORDER X_GUE
          TIT = 'Clientes - Ordem Nome de Guerra'
      ENDIF
    ENDIF
*
DO ABREVGA
? cabtela45(0,0,49,79)

MENS = TIT
? MENSAGR(MENS)
@ 01,POS SAY MENS
*
@ 03,01  say 'C¢digo/N.Guerra'
@ 03,22  say 'R.Social/Enderˆ‡o/Estado/CEP'
@ 03,60  say 'Tel/Contato'
MC = SPACE(01)
*
lin = 5
do while .not. EOF()
*
@ lin,01 say CGC
@ lin,22 say RAZ
@ lin,60 say DDD1+' '+FON1
  lin = lin + 1
@ lin,02 say GUE
@ lin,22 say END
@ lin,60 say DDD2+' '+FON2
  lin = lin + 1
@ lin,22 say CID+' '+EST+' '+CEP
@ lin,60 say SUBS(CONTATO,1,18)
*
  LIN = lin + 2
    skip
      IF EOF()
        set esca off
        ? MENSAG49('Final do Cadastro   -   ENTER')
        =INKEY(0)
        CLOS DATA
        DO FECHAVGA
        RETURN
      ENDIF

      IF LIN > 44
        MENS = 'Consulta mais fornecedores ?'
        DO PEGRESP49 WITH [SN]
            IF MC = 'S'
              @ 04,01 CLEAR TO 46,78
              LIN = 5
            ELSE
              set esca off
              CLOS DATA
              DO FECHAVGA
              RETURN
            ENDIF
      ENDIF
  enddo
return
