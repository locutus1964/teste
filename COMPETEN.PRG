* COMPETEN - DEFINICAO E ESTABELECIMENTO DO MES DE COMPETENCIA PARA EMISSAO
*            DOS RAZOES E LIVROS DO FECHAMENTO CONTABIL MENSAL
* SISTEMA CONTAS A PAGAR/ADMINISTRACAO DE VENDAS
* ADAO ELI PEREIRA    -    01/05/92 - 16/11/92
*
USE PARAM
  IF PCOMPET <> UCOMPET
    ? MENSAG('Competˆncia j  definida ou Sistema c/problemas. Procure a Inform tica')
    =INKEY(0)
    RETURN
  ENDIF

  MUCOMPET = VAL(SUBS(UCOMPET,4,2) + SUBS(UCOMPET,1,2))
  MPCOMPET = PCOMPET
  MANO     = VAL(SUBS(UCOMPET,4,2))
*
? CABTELA(0,0,24,79)
MENS = 'Defini‡„o do Mˆs de Competˆncia Cont bil'
? MENSAGR(MENS)
@ 01,POS SAY MENS

@ 05,10 SAY '                  A T E N € Ž O'
@ 08,10 SAY 'O processo de "Encerramento Mensal" atualiza os Arquivos e dados'
@ 10,10 SAY 'do Sistema de forma irrecuper vel no caso "falha ou necessidade"'
@ 12,10 SAY 'de reprocessar a defini‡„o da Competˆncia.'
@ 15,10 SAY 'Portanto, antes de come‡ar o processo,  assegure-se de que foram'
@ 17,10 SAY 'efetuados 2 (duas) c¢pias BACKUP do Sistema...'

  MC = ' '
  MENS = "Tem certeza que os BACKUP'S foram feitos? (S/N) "
  ? MENSAG(MENS)
  @ 23,POS SAY MENS GET MC PICT '!' VALID(MC$'SN')
    READ
      IF MC = 'N'
        RETURN
      ENDIF
@ 05,10 CLEAR TO 17,79
*
MCOMPET = SPACE(05)

DO WHILE .T.
MENS = 'Ultima Competˆncia processada = '+UCOMPET
? MENSAGR(MENS)
@ 11,POS SAY MENS

MENS = 'Indique o Mes e Ano de Competˆncia a definir'
? MENSAGR(MENS)
@ 13,POS-3 SAY MENS GET MCOMPET PICT '99/99'
  READ
    IF VAL(SUBS(MCOMPET,1,2)) = 0 .OR. VAL(SUBS(MCOMPET,1,2)) > 12
      ? MENSAG('Mes n„o pode ser diferente de 1 a 12  -  ENTER')
      =INKEY(0)
      LOOP
    ENDIF

    IF VAL(SUBS(MCOMPET,4,2)) < 92
      ? MENSAG('Ano n„o pode ser menor que 92  -  ENTER')
      =INKEY(0)
      LOOP
    ENDIF

    IF VAL(SUBS(MCOMPET,4,2)) = (MANO + 1)
       MUCOMPET = MUCOMPET + 88
    ENDIF

    IF VAL(SUBS(MCOMPET,4,2) + SUBS(MCOMPET,1,2)) <> (MUCOMPET + 1)
      ? MENSAG('Competˆncia n„o est  na sequˆncia correta - ENTER')
      =INKEY(0)
      RETURN
    ENDIF

  MC = ' '
  ? MENSAG(DADCORR)
  @ 23,POS SAY DADCORR GET MC PICT '!' VALID(MC$'SN')
    READ
    IF MC = 'N'
      RETURN
    ELSE
      REPL PCOMPET WITH MCOMPET
    ENDIF
*
  RETURN
ENDDO
