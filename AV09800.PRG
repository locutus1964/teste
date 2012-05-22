* AV09800 - TROCA DE SENHA
* SISTEMA ADMINISTRATIVO DE VENDAS
* ADAO ELI PEREIRA - 27-08-90
*
USE PARAM
  STOR SCTR TO MSENHA
*
  ? JANELA(16,52,22,76)
  ? BORDA(16,52,22,76)
*
@ 18,55 SAY "Senha atual: "
S=""
DO WHILE LEN(S) < 6
    R = 0
    DO WHILE R = 0
        R=INKEY()
    ENDDO
    S=S+CHR(R)
ENDDO
  IF UPPER(S) <> MSENHA
    ? MENSAG("Funcion rio n„o Autorizado - ENTER")
      =INKEY(0)
      CANCEL
  ELSE
    @ 20,55 SAY "Nova Senha: "GET MSENHA PICT '!!!!!!'
      READ
      MENS = 'Nova Senha est  correta ?'
      DO PEGRESP WITH [SN]
      IF MC = 'S'
        REPL SCTR WITH MSENHA
      ENDIF
  ENDIF
*
CLEAR
CLOS DATA
RETURN
