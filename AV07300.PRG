* AV07300 - PARAMETROS PARA EMISSAO DE NOTAS FISCAIS
* SISTEMA ADMINISTRACAO DE VENDAS
* ADAO ELI PEREIRA - 6/5/90
*
=CABTELA(0,0,24,79)

mens = 'Parƒmetros para emiss„o de Notas Fiscais'
=mensagr(mens)
@ 01,pos say mens
*
SELE A
USE PARAM
MTOL = TOL
MUNF = UNF
MNNF = NNF
munnf_entr = unnf_entr
mnnf_entr  = nnf_entr
MICMEMB = ICMEMB
@ 08,46 SAY MICMEMB
MICMFRE = ICMFRE
@ 08,55 SAY MICMFRE
MICMDES = ICMDES
@ 08,65 SAY MICMDES
MIPIEMB = IPIEMB
@ 10,46 SAY MIPIEMB
MIPIFRE = IPIFRE
@ 10,55 SAY MIPIFRE
MIPIDES = IPIDES
@ 10,65 SAY MIPIDES
MDESTACA = DESTACA
@ 12,55 SAY MDESTACA
DO WHILE .T.
   @ 06,44 SAY 'Frete'
   @ 06,51 SAY 'Embalagem'
   @ 06,61 SAY 'Descontos'
   @ 08,10 SAY 'Inclui para c lculo do ICMS=(S/N)'
   @ 10,10 SAY '                       IPI=(S/N)'
   @ 12,10 SAY 'Destaca os Descontos na Nota Fiscal=(S/N)'
   @ 14,10 SAY 'Percentual de Tolerƒncia p/Cr‚dito excedido  '+str(MTOL,5,2)
   @ 16,10 say 'At‚ que NF pagar Comissao aos Vendedores    '+STR(MUNF,6)
   @ 17,05 say 'NOTA FISCAL DE SAIDA'
   @ 18,10 say 'N£mero da £ltima Nota Fiscal emitida        '+STR(MNNF,6)
   @ 19,05 say 'NOTA FISCAL DE ENTRADA'
   @ 20,10 say 'N£mero da £ltima Nota Fiscal emitida        '+STR(mnnf_entr,6)
   MENS = 'Altera os Parƒmetros para emiss„o das Notas Fiscais=(S/N) '
   DO PEGRESP WITH [SN]
   IF MC = 'N' .or. empty(mc)
      PARAM = .F.
      exit
   ELSE
      =MENSAG('Informe se inclui o ICMS para o c lculo do Frete')
      @ 08,46 GET MICMFRE PICT '!' VALID(MICMFRE$'SN')
      =MENSAG('Informe se inclui o ICMS para o c lculo da Embalagem')
      @ 08,55 GET MICMEMB PICT '!' VALID(MICMEMB$'SN')
      =MENSAG('Informe se inclui o ICMS para o c lculo dos Descontos')
      @ 08,65 GET MICMDES PICT '!' VALID(MICMDES$'SN')
      =MENSAG('Informe se inclui o IPI para o c lculo do Frete')
      @ 10,46 GET MIPIFRE PICT '!' VALID(MIPIFRE$'SN')
      =MENSAG('Informe se inclui o IPI para o c lculo da Embalagem')
      @ 10,55 GET MIPIEMB PICT '!' VALID(MIPIEMB$'SN')
      =MENSAG('Informe se inclui o IPI para o c lculo dos Descontos')
      @ 10,65 GET MIPIDES PICT '!' VALID(MIPIDES$'SN')
      =MENSAG('Informe se desta os Descontos concedidos na Nota Fiscal')
      @ 12,55 GET MDESTACA PICT '!' VALID(MDESTACA$'SN')
      =MENSAG('Informe at‚ que percentual o Limite de Cr‚dito pode ser excedido')
      @ 14,55 GET MTOL PICT '99.99'
      =MENSAG('Informe at‚ que n£mero de Nota Fiscal pagar comiss„o aos Vemdedores')
      @ 16,54 get MUNF pict '999999'
      =MENSAG('Informe qual o n£mero da £ltima Nota Fiscal de Sa¡da emitida pelo Sistema')
      @ 18,54 get MNNF pict '999999'
      =MENSAG('Informe qual o n£mero da £ltima Nota Fiscal de Entrada emitida p/ Sistema')
      @ 20,54 get MNNF_entr pict '999999'
      READ
      if lastkey() = 27
         exit
      endif
   ENDIF
   MENS = DADCORR
   DO PEGRESP WITH [SN]
   IF MC = 'S'
      REPL TOL WITH MTOL,UNF WITH MUNF,NNF WITH MNNF, nnf_entr with mnnf_entr
      REPL ICMEMB WITH MICMEMB,ICMFRE WITH MICMFRE,ICMDES WITH MICMDES
      REPL IPIEMB WITH MIPIEMB,IPIFRE WITH MIPIFRE,IPIDES WITH MIPIDES
      REPL DESTACA WITH MDESTACA
      EXIT
   ENDIF
ENDDO
close data
CLEAR
RETURN
