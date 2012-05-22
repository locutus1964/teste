* AV05200 - ATUALIZACAO DO CADASTRO DE PRODUTOS - QUANTIDADES - 18/11/86
* SISTEMA DE ADMINISTRACAO DE VENDAS
* ADAO ELI PEREIRA - 15-04-90
*
SELE 0
  USE PROD INDEX PROD.IDX
*
? CABTELA(0,0,24,79)
*
@ 01,29 say 'Atualiza‡„o de Produto'
@ 05,11 say 'Referˆncia'
@ 07,11 say 'Descri‡„o'
@ 09,11 say 'Saldo'
@ 11,11 say 'Notas Fiscais'
@ 13,11 say 'Vendas no Mes'
@ 15,11 say 'Pedidos em Aberto'
@ 17,11 say 'Vendas at‚ o Mes'
@ 19,11 say 'Faturado no Mes'
@ 21,11 say 'Faturado at‚ o Mes'
mref = SPACE(04)
*
do while .t.
*
stor 0 to mpea, mvam, mvaa, mfme, mfac, mdel
mcom = 0.00
*
? MENSAG(ECODIGO)
@ 05,43  get mref
  read
  
  if empty(mref)
    ? MENSAG(CALPHA)
    loop
  endif
  
  if lastkey() = 27
    clos data
    clear
    return
  endif
*
  if !seek(MREF)
    ? MENSAG(CORRIJA)
    =INKEY(0)
    loop
  else
    @ 07,25 say des
    @ 09,41 say str(sal,11,2)
    @ 11,41 say str(com,11,2)
      mcom = com
    @ 13,42 say str(vam,10)
      mvam = vam
    @ 15,42 say str(pea,10)
      mpea = pea
    @ 17,42 say str(vaa,10)
      mvaa = vaa
    @ 19,42 say str(fme,10)
      mfme = fme
    @ 21,42 say str(fac,10)
      mfac = mfac
*
    MENS = 'Qual sua op‡„o? Retorna (1), Altera (2) ? '
    DO PEGOPCAO WITH [12]
        if opcao = '1'
          loop
        else
          ? MENSAG('Ajustar saldo somente pelo AJUSTE DE INVENTARIO')
          @ 11,41 get mcom pict '99999999.99'
          @ 13,42 get mpea pict '9999999999'
          @ 15,42 get mvam pict '9999999999'
          @ 17,42 get mvaa pict '9999999999'
          @ 19,42 get mfme pict '9999999999'
          @ 21,42 get mfac pict '9999999999'
            read
              MENS = dadcorr
              DO PEGRESP WITH [SN]
                  if mc = 'N'
                    loop
                  endif
                    repl com with mcom,vam with mvam,pea with mpea
                    repl vaa with mvaa,fme with mfme,fac with mfac

        endif
  endif
enddo
