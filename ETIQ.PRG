CLS
NCOLS = 2
ARQUIVO = SPACE(8)
DO WHILE .T.
  @10, 20 SAY "ARQUIVO....:" GET ARQUIVO PICT [@!]
  @11, 20 SAY "NUM.COLUNAS:" GET NCOLS PICT [999]
  READ
  IF LASTKEY() = 27
    EXIT
  ENDIF  
  ARQUIVO = LTRIM(ARQUIVO)
  RESP = 'S'
  @ 24, 10 SAY "Dados Corretos (S/N)?" GET RESP PICT [@!] VALID RESP $ [SN]
  READ
  IF RESP = 'S'
    =IMPRIMIR()
  ENDIF 
ENDDO

******************
PROCEDURE IMPRIMIR
******************
  LOCAL F, ALINHAS
  
  ALINHAS := {}
  F = FOPEN(ARQUIVO + '.TXT', 0)
  IF F < 0
    @ 24, 10 SAY 'Impossivel abrir arquivo...'
    =INKEY(0)
    RETURN
  ENDIF  
  DO WHILE ! FEOF(F)
    LINHA = FGETS(F, 80)
    AADD(ALINHAS, LINHA)
  ENDDO

  SET DEVICE TO PRINTER
  SET PRINTER TO ARQ.TXT
  L = 0
  C = 0
  FOR I = 1 TO NCOLS
    
  NEXT I
  SET DEVICE TO SCREEN  
  =FCLOSE(F)
RETURN
