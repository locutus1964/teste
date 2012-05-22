* AV09200 - BACKUP COMPACTADO DOS CADASTROS E ARQUIVOS - EM DISQUETE
* JAIR GONCALVES - 26/08/97
* SISTEMA EMISSAO DE NOTAS FISCAIS - 
*
PRIVATE cPath, cDrive, cComando, cNomeVol, Apaga, Formata

*------- Parametros basicos do Programa de Back-up
cArquivos = '*.DBF *.CDX *.MDX *.FPT' 
cNomeVol = 'AV'
cComando = 'arj a -v1440 -jyv -i1 ' + cNomeVol + '.A01 ' + cArquivos
cDrive   = 'A:'
Apaga    = .F.
Formata  = .T.
 
close data
cPath = FULLPATH('')
mc = '' 
mens = 'Vai fazer Backup dos Arquivos para Disquete ?'
do pegresp with [SN]
if mc = 'S'
  =Compactar()
  mloop = CalcNDisco()
  if mloop > 0 
    IF Formata 
      =Formatar()  
    ENDIF  
    =Copiar()
  endif  
  SET DEFAULT TO &cPath
  IF Apaga 
    =Apagar()
  endif  
endif
   
RETURN

* Calcula a quantidade de discos necessarios para o back-Up
*==================
FUNCTION CalcNDisco
*==================
  PRIVATE cFile, nDisco, nNext, nLast
  
  nDisco = 1
  nLast = 0
  DO WHILE .T.
    cFile = GetNome( nDisco )
    IF ! FILE(cFile)
      EXIT
    ENDIF
    nLast = nDisco
    nDisco = nDisco + 1
  ENDDO
  
RETURN nLast

*===============
FUNCTION GETNOME
*===============
PARAMETER nNum

 cNome = cNomeVol + '.A' + PADL( LTRIM(STR(nNum,2)),2,'0')

RETURN cNome

*==================
PROCEDURE COMPACTAR
*==================
  PRIVATE cTela
  
  =mensag('Aguarde ... Compactando Arquivos')
  SAVE SCREEN TO cTela
  run CLS
  RUN foxswap 'del ' + cNomeVol + '.A??'
  run foxswap &cComando
  RESTORE SCREEN FROM cTela
  
RETURN

*=================
PROCEDURE FORMATAR
*=================
  PRIVATE cTela
  
  SAVE SCREEN TO cTela    
  =Mensag( 'Voce precisara formatar ' + str(mloop,3) + ' disquete(s), Tecle algo para iniciar...' )
  IF INKEY(0) <> 27 
    RUN CLS
    RUN FORMAT A: /U     
    RESTORE SCREEN FROM cTela
  ENDIF
    
RETURN

****************
PROCEDURE COPIAR
****************

  for xx = 1 to mloop
    =mensag('Coloque o Disquete N£mero '+alltrim(str(xx,1))+' no Drive [ A ] e ENTER')
    cTecla = INKEY(0)
    if cTecla = 27
      exit
    endif
    SET DEFAULT TO &cDrive
    cFile = GetNome( xx )
    cNome = cPath + cFile     
    cDest = cDrive + cFile
    SAVE SCREEN TO ctmp1
    COPY FILE &cNome TO &cDest
    =mensag( 'Copiando Volume: ' + cFile + ' para disco n§ '+alltrim(str(xx,2)))
    RESTORE SCREEN FROM ctmp1
 next xx
 
RETURN 

****************
PROCEDURE APAGAR
****************

  FOR I = 1 TO MLOOP
    cFile = GetNome(i)
    ERASE &cFile
  NEXT I
  
RETURN  
