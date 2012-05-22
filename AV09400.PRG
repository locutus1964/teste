* AV09400 - RESTAURA OS ARQUIVOS EM BACKUP DE DISQUETE
* JAIR GONCALVES - 05/12/97
* SISTEMA EMISSAO DE NOTAS FISCAIS 
*
PRIVATE cPath, cDrive, cComando, cNomeVol

*------- Parametros basicos do Programa de Restore
cNomeVol = 'AV'
cComando = 'arj e -i1 -y '
cDrive   = 'A:'
 
close data
nDisco = 1
mc = '' 
mens = 'Deseja restaurar Arquivos do Backup em Disquete ?'
do pegresp with [SN]
if mc = 'S'
   MENS = ' Drive de origem: '  
   MC = '' 
   DO PEGRESP WITH [AB]
   IF MC <> '' 
      cDrive = MC + ':' 
   ENDIF
   DO WHILE .T.
     IF nDisco = 1
       =MENSAG('Insira primeiro disquete e Tecle ENTER ')
     ELSE   
       =MENSAG('Insira o pr¢ximo disquete e tecle ENTER ou ESC para sair')
     ENDIF  
     =INKEY(0)
     IF LASTKEY() = 27
       EXIT
     ENDIF  
     cTexto = cComando + cDrive + cNomeVol + '.A' + PADL( LTRIM(STR(nDisco,2)),2,'0')
     SAVE SCREEN TO cTela
     run CLS
     run foxswap &cTexto
     RESTORE SCREEN FROM cTela
     nDisco = nDisco + 1
   ENDDO  
endif
return

