REQUEST DBFCDX

CLEAR
aFiles = DIRECTORY( '*.DBF')
FOR i = 1 TO LEN(aFiles)
   IF aFiles[i,1] <> 'FOXUSER.DBF' .AND. aFiles[i,1] <> 'ARQFOX.DBF'
      Copiar( aFiles[i,1] )
   ENDIF
NEXT i


*****************************
PROCEDURE COPIAR( cFileName )
*****************************
  LOCAL cFileNovo

  cFileNovo = 'NOVO\' + cFileName
  IF ! FILE( cFileNovo )
     DBUSEAREA( .F., 'DBFCDX', cFileName )
     ? 'Copiando... ' + cFilename
     COPY TO (cFileNovo)
     /*
     aDbf := DBSTRUCT()
     DBCREATE( cFileNovo, aDbf, 'DBFNTX' )
     ? 'Copiando... ' + cFilename 
     DBUSEAREA( .F., 'DBFNTX', cFileNovo )
     APPEND FROM &cFileName
     */
  ELSE
     ? 'Ignorando... ' + cFileNovo
  ENDIF
RETURN NIL
