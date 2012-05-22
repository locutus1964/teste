CLEAR
SET TALK OFF
close all
set safety off

RUN DIR /B STRU\*.DBF >ARQ.TXT
P = FOPEN('ARQ.TXT', 0)
L = FCREATE('LISTA.TXT', 0)
DO WHILE ! FEOF(P)
  MNOME = 'STRU\' + FGETS(P)
  IF MNOME <> 'STRU\FOXUSER.DBF'
    ? 'Abrindo ' + MNOME 
    USE &MNOME ALIAS ORIGEM
    ? 'Processando ' + MNOME   
    Suspeito = .F.
    DO WHILE !EOF()
      IF (FIELD_TYPE = 'C' .AND. FIELD_LEN = 8) .OR.;
         (FIELD_TYPE = 'C' .AND. FIELD_NAME = 'ANO') THEN
        IF ! Suspeito THEN
          Suspeito = .T.          
          =FPUTS(L, MNOME)
        ENDIF
        =FPUTS(L, '  ' + PADR(FIELD_NAME, 10, ' ') + PADR(FIELD_TYPE, 1, ' ') + STR(FIELD_LEN,2))
      ENDIF
      SKIP
    ENDDO
  ENDIF  
ENDDO
=FCLOSE(P)
=FCLOSE(L)
SET TALK ON
SET SAFETY ON
CLOSE ALL


