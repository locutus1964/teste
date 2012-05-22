close all
set talk on
run dir *.dbf /b >arq.txt
A = FOPEN('ARQ.TXT')
DO WHILE ! FEOF(a)
  F = FGETS(A)
  USE &F
  DELETE TAG ALL
ENDDO
FCLOSE(A)
CLOSE ALL
