CLOSE DATA
CLEAR
SET TALK OFF
SET SAFETY OFF
* Seleciona Clientes
SELE 0
USE CLIEN
INDEX ON FmtRegiao(REGIAO, IDREGIAO)+ESTATUS TO TST1
* Seleciona Acumulador por Regiao e Status
SELE 0
USE STATUS INDEX STATUS
SELE CLIEN
SET RELATION TO ESTATUS INTO STATUS
GO TOP
* Enquanto houverem registros no arquivo
DO WHILE ! EOF()
  * Enquanto for a mesma regiao
  MREGIAO = FmtRegiao(REGIAO, IDREGIAO)
  DO WHILE ! EOF() .AND. MREGIAO = FmtRegiao(REGIAO, IDREGIAO)
    * Enquanto for o mesmo status, acumula 
    MSTATUS = ESTATUS
    CONT = 0
    DO WHILE ! EOF() .AND. MREGIAO == FmtRegiao(REGIAO, IDREGIAO) .AND. MSTATUS == ESTATUS 
      CONT = CONT + 1 
      SKIP
    ENDDO 
    SKIP -1
    REPLACE STATUS->QUAN WITH CONT
    SKIP
  ENDDO  
ENDDO
CLOSE DATA
SET TALK ON
SET SAFETY ON