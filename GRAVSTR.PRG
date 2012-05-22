CLOSE ALL
SET STATUS OFF
SET SAFETY OFF
=Gravar('CADNF')
=Gravar('CLIEN')
=Gravar('PEDID')
=Gravar('PEDIC')
=Gravar('DUPL')
=Gravar('CABECNF')
=Gravar('PROD')
=Gravar('TABREF')
=Gravar('VEND')
=Gravar('ITEMBASE')
=Gravar('ITEMCONJ')
=Gravar('CONJUNTO')
=Gravar('TPNF')
=Gravar('REGIAO')
=Gravar('FAMITEM')
=Gravar('ATREGIAO')
=Gravar('ESTADOS')
=Gravar('GRUPOFAM')
=Gravar('GRUPOS')
CLOSE ALL
RETURN


****************
PROCEDURE GRAVAR
****************
PARAMETER NOME
PRIVATE MNOME

  MNOME = NOME + '.MAP'
  USE &NOME
  COPY STRUCT TO TMP
  USE TMP
  LIST OFF FIELDS FIELD_NAME TO &MNOME
  USE
  
RETURN
