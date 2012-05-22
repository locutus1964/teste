
* Remove data de cancelamento no arquivo de dupls
*  anterior a data de emissao das mesmas
PROCEDURE ElimDataCancel
  ? 'Processando...' 
  CLOSE DATA
  CLEAR
  SET TALK ON
  USE DUPL
  REPLACE ALL dtcancel WITH ctod('');
          FOR dtcancel < emis .AND. ! empty(dtcancel)
                                      
RETURN

* Elimina duplicatas que estao duplicadas no arquivo
PROCEDURE ExclDupl
  clear
  set talk off
  close data
  use dupl
  index on str(ndup)+str(pdup) to tst
  go top
  cont = 0
  ? 'Processando...'
  do while .not. eof()
    mndup = ndup
    mpdup = pdup
    do while .not. eof() .and. (mndup = ndup .and. mpdup = pdup)
      if pag = space(8) 
         delete 
         cont = cont + 1
      endif   
      skip
    enddo  
  enddo
  ? 'Duplicatas eliminadas:' + Str( cont )
  close data
RETURN
  