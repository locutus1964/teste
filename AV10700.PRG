* AV10700 - LIMPA ARQUIVOS SEM USO APOS RELATORIOS DE ESTATISTICA DE VENDAS
*
    MENS = 'Tem certeza que pode excluir os arquivos ?'
    DO PEGRESP WITH [SN]
    
    IF MC = 'N'
      RETURN
    ELSE
      IF FILE("CADWORK1.DBF")
        ERASE CADWORK1.DBF
      ENDIF
        
      IF FILE("CADWORK2.DBF")
        ERASE CADWORK2.DBF
      ENDIF
      
      IF FILE("CADWORK3.DBF")
        ERASE CADWORK3.DBF
      ENDIF
      
      IF FILE("CADWORK4.DBF")
        ERASE CADWORK4.DBF
      ENDIF
      
    ENDIF
  RETURN
