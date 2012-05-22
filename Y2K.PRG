	*==============================================
	* The Y2K problem - Get it Done!
	*
	* Many thanks to Norm Weil, Tom Rombouts, 
	* Gerard O'Carroll, Brad Schulz and Peter 
	* Harris for contributing to this solution. 
	* You have been fantastic!
	*==============================================

		#Define Y2K_ROLLOVER  1944
		#Define Y2K_INVDATE   "Invalid date."
		#Define Y2K_INVINPUT  "Invalid input"

		Clear
		Set Talk off
		
		dDate1 = date()
		@ 1,1 Say "Here you can't enter 02/29/00: "	Get dDate1
		dDate2 = date()
		@ 3,1 Say "and here you can!            : "	Get dDate2 ;
			When Y2K_When() ;
			Message Y2K_Mess() ;
			Valid Y2K_Valid()
		Read
		=y2k_valid2()
			
		Set Century on
		@ 7, 1 Say "You entered " + DToC(dDate2)

	return
	
	*==================================================
	* pass #1:
	* get the date that the user entered. 
	*==================================================
	Procedure Y2K_1
	
		*-----------------------------------------------
		* Repeat error message in Wait Window?
		*-----------------------------------------------
		gnY2KMess = gnY2KMess + 1
		If Type("gcY2KErrMsg") == "C"
			Wait Window NoWait gcY2KErrMsg
			Release gcY2KErrMsg
		Endif

		*-----------------------------------------------
		* Did the previous VALID return 0/.F. and was 
		* the invalid date 02/29/00? In this case we
		* keyboarded the date again and therefore we
		* should ignore this function call.
		*-----------------------------------------------
		If Type("glY2KRestore") # "U"
			If not glY2KInVal
				Set Confirm &gcY2KConfirm
			Endif
			Release glY2KRestore
			Return 
		Endif
	
		*-----------------------------------------------
		* If Y2K_1 was called by a RETURN .F. in the
		* VALID, ignore REARERROR....
		*-----------------------------------------------
		If glY2KInVal
		
			Set Confirm &gcY2KConfirm
			Do Y2K_ReadErr
			glY2KInVal = .F.
		
		*-----------------------------------------------
		* ....otherwise get the input
		*-----------------------------------------------
		Else
		
			*--------------------------------------
			* Make some initializations...
			*--------------------------------------
			Set Confirm &gcY2KConfirm
			gnY2KRead = Lastkey()
			glY2K = .F.
			_Cliptext = "NO DATE"
			Clear Typeahead
			On Readerror Do Y2K_2
			
			*--------------------------------------
			* Get the current input by simulating
			* Ctrl+A, Ctrl+C (Select all, Copy).
			* We have top work around a few 
			* problems here. Therefore the code is
			* more than just a Keyboard command.
			*--------------------------------------
			If _Windows and "2.6" $ Version()
				If Y2K_Browse()
					Keyboard "{Home}{Shift+Home}" Plain
				Endif
				If InList( gnY2KRead, 1, 19 )
					Keyboard "{Home}" Plain
				Endif
				If Set("Confirm") == "ON"
					Keyboard "{Home}" Plain
				Endif
				Keyboard "{Shift+End}" Plain
			Else
				Keyboard "{Ctrl+A}" Plain
			Endif
			Keyboard "{Ctrl+C}" + ;
				Iif(_Dos,"{End}{Home}","")+"{Tab}" Plain
			
		Endif
		
	Return

	*==================================================
	* pass #2:
	* check for 02/29/00 and change it to 2000
	*==================================================
	Procedure Y2K_2
	
		*--------------------------------------------	
		* check wether the "K" property has been 
		* used. In this case Shift+End exited the 
		* field and Ctrl+C is still in the buffer. 
		* Just return to get the date.
		*--------------------------------------------	
		gnY2KMess = gnY2KMess + 1
		If	  _Windows and "2.6" $ Version() ;
			and _Cliptext == "NO DATE"
			Return
		Endif

		*--------------------------------------------	
		* When SET CONFIRM is ON and the "K" property
		* has not been used, only the last character
		* is copied. Therefore we try it again now.
		*--------------------------------------------	
		If	  _Windows and "2.6" $ Version() ;
			and _Cliptext == "0"
			Keyboard "{Shift+End}{Ctrl+C}{Tab}" ;
				Clear Plain
			Return
		Endif

		*--------------------------------------------	
		* re-activate pass #1 handler
		*--------------------------------------------	
		Private llValid
		llValid = .F.
		On Readerror Do Y2K_1

		*--------------------------------------------
		* Using the julian date, we build a 02/29/00
		* string that respects the current SET DATE
		* setting. For british dates, we hardcode
		* the british format, but handle a possible
		* different SET MARK setting.
		*--------------------------------------------
		Private cDate, cBritish, cCentury
		cCentury = Set("Century")
		Set Century Off 
		cDate = Sys( 10, 2451604 )
		Set Century &cCentury
		cDate = ChrTran(cDate," ","")
		If ChrTran(_ClipText," ","") == cDate
			llValid = .T.
		Else
			If glY2KBritish
				cBritish = "29/02/00"
				If Set("Century") == "ON"
					cBritish = cBritish + "  "
				Endif
				If not Empty( Set("Mark") )
					cBritish = ;
						ChrTran( cBritish,;
						"/", Set("Mark") )
				Endif
				If Alltrim(_ClipText) == cBritish
					llValid = .T.
				Endif
			Endif
		Endif
		
		*-----------------------------------------------	
		* and clear the field in order to leave it or
		* if it's another wrong date display a message
		* or call the previous ReadError handler
		*-----------------------------------------------	
		gcY2KConfirm = Set("Confirm")
		If llValid
			glY2K = .T.
			Do Y2K_Stuff with gdY2KValid
		Else
			Do Y2K_ReadErr
		Endif
		
	Return

	*===============================================
	* Stuff a date into the keyboard buffer and
	* leave the field in the last direction. If
	* this GET uses the "E" function code for 
	* british dates, but SET DATE is set to some-
	* thing different, you should specifiy a
	* date that is valid in both cases, for example
	* 01/01/01.
	*===============================================
	Procedure Y2K_Stuff
	Parameter tdDate
	
		Private lcDate
		Clear Typeahead
		If Empty( tdDate )
			Keyboard "{Ctrl+A}{Del}" Plain
			Keyboard Y2K_Key(.T.) Plain
		Else
			lcDate = DTOC(tdDate)
			Set Confirm On
			Keyboard "{Ctrl+A}"+lcDate Plain
			Keyboard "{Home}" Plain
			Keyboard Y2K_Key(.F.) Plain
		Endif

	Return

	*===============================================
	* returns the last key. This function is used to
	* repeat the key stroke the user pressed to 
	* leave the GET field.
	*===============================================
	Function Y2K_Key
	Parameters tlDateBlank
	
		Do Case
			Case gnY2KRead = 1
				lcKey = "{Home}"
			Case gnY2KRead = 2
				lcKey = "{Ctrl+Leftarrow}"
			Case gnY2KRead = 3
				lcKey = "{PgDn}"
			Case gnY2KRead = 4
				lcKey = "{End}{RightArrow}" +;
					Iif( tlDateBlank,"{RightArrow}","" )
			Case gnY2KRead = 5 
				lcKey = "{UpArrow}"
			Case gnY2KRead = 6
				lcKey = "{End}{End}"
			Case gnY2KRead = 9 
				lcKey = "{Tab}"
			Case gnY2KRead = 10
				lcKey = "{Ctrl+Enter}"
			Case gnY2KRead = 11
				lcKey = "{UpArrow}"
			Case gnY2KRead = 13
				lcKey = "{Enter}"
			Case gnY2KRead = 15
				lcKey = "{Backtab}"
			Case gnY2KRead = 17
				lcKey = "{Ctrl+Q}"
			Case gnY2KRead = 18 
				lcKey = "{PgUp}"
			Case gnY2KRead = 19
				lcKey = "{LeftArrow}"
			Case gnY2KRead = 23
				lcKey = "{Ctrl+End}"
			Case gnY2KRead = 24
				lcKey = "{DnArrow}"
			Case gnY2KRead = 27
				lcKey = "{Esc}"
			Case gnY2KRead = 29 
				lcKey = "{Ctrl+Home}"
			Case gnY2KRead = 30
				lcKey = "{Ctrl+PgDn}"
			Case gnY2KRead = 31
				lcKey = "{Ctrl+PgUp}"
			Case gnY2KRead = 127
				lcKey = "{BackSpace}"
			Otherwise
				lcKey = "{Tab}"
		Endcase
	
	Return lcKey

	*===============================================
	* Display "invalid" message or call the repvious
	* ON READERROR handler. Using this technique
	* a previously used ON READERROR handler will 
	* still be used and doesn't need to be changed.
	* BROWSE and GET display different messages. A
	* BROWSE prefers the status bar, but uses a 
	* WAIT WINDOW if no status bar is visible.
	*===============================================
	Procedure Y2K_ReadErr

		If	 Empty( gcY2KRead ) ;
			or gcY2KOnRead == gcY2KRead
			If Y2K_Browse()
				If     _Windows ;
				   and Set( "Status Bar" ) == "ON"
					Set Message to Y2K_INVINPUT
				Else
					?? Chr(7)
					Wait Window Nowait Y2K_INVINPUT
				Endif
			Else
				?? Chr(7)
				Wait Window Nowait Y2K_INVDATE
			Endif
		Else
			&gcY2KRead
		Endif

	Return

	*===============================================
	* valid method to recognize the global flag and
	* change the date.
	*===============================================
	Function Y2K_Valid1
	Parameter tuReturn

		*--------------------------------------------
		* parameters are necessary for numeric 
		* values. By default .T. is assumed.
		*--------------------------------------------
		Private luReturn
		If Parameters() == 0
			luReturn = .T.
		Else
			luReturn = tuReturn
		Endif
	
		*--------------------------------------------
		* change date if it's either below Rollover 
		* or 02/29/00 has been entered.
		*--------------------------------------------
		Private lcVar
		Set Confirm &gcY2KConfirm
		lcVar = Y2K_Var()
		If glY2K
			If Y2K_MemVar(lcVar)
				Store Y2K_022900() To (lcVar)
			Else
				Replace next 1 ;
					(lcVar) with Y2K_022900()
			Endif
			If not Y2K_Browse()
				Show Get (lcVar)
			Endif
		Endif
	  	ldDate = Evaluate(lcVar)
		If Year( ldDate ) < Y2K_ROLLOVER
			If Y2K_MemVar(lcVar)
				Store GoMonth(ldDate,1200) To (lcVar)
			Else
				Replace next 1 ;
					(lcVar) With GoMonth(ldDate,1200)
			Endif
		Endif
		
 		*--------------------------------------------
		* Should we check for a changed field in
		* BROWSE? This simulates a :V clause without
		* :F clause. Pass the valid expression as
		* a string in this case.
 		*--------------------------------------------
 		If Type( "luReturn" ) == "C"
 			luReturn = .T.
 			If     Evaluate(lcvar) # gdY2KOldVal ;
 			   and not Empty(luReturn)
				luReturn = Evaluate(luReturn)
 			Endif
 		Endif

 		*--------------------------------------------
		* update flags
		*--------------------------------------------
		=Y2K_Failed( luReturn )

	Return luReturn

	*===============================================
	* return the GET variable/field
	*===============================================
	Procedure Y2K_Var

		*--------------------------------------------
		* Is it a BROWSE?
		*--------------------------------------------
		Private lcVar
		If Y2K_Browse()
			If Empty( gcY2KField )
				lcVar = Alias() + "." + VarRead()
			Else
				lcVar = gcY2KField
			Endif
				
		*--------------------------------------------
		* Nope, it's a GET. 
		* BUG: ObjVar() doesn't return the proper
		*      value if a radiobutton with more than
		*      two objects is used in the screen. In 
		*      that case we assume a memory variable
		*      and use VarRead() to get it. If you
		*      work on the table, you must define
		*      a variable gcY2KObVar in the setup
		*      snippet of the screen and set it to
		*      the proper field name before you call
		*      Y2K_WHEN.
		*--------------------------------------------
		Else
	  		lcVar = ObjVar(_CurObj)
			If Empty(lcVar)
				If     Type("gcY2KObVar") == "C" ;
				   and not Empty(gcY2KObVar)
					lcVar = gcY2KObVar
				Else
					lcVar = "M." + VarRead()
				Endif
			Endif
	  		If     Left(lcVar,2) == "M." ;
	  		   and "(" $ lcVar
  				lcVar = SubStr(lcVar,3)
	  		Endif
		Endif
		
	Return lcVar

	*===============================================
	* returns .T., if tcVar is a memvar
	*===============================================
	Function Y2K_MemVar
	Parameter tcVar
	
		If	   Left(Upper(tcVar),2) == "M." ;
			or "(" $ tcVar
			Return .T.
		Else
			Return .F.
		Endif
		
	Return

	*===============================================
	* returns .t. if we are in a BROWSE window
	*===============================================
	Function Y2K_Browse
	
		If	  Empty( gcY2KField ) ;
			and VarRead() == Upper(VarRead())
			Return .F.
		Endif
		
	Return .t.

	*===============================================
	* Store the return type of valid. This is
	* neccessary because numeric and logical 
	* values cause different effects on the read-
	* error handler. Numeric return values tell 
	* FoxPro that everything is OK, even if 0 is
	* returned. Therefore the ReadError handler is
	* only fired again if .F. is returned which is
	* the only return value that indicates a failure
	* for FoxPro.
	*===============================================
	Function Y2K_Failed
	Parameter tuReturn
	
		If Type( "glY2KInVal" ) # "U"
			If Type( "tuReturn" ) == "L"
				glY2KInVal = .T.
			Else
				glY2KInVal = .F.
			Endif
		Endif
	
	Return tuReturn

	*===============================================
	* valid method to restore the previous ON
	* READERROR handler and cleanup the variables
	* that we created in Y2K_When.
	*===============================================
	Function Y2K_Valid2
	Parameter tuReturn
	
		*--------------------------------------------
		* parameters are necessary for numeric 
		* values. By default .T. is assumed.
		*--------------------------------------------
		Private lnParameter
		lnParameter = Parameter()
		If lnParameter == 0
			tuReturn = .T.
		Endif

		*--------------------------------------------
		* In case 0 or .F. has been passed, the 
		* previous valid failed. We now have to set
		* the flags and in case it was 02/29/00 that
		* failed we need to make sure it will be 
		* restored again. Otherwise FoxPro would
		* clear the GET field, because 02/29/00 iw
		* not a valid date.
		*--------------------------------------------
		If Empty( tuReturn ) 
			gnY2KMess = gnY2KMess + 1
 			If lnParameter > 0
				=Y2K_Failed(tuReturn)
			Endif
			_CurObj = 0
			If	  glY2K ;
				and gnY2KRead # 27
				Public glY2KRestore
				Do Y2K_Stuff with Y2k_022900()
			Endif
			glY2K = .F.
		
		*--------------------------------------------
		* otherwise clean up
		*--------------------------------------------
		Else
			If Type("glY2K") # "U"
				If Y2K_Browse()
					Set Message to
				Endif
				If LastKey() == 27
					If Y2K_MemVar(gcY2KVar)
						Store gdY2KOldVal To (gcY2KVar)
					Else
						Replace next 1 ;
							(gcY2KVar) With gdY2KOldVal
					Endif
				Endif
				On Readerror &gcY2KRead
				Set Bell &gcY2KBell
				Release Popup Y2KPopup
				Release glY2K, gcY2KRead, glY2KInVal,;
					gcY2KBell, glY2KBritish,;
					gdY2KValid, gcY2KOnRead,;
					gcY2KField, gdY2KOldVal, gnY2KRead,;
					gcY2KConfirm, gnY2KMess, gcY2KVar
			Endif
		Endif

	Return tuReturn

	*===============================================
	* complete Y2K Valid for all GETs that don't 
	* have any other valid that could fail.
	*===============================================
	Function Y2K_Valid

		=Y2K_Valid1(.t.)
		=Y2K_Valid2(.t.)
	
	Return .T.
	
	*===============================================
	* returns 02/29/00 as date
	*===============================================
	Procedure Y2k_022900
		
		Private lcCentury, ldDate
		lcCentury = Set("Century")
	  	Set Century On
		ldDate = Evaluate("CToD(Sys(10,2451604))")
		Set Century &lcCentury
			
	Return ldDate

	*===============================================
	* when method to initialize the Y2K handler
	* tlBritish: Accept a british date (default).
	*				 This is necessary if the "E" code
	*				 has been used.
	* tdValid: A valid date. By default this is
	*			  an empty date. When using a RANGE 
	*			  clause this must be any date within
	*			  the valid range. 
	* tcField: The date field. If not empty, Y2K
	*			  assumes we're in a BROWSE window. 
	*			  Otherwise it tries to figure out 
	*          whether GET or BROWSE is active. In 
	*          case the the field contains only one 
	*          character (eg. A, D1887, etc.) a GET 
	*          is assumed.
	*===============================================
	Procedure Y2K_When
	Parameter tlBritish, tdValid, tcField
	
		*--------------------------------------------
		* Default for tlBritish is .T., ie. check for
		* British date.
		*--------------------------------------------
		If Parameter() < 1
			tlBritish = .T.
		Endif

		*--------------------------------------------
		* Default for tdValid is an empty date.
		*--------------------------------------------
		If Parameter() < 2
			tdValid = {}
		Endif

		*--------------------------------------------
		* Default for tcField is ""
		*--------------------------------------------
		If Parameter() < 3
			tcField = ""
		Endif
		
		*--------------------------------------------
		* recursive call of WHEN?
		*--------------------------------------------
		Private lcVar
		If Type("glY2K") # "U"
			Return .T.
		Endif

		*--------------------------------------------
		* Initialize some variable for all other
		* parts of Y2K. Here global variables are
		* used to minimize your coding. If you have
		* nested READ levels with date fields on it,
		* you have to replace these variables with
		* an array and define an index that is 
		* increased on every new level. Without 
		* BROWSEs you could use RDLEVEL() for this 
		* index, otherwise you have to define your 
		* own index.
		* glY2K: This flag is set by the ON READERROR
		*        handler, when 02/29/00 is detected.
		*        Y2K_Valid1 uses it, to correct the
		*        date in this case.
		* gcY2KRead: Contains the current ON 
		*            READERROR setting. It's used to 
		*            call the previous ON READERROR
		*            handler, when an invalid date 
		*            has been entered, and we need it
		*            to restore the previous setting
		*            in Y2K_Valid2.
		* glY2KInVal: When VALID returns .F., ON
		*             READERROR is called again. This
		*             flag is set in Y2K_Failed, when
		*             a logical return value is 
		*             passed to the VALID handler. In
		*             Y2K_1 this forces the ON 
		*             READERROR handler to ignore 
		*             this second function call.
		* gcY2KBell: the old SET BELL setting
		* glY2KBritish: Store the thBritish para-
		*               meter passed to Y2K_When.
		* gcY2KOnRead: Contains the ON READERROR
		*              setting after we directed it
		*              to Y2K_1. If for some reason
		*              ON READERROR is not restored
		*              afterwards, this variable is
		*              used to prevent recursive 
		*              calls to Y2K_1.
		* gcY2KField: tcField parameter of Y2K_When
		* gdY2KOldVal: Contains the value of the
		*              GET field or current field in
		*              BROWSE, when Y2K_When is exec-
		*              uted. We need this value to be
		*              able to restore the previous 
		*              value (see below).
		* gnY2KRead: Contains the Inkey() code of the
		*            key that has been used to leave
		*            the current input field. This
		*            variable is set in Y2K_1. We use
		*            it to simulate the last key 
		*            stroke after we finished 
		*            processing.
		* gcY2KConfirm: Old SET CONFIRM setting.
		* gnY2KMess: A counter that is increased in
		*            the ON READERROR handler and the
		*            VALID code. If this variable is
		*            0 when Y2K_Mess() fires, the 
		*            user 
		*            a) pressed a OKL.
		*            b) launched a second screen
		*            c) entered the menu
		*            d) started a system tool like
		*               the calculator.
		*            In these cases, we need to 
		*            restore the proper value, 
		*            because FoxPro accepted the 
		*            current input without 
		*            validation. (See Y2K_Mess)
		* gcY2KVar: Contains the variable this GET
		*           field or BROWSE column is bound
		*           to.
		*--------------------------------------------
		Public glY2K, gcY2KRead, glY2KInVal,;
			gcY2KBell, glY2KBritish, gdY2KValid,;
			gcY2KOnRead, gcY2KField,;
			gdY2KOldVal, gnY2KRead, gcY2KConfirm,;
			gnY2KMess, gcY2KVar
		glY2K = .F.
		glY2KInVal = .F.
		gcY2KBell = Set( "Bell" )
		gcY2KRead = On( "Readerror" )
		glY2KBritish = tlBritish
		gdY2KValid = tdValid
		gcY2KField = tcField
		gnY2KRead = 9
		gcY2KConfirm = Set("Confirm")
		gnY2KMess = 1
		gcY2KVar = Y2K_Var()

		*--------------------------------------------
		* set some necessary settings
		*--------------------------------------------
		Set Bell Off
		On Readerror Do Y2K_1
		gcY2KOnRead = On("Readerror")

		*--------------------------------------------
		* As Y2K needs the edit menu, we create one
		*--------------------------------------------
		DEFINE POPUP Y2KPopup 
		DEFINE BAR _med_copy OF Y2KPopup PROMPT "" ;
			KEY CTRL+C
		DEFINE BAR _med_slcta OF Y2kPopup PROMPT "" ;
			KEY CTRL+A
			
		*--------------------------------------------
		* we need the previous value. As in a BROWSE
		* the :F clause is required in :V, we need
		* this value to find out whether the value
		* changed, or not. Only if it changed, we
		* evaluate the character expression passed to
		* Y2K_Valid1(), to simulate a :V clause 
		* without :F.  
		* And we need this value in Y2K_When to check
		* wether an OKL fired, thus being able to 
		* restore the previous value.
		*--------------------------------------------
		Private lcVar
		gdY2KOldVal = Evaluate( Y2K_Var() )

	Return


	*===============================================
	* MESSAGE wrapper: Whenever the user enters a
	* new readlevel by selecting a menu item, valid
	* won't fire, WHEN only with user defined 
	* screens and ON KEY LABELS, but only MESSAGE
	* as well when a system tool like the calculator
	* has been activated.
	*===============================================
	Procedure Y2K_Mess
	Parameter tcMessage
	
		*--------------------------------------------
		* determine return value
		*--------------------------------------------
		Private lcMessage
		If Parameters() == 0
			lcMessage = ""
		Else
			lcMessage = tcMessage
		Endif
		
		*--------------------------------------------
		* Re-type date, if necessary. If the user has
		* entered an invalid date before she pressed
		* the OKL, or called another screen, we 
		* restore the old value in the variable and
		* fill the GET with 02/29/00. An alternative
		* is to clear the field. I decided not to do
		* this, because when the user entered 
		* 02/29/00 before, the date would also be 
		* cleared.
		*--------------------------------------------
		Private lcVar
		If Type("gnY2KMess") == "N"
			If	 gnY2KMess <= 0 ;
				and Type("glY2K") # "U"
				lcVar = Y2K_Var()
				If	  Empty( Evaluate(lcVar) ) ;
					and not Empty( gdY2KOldVal )
					Public glY2KRestore
					If Y2K_MemVar(lcVar)
						Store gdY2KOldVal to (lcvar)
					Else
						Replace next 1 ;
							(lcVar) With gdY2KOldVal
					Endif
					Do Y2K_Stuff with Y2k_022900()
				Endif
			Else
				gnY2KMess = Max( gnY2KMess-1, 0 )
			Endif
		Endif

	Return lcMessage
	
	*===============================================
	* Replacement for the built in CTOD() function
	* The original version causes two problems:
	* 1) it always add 19xx with 2-digit years
	* 2) it doesn't recognize "02/29/00".
	* Usage: Add the following line to every single
	* program or screen:
	*  #DEFINE  CTOD   Y2K_CTOD
	*===============================================
	Procedure Y2K_CToD
	Parameter tcDate
	
		*--------------------------------------------
		* get date string. If a short date has been
		* given, and this date equals to "02/29/00",
		* we create a long date string. FoxPro will
		* convert a 4-digit-year properly, even if
		* SET CENTURY is OFF.
		*--------------------------------------------
		Private lcDate, lcCentury
		If Len( tcDate ) <= 8
			lcCentury = Set("Century")
	  		Set Century Off
			If tcDate == Sys(10,2451604)
				Set Century On
				lcDate = Sys(10,2451604)
			Else
				lcDate = tcdate
			Endif
			Set Century &lcCentury
		Else
			lcDate = tcDate
		Endif

		*--------------------------------------------
		* convert into date format. Evaluate() is
		* necessarty, if #DEFINE CTOD is used and 
		* this function is included into the main
		* program. Strings are not replaced by
		* #DEFINE, so we can nevertheless use CtoD 
		* in a string	
		*--------------------------------------------
		Private ldDate
		ldDate = Evaluate("CToD(lcDate)")

		*--------------------------------------------
		* Handle roll-over with 2-digit years
		*--------------------------------------------
		If Len( tcDate ) <= 8
			If Year( ldDate ) < Y2K_ROLLOVER
				lddate = GoMonth(ldDate,1200)
			Endif
		Endif

	Return ldDate
	
	*===============================================
	* Replacement for the built-in LUPDATE() 
	* The original version causes two problems:
	* 1) it always add 19xx after 2000
	* 2) it doesn't recognize 02/29/2000.
	* Usage: Add the following line to every single
	* program or screen:
	*  #DEFINE  LUPDATE   Y2K_LUPDATE
	*===============================================
	Procedure Y2K_LUpdate
	Parameter tuAlias
	
		*--------------------------------------------
		* check for default work area
		*--------------------------------------------
		If Parameters() == 0
			tuAlias = Select()
		Endif

		*--------------------------------------------
		* table opened in given work area?
		*--------------------------------------------
		If	  Type( "tuAlias" ) == "N" ;
			and Empty(Dbf(tuAlias))
			Return {}
		Endif

		*--------------------------------------------
		* get date. Causes error, if tuAlias doesn't
		* exist.
		*--------------------------------------------
		Private ldDate
		ldDate = Evaluate("LUpdate(tuAlias)")
		If Empty(lddate)
			ldDate = Y2K_022900()
		Else
			If Year( ldDate ) < Y2K_ROLLOVER
				lddate = GoMonth(ldDate,1200)
			Endif
		Endif
		
	Return ldDate
