;;	casslabels_define.pro
;;       Package defining CassLabels class and access methods
;; This is meant to stand in for associative arrays, which IDL lacks :-(
;;       Kevin Beurle    8th September 1998
;;	Version Tag: $Id: casslabels__define.pro,v 1.1 2004/02/16 16:18:44 mwe Exp $

; A CassLabels object will contain three arrays, one each for
;	Keyword (string)
;	Value (string)
;	Quoted (boolean) Was the value enclosed in quotes 'like this'

; We don't know how many label pairs we will read, so we will guess 200
; for the moment to size the arrays. (Previously 100 until November 2000).
; Arrays in objects don't seem to be dynamically extensible, so we might
; have to change the object contents to pointers and manipulate dynamic
; arrays on the heap if this becomes an issue



FUNCTION CassLabels::Have, KeyWord
	Index = WHERE (( self.Keyword EQ KeyWord ), Count)
	RETURN,( Count NE 0 )
END

FUNCTION CassLabels::Get, KeyWord
	Index = WHERE (( self.Keyword EQ KeyWord ), Count)
	IF Count EQ 0 THEN BEGIN								; Keyword not found
		CISSCAL_Log, 'Keyword ' + KeyWord + ' not found!'
		RETURN, -1
	ENDIF ELSE RETURN, self.Value[Index]
END

FUNCTION CassLabels::Set, KeyWord, Value, Quoted, New=New

	; Unless its being quoted, we must trim spaces
	IF Quoted EQ 0 THEN BEGIN
		Value = STRTRIM(Value,2)
	ENDIF

	;	Have we already seen this keyword ?
	;	If multiple matches, then just consider the most recent of them
	Indices = WHERE (( self.Keyword EQ KeyWord ), Count)
	Index = Count GT 1 ? Indices[Count-1] : Indices

	IF Count EQ 0 THEN BEGIN	; Keyword not found: insert a new one
		Index = self.NLabels
		self.NLabels = self.NLabels + 1
		IF self.ObjDebugLevel THEN $
			CISSCAL_Log, 'Keyword ' + KeyWord + '(slot', BYTE(Index), ')'+ $
				' inserting [' + Value + ']'
		self.Keyword[Index] = KeyWord
	ENDIF ELSE BEGIN
		; LBLSIZE is a special case... accumulate these values, not replace
		IF KeyWord EQ 'LBLSIZE' THEN BEGIN
			self.Value[Index] = self.Value[Index] + Value
		ENDIF ELSE IF KEYWORD_SET(New) THEN BEGIN
			;	We're asserting that this should be a new label, but we've seen
			;	the same keyword before... so handle this differently: append a new
			;	keyword/value pair for  this as though keyword was *not* known already
			CISSCAL_Log, 'Appending repeated Label: ' + KeyWord + $
				' last value [' + self.Value[Index] + '] now [' + Value + ']'
			Index = self.NLabels
			self.NLabels = self.NLabels + 1
			self.Keyword[Index] = KeyWord
		ENDIF ELSE BEGIN
			; We're changing a pre-existing value for a known key
			IF ( self.Value[Index] NE Value) THEN BEGIN
				IF self.ObjDebugLevel THEN $
		  			CISSCAL_Log, 'Keyword ' + KeyWord + '(slot', BYTE(Index), ')' + $
					' change from [' + self.Value[Index] + '] to ['+ Value + ']'
		  	ENDIF
		ENDELSE
	ENDELSE

	self.Value[Index] = Value
	self.Quoted[Index] = Quoted
	RETURN, self.Value[Index]
END

;########## CassLabels::LabelArray()
; Access function returning a [[Key, val], [Key, val]...] array
FUNCTION CassLabels::LabelArray
	StrArray = STRARR(2, self.NLabels)
	StrArray[0,*] = self.Keyword[0:self.NLabels-1]
	StrArray[1,*] = self.Value[0:self.NLabels-1]
	RETURN, StrArray
END

;########## CassLabels::ParseAll
; Parse all keywords from TextBuf into this object.
; Should expect that all informations should be new rather than modifying existing values,
; so assert this when using self->Set()
PRO CassLabels::ParseAll, TextBuf, Quiet=Quiet

	Quote = ''''

	Index = 0
	LabelCnt = 0

	WHILE ( STRPOS(TextBuf, '=', Index) NE -1) DO BEGIN
		; There is at least one more Keyword=Label pair...
		; split at the first '=', then take the LHS for keyword
    	EQPOS = STRPOS(TextBuf, '=', Index)
    	Keyword = STRTRIM(STRMID(TextBuf, Index, EqPos-Index),2)

;       If the value does not begin with a quote, then it is simple token
;       so grab it from its beginning up to the next whitespace.
;       Otherwise its a compound value containing whitespace and
;       delimited by quotes so grab it from the character after the
;       opening quote up until the character before the closing quote

		IF ( STRMID(TextBuf, EqPos+1, 1) NE Quote ) THEN BEGIN
			Delim = ' '			; Terminating delim is space
			FirstPos = EqPos+1
		ENDIF ELSE BEGIN
			Delim = Quote		; Terminating delim is '
			FirstPos = EqPos+2	; Skip leading delimiter
		ENDELSE

		DelimPos = STRPOS(TextBuf, Delim, FirstPos)
		KeyVal = STRMID(TextBuf, FirstPos, DelimPos-FirstPos)

		IF self.ObjDebugLevel GE 2 THEN $
			CISSCAL_Log, 'Parsed [' + KeyVal + '] for keyword [' + Keyword + '] delimited by [', Delim, ']'
			
		Index = DelimPos+1
		LabelCnt = LabelCnt + 1
		Res = self->Set(KeyWord, KeyVal, Delim EQ Quote, New=1)
	ENDWHILE
	self.LblSize = self->Get('LBLSIZE')	; for convenience (?)
	IF NOT KEYWORD_SET(Quiet) THEN CISSCAL_Log, 'Parsed', LabelCnt, ' labels'
END



PRO CassLabels::LogAll
	FOR I = 0, self.NLabels-1 DO BEGIN
		CISSCAL_Log, 'Slot', I, ' Key [' + self.Keyword[I] + '] value ['+ self.Value[I] + ']'
	ENDFOR
END


;########## CassLabels::toString()
;	Disgorge ourself into a long string of VICAR labels for output
;	If this turns out to be longer than will fit into the space
;	 we declare (self.LblSize), then increase this by RECSIZE
;	Note that toString() may change the required
;	 labelsize (i.e. if new labels go beyond a record boundary)
;	 therefore, update LBLSIZE from the result for safety
FUNCTION CassLabels::toString
  Quote = ''''
  myRecSize = self->Get('RECSIZE')
  self.LblSize = myRecSize	; start by allowing one record for labels
  Junk = self->Set('LBLSIZE',self.LblSize,0)
  REPEAT BEGIN
    Out = ''
    FOR I = 0, self.NLabels-1 DO BEGIN
      IF self.Quoted[I] THEN Delim = Quote ELSE Delim = ''
      Pair = self.Keyword[I] + '=' + Delim + STRING(self.Value[I]) + Delim
      Out = Out + Pair + '  '
    ENDFOR
    LabelsFit = STRLEN(Out) LE self.LblSize
    IF NOT LabelsFit THEN BEGIN
      CISSCAL_Log, 'Increasing label space to fit all labels from ' + STRING(self.LblSize) $
	+ ' to ' + STRING(self.LblSize+myRecSize)
      self.LblSize  = self.LblSize + myRecSize
      Junk = self->Set('LBLSIZE',self.LblSize,0)
    ENDIF
  ENDREP UNTIL LabelsFit 
  RETURN, Out
END





;########## CassLabels::Init()
; Runs automatically during execution of OBJ_NEW('CassLabels')
; If called with an argument, use that as a buffer to parse from
; Return TRUE if we're happy, otherwise FALSE
FUNCTION CassLabels::Init, TxtBuf, DebugFlag=DebugFlag, Quiet=Quiet
	Status = 1		; TRUE means we're happy
	IF (N_PARAMS() GT 0) THEN BEGIN
	    IF NOT KEYWORD_SET(Quiet) THEN $
	    	CISSCAL_Log, 'Auto parsing buffer: [' + STRMID(TxtBuf,0,16) + '...]
	    self->ParseAll, TxtBuf, Quiet=Quiet
	ENDIF
	IF KEYWORD_SET(DebugFlag) THEN self.ObjDebugLevel = DebugFlag
	RETURN, Status
END


;########## CassLabels::Cleanup()
; Runs automatically during OBJ_DESTROY of a CassLabels object
PRO CassLabels::Cleanup
	; No action is actually needed!
	CISSCAL_Log, 'Cleanup for destruction of a CassLabels...'
END


PRO CassLabels__Define
  tmp = { CassLabels, $
		  ObjDebugLevel: 0, $	; Class debugging info level 0 is OFF
		  LblSize: 0, $			; Size in bytes of the VICAR label block in the relevant VICAR 
		  						; image file. It is always a multiple of the record size.
		  NLabels: 0, $
		  Keyword: STRARR(200), $
		  Value:   STRARR(200), $
		  Quoted:  BYTARR(200) $
		}
END
