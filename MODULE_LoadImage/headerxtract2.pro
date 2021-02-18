;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
; NAME: HEADERXTRACT
; PURPOSE: Obtain the value of a parameter in an image header.
;
; CALLING SEQUENCE:
;   result = HDRXTRACT(header, tag, type, opendlmtr, closedlmtr [, EXACT_MATCH])   
;
; INPUTS:
;	header = String array of dimensions 1 or 2 by n. Image header array. 
;		E.g. as returned by READFITS
;	label = Scalar, array or list of strings. String or regular expression 
;		representative of the label of the parameter(s) to return.
;		'HEADERXTRACT' use the IDL's 'STREGEX' routine to search the label, so
;		don't forget the escape charater '\' to use as a character one of these 
;		meta character: 
;			'.', '[', ']', '\', (', ')', '*', '+', '?', '{', '}', '|', '^', '$'
;	vtype = Scalar, array or list of strings of the same size as 'tag'. 
;		Type of the result list elements. 
;		E.g. 'STRING', 'INT', 'LONG', 'FLOAT', 'DOUBLE'
;	
; OPTIONAL INPUTS:
;	opendlmtr/closeDlmtr = Scalar, array or list of strings or regular
;		expressions which indicates the beginning/the end of the value to extract.
;		In case of regular expression, don't use subexpression and don't forget
;		the escape charater '\' to use as a character one of these meta character: 
;			'.', '[', ']', '\', (', ')', '*', '+', '?', '{', '}', '|', '^', '$'
;		Note that these characters wont be part of the result! 
;		By default open/close delimiters are respectively the beginning/the end
;		of the header line (witch is the second column in case of 2-columns header).
;		To specify the default for some of the values to extract, you can use an
;		empty string ''.
;
; OPTIONAL KEYWORDS: 
;	NOTRIM = Set this keyword to keep the leading and trailing blanks between
;		the delimiters.
;
; OUTPUTS:
;   result = IDL object list of the same dimension as 'tag'. Contains the value
;		corresponding to the parameter label found in the header with the type 
;		specified in input. If a parameter label cannot be found or the value
;		cannont be extract, the element result list will be of type '!NULL'. 
;
; EXAMPLE:
;   Given an image header, h, return the values of "EXPOSURE", 
;	"NS", "NL", "INSTRUMENT" values into a list variable.
;	
;	IDL> labels = ['EXPOSURE','NS','NL','INSTRUMENT']
;	IDL> types = ['DOUBLE','INT','INT','STRING']
;	IDL> opendlmtr  = ['=','=','=','"']
;	IDL> closedlmtr = ['', '', '', '"']
;   IDL> valuesList = headerXtract(h , labels, types, opendlmtr, closedlmtr)
;
; PROCEDURE:
;	Label string or regular expression is searched in the header with the IDL's
;	'STREGEX' routine. In case of a 2-columns header, the label is search in the
;	first column. 
;	When the label is matched, the value is read from the header line from which 
;	have been extracted the substring delimited by the open and close delimiters, 
;	cleaned of leading and trailing blanks (if NOTRIM keyword haven't been set).
;	If no delimiters have been defined, the substring is the entire line or the
;	entire second column string for the case of a 2-columns header.
;	If the label is found in multiple lines of the header, the routine will try 
;	to extract the value from the line at the first index found, and pass to the
;	next index until the value can be read with the proper type and framed by
;	the specified delimiters.
;	When a value is read succesfully, the value is added to the result list,
;	otherwise the !NULL value is added to the list. Then, the next label is 
;	searched and so forth for all the input labels.
;
; MODIFICATION HISTORY:
;	2013 - Written by L-E. Meunier (IMCCE/OBSPM)
;	2014, March		  L-E. Meunier (IMCCE/OBSPM)
;		- Remove the 'EXACT_MATCH' keyword because its function can be done with
;		the anchors '^' and '$' in the 'label' regular expression.
;		- Replace the loop on the header lines by using the 'STREGEX' routine
;		on the entire header (or entire first column for 2-columns header).
;		- Add the 'NOTRIM' keyword to keep the leaving and trailing blanks in
;		the returned string value.
;		- Add possibility to set only one element for 'VTYPE' argument, used for
;		every elements of 'LABEL' argument. 
;-------------------------------------------------------------------------------
FUNCTION headerXtract, header, label, vtype, dlmtr1, dlmtr2, $
					   NOTRIM=notrim, SILENT=silent
	
	IF ~ISA(header, 'STRING') THEN MESSAGE, '"HEADER" must be a string array of dimensions: 1 or 2 by n.'
	IF ~ISA(label, 'STRING')  THEN MESSAGE, '"LABEL" must be a string or an array of strings.'
	IF ~ISA(vtype,'STRING')   THEN MESSAGE, '"VTYPE" must be a string or an array of strings.'
	
	hdrsize = SIZE(header)
	IF hdrsize[0] GT 2 THEN MESSAGE, '"HEADER" parameter have to be 1 or 2 by n array.'
	
	ncol = hdrsize[hdrsize[0]-1]
	nline = hdrsize[hdrsize[0]]
	nlbl = N_ELEMENTS(label)
	ntype = N_ELEMENTS(vtype)
	IF ntype NE nlbl && ntype NE 1 THEN MESSAGE, '"TYPE" and "LABEL" arguments must have the same size.'
	
	
	
	IF N_ELEMENTS(dlmtr1) EQ 0 THEN dlmtr1 = ''
	ndlmtr1 = N_ELEMENTS(dlmtr1)
	IF ndlmtr1 NE nlbl && ndlmtr1 NE 1 THEN MESSAGE, '"DLMTR1" and "LABEL" arguments must have the same size.'
	
	IF N_ELEMENTS(dlmtr2) EQ 0 THEN dlmtr2 = ''
	ndlmtr2 = N_ELEMENTS(dlmtr2)
	IF ndlmtr2 NE nlbl && ndlmtr2 NE 1 THEN MESSAGE, '"DLMTR2" and "LABEL" arguments must have the same size.'
	
	valList = LIST(LENGTH=nlbl)
	nfound = 0
	
	verbose = 1-KEYWORD_SET(silent)
	
	; Iterate trough labels:
	FOR k=0, nlbl-1 DO BEGIN
		;***********************************************************************
		; Search the label (regular expression) in the header:
		;***********************************************************************
		CASE ncol OF
			1: lblfound = STREGEX(header, label[k], /BOOLEAN)
			2: lblfound = STREGEX(header[0,*], label[k], /BOOLEAN)
		ENDCASE
		index = WHERE(lblfound, count)

		IF count EQ 0 THEN BEGIN
			IF verbose THEN MESSAGE, /INF, "Cannot find label '"+label[k]+"'."
			CONTINUE
		ENDIF
		IF count EQ 1 && verbose THEN BEGIN
			MESSAGE, /INF, "Found label '"+label[k]+"' at line: "+STRING(index,'(I0)')
		ENDIF
		IF count GT 1 && verbose THEN BEGIN
			MESSAGE, /INF, "Found label '"+label[k]+"' at lines: "+STRJOIN(STRING(index,'(I0)'), ', ')
			PRINT, '  The returned value will be the first one that can be extracted with the proper type and delimiters set in input.'
		ENDIF
		
		;***********************************************************************
		; Set value type and delimiters:
		;***********************************************************************
		begDlmtr = (ndlmtr1 EQ nlbl) ? dlmtr1[k] : dlmtr1[0]
		endDlmtr = (ndlmtr2 EQ nlbl) ? dlmtr2[k] : dlmtr2[0]
		valType  = (ntype   EQ nlbl) ? vtype[k]  : vtype[0]
		
		; Search the value where the label have been found:			
		FOREACH i, index DO BEGIN			
			print, i
			;*******************************************************************
			; Extract the string that contains the value from the header
			;*******************************************************************
			CASE ncol OF
				1: str = header[i]
				2: str = header[1,i]
			ENDCASE
			str2read = STREGEX(str, begDlmtr+'(.+)'+endDlmtr, /EXTRACT, /SUBEXPR)
			
			; Remove leading and trailing blanks:
			str2read = KEYWORD_SET(notrim) ? str2read[1] : STRTRIM(str2read[1], 2)
			;*******************************************************************
			; Initialize the value to read:
			;*******************************************************************
			CASE valType OF
				'STRING': 	val = ''
				'INT':		val = 0S
				'LONG':		val = 0L
				'FLOAT':	val = 0.0
				'DOUBLE':	val = 0.0D
			ENDCASE
			
			;*******************************************************************
			; Read the value in the substring:
			; In case of error, display warning message and pass to next index or label.
			;*******************************************************************
			CATCH, error
			IF error NE 0 THEN BEGIN
				IF verbose THEN BEGIN
					PRINT, !ERROR_STATE.MSG_PREFIX+!ERROR_STATE.MSG
					msg = "Cannot read '"+valType+"' value delimited by '"+begDlmtr+"' and '"+endDlmtr+"' from the substring '"+str2read+"'"
					MESSAGE, /CONTINUE, msg
					PRINT, "  Full string = '", str, "'"
				ENDIF
				CONTINUE
			ENDIF
			;print, "CACA ", str, str2read, val
			READS, str2read, val
			
			;*******************************************************************
			; Add the value to the returned list:
			;*******************************************************************
			valList[k] = val
			nfound++
			BREAK
		ENDFOREACH
	ENDFOR
	
	RETURN, valList
END
