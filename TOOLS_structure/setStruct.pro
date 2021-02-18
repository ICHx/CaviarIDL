;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
; NAME: SETSTRUCT
; PURPOSE: Add tags and values pairs to an existing structure.
;		   If tags already exists, the values will be updated.
;		   If tags didn't exists, tags and values pairs will be added.
; INPUTS:
;	struct: An existing structure with any content in which will be added or updated value (and tag).
;	tag: Scalar or array of strings corresponding to structure tag(s) to add/update.
;	val: Scalar, array or list of any type of data that can be add to a structure.
;		 If the tag already exists in the structure, make sure the value have compatible type!
; OUTPUT:
;	struct: The updated structure.
; MODIFICATIONS:
;	2012, October 25			MEUNIER L.E. - IMCCE (OBSPM)
;		- Written
;---------------------------------------------------------------------------------------------------
PRO setStruct, struct, tag, val
	
	IF N_ELEMENTS(struct) EQ 0 THEN struct={}
	nTag = N_ELEMENTS(tag)
	nVal = N_ELEMENTS(val)
	IF nTag NE 1 && nTag NE nVal $
	THEN MESSAGE, "'TAG' and 'VAL' parameters must have the same size."
	
	IF nTag EQ 1 THEN BEGIN	;&& ISA(val, 'LIST') EQ 1 $

		s = CREATE_STRUCT(tag, val)
		IF haveTag(struct, tag) $
		THEN STRUCT_ASSIGN, s, struct, /NOZERO , /VERBOSE $
		ELSE struct = CREATE_STRUCT(struct, tag, val)

	ENDIF ELSE BEGIN

		FOR i=0, nTag-1 DO BEGIN
			s = CREATE_STRUCT(tag[i], val[i])
			IF haveTag(struct, tag[i]) $
			THEN STRUCT_ASSIGN, s, struct, /NOZERO , /VERBOSE $
			ELSE struct = CREATE_STRUCT(struct, s)
		ENDFOR

	ENDELSE
END
