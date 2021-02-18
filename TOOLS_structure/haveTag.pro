;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
; NAME: haveTag
; PURPOSE: Search in a structure or list of structure, the presence of tag(s) 
;	given in argument, and return a scalar or an array of scalars with the value 
;	of 1 (found) or 0 (not found).
; INPUTS:
;	struct: Structure or n-elements list of structures in which you want to know 
;		if it have the tag(s).
;	tag: String or m-elements array of strings corresponding to tag(s) to 
;		research in struct.
; OUTPUTS:
;	tag_found: Scalar or n by m array of scalars with the test result value of 
;		1 or 0 depending the tag is found or not.
;-------------------------------------------------------------------------------
FUNCTION haveTag, struct, tag
	
	n_struct = N_ELEMENTS(struct)
	IF n_struct EQ 0 THEN RETURN, 0
	
	n_tag = N_ELEMENTS(tag)
	tag_found = INTARR(n_struct, n_tag)
	
	FOR i=0, n_struct-1 DO BEGIN
		struct_tags = TAG_NAMES(struct[i])
		FOR k=0, n_tag-1 DO BEGIN
			IF WHERE(struct_tags EQ STRUPCASE(tag[k])) NE -1 THEN tag_found[i,k]=1
		ENDFOR
	ENDFOR

	RETURN, tag_found
END
