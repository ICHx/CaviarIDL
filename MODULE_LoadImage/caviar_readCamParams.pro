
PRO caviar_readCamParams, cam_lname, valStruct, NFOUND=nfound

	IF  NOT ISA(cam_lname,'STRING') $
	THEN MESSAGE, '"CAM_LNAME" must be a scalar string.'

	IF NOT ISA(valStruct, /ARRAY) $
	THEN MESSAGE, '"VALSTRUCT" must be a structure with at least one tag and one initialized value.'
	
	;***************************************************************************
	; Extract cameras prameters file into an array:
	;***************************************************************************
	file = GETENV('CAVIAR_CAMPARAMS_FILE')
	OPENR, lun, file, /GET_LUN
	READF, lun, (lines = STRARR(FILE_LINES(file)))
	CLOSE, lun
	FREE_LUN, lun
	
	;***************************************************************************
	; Find the beginning and the end of the camera parameters block:
	;***************************************************************************
	begBlock = WHERE(STREGEX(lines, '(BEGIN_OBJECT).*('+cam_lname+')', /BOOLEAN, /FOLD_CASE) EQ 1)
	endBlock = WHERE(STREGEX(lines, '('+cam_lname+').*(END_OBJECT)', /BOOLEAN, /FOLD_CASE) EQ 1)
	
	; Verify that the camera parameters block exists:
	IF begBlock[0] EQ -1 || endBlock[0] EQ -1 $
	THEN MESSAGE, cam_lname+" camera parameters block hasn't been found or has bad formatting in the file: "+file
	
	; Verify that the beginning of the block is unique:
	IF N_ELEMENTS(begBlock) NE 1 $
	THEN MESSAGE, "Multiple occurences of the "+cam_lname+" camera parameters block has been found. Verify the file: "+file
	
	; Verify that the end of the block is unique and after the beginning:
	FOR i=0, N_ELEMENTS(endBlock)-1 DO BEGIN
		IF (endBlock GT begBlock)[i] EQ 1 THEN BEGIN 
			endBlock = endBlock[i]
			BREAK
		ENDIF
	ENDFOR
	IF endBlock LT begBlock $
	THEN MESSAGE, cam_lname+" camera parameters block has bad formatting in the file: "+file
	
	begBlock = begBlock[0]
	endBlock = endBlock[0]
	
	;***************************************************************************
	; Find input tags in the selected camera parameters block and extract
	; corresponding values with the specified type.
	;***************************************************************************
	tags = TAG_NAMES(valStruct)
	ntags    = N_ELEMENTS(tags)
	nfound   = 0
	valFound = LIST()
	
	FOR k=begBlock+1, endBlock-1 DO BEGIN
		; Get a new line and remove commented part from it:
		line = ( STRSPLIT(lines[k],'#',/EXTRACT) )[0]
		
		; If 'line' is empty, pass to next line:
		IF STRLEN(line) EQ 0 THEN CONTINUE
		
		; Extract 'tag' and 'val' parts from the line string:
		lineParts = STRSPLIT(line,'=',/EXTRACT)
		
		; If 'line' contains more than one "=", pass to next line:
		IF N_ELEMENTS(lineParts) NE 2 THEN CONTINUE
			
		FOR i=0, ntags-1 DO BEGIN
			; If the tag has already been found, pass to next line:
			IF WHERE(valFound EQ i) NE -1 THEN CONTINUE 

			; Is there 'tag[i]' in the line in front of the '='?
			IF NOT STREGEX(lineParts[0], tags[i], /BOOLEAN) THEN CONTINUE

			CATCH, error
			IF error NE 0 THEN BEGIN
				CATCH, /CANCEL
				PRINT, !ERROR_STATE.MSG_PREFIX, !ERROR_STATE.MSG
				MESSAGE, "Cannot read "+tags[i]+" from '"+line+"'", /CONTINUE
				CONTINUE
			ENDIF
			READS, lineParts[1], (val=valStruct.(i))
			valStruct.(i) = val
			
			nfound++
			valFound.ADD, i
			BREAK
		ENDFOR
		IF nfound EQ ntags THEN BREAK	
	ENDFOR

	RETURN
END
