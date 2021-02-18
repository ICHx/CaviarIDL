;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
; NAME: CAVIAR_UPDCOORDS
;
; INPUTS:
;	object: List of structures containing the 'XCOORD' and 'YCOORD' tags to 
;			update along with pointing matrix value.
;	vobs: Velocity of the object relative to the observer (i.e. the spacecraft)
;
;-------------------------------------------------------------------------------
PRO caviar_updtCoords, object, vobs
	
	
	IF (nobj = N_ELEMENTS(object)) EQ 0 THEN RETURN
	
	IF ISA(object, 'LIST') NE 1 $
	THEN MESSAGE, "WARNING: Attempting to update coordinates of an undefined object"
	
	CATCH, error
	IF error NE 0 THEN BEGIN
		PRINT, !ERROR_STATE.MSG_PREFIX+!ERROR_STATE.MSG
		RETURN
	ENDIF
	
	
	npts = N_ELEMENTS((object[0]).RA)
	
	IF npts EQ 1 THEN BEGIN
		
		ra = DBLARR(nobj)
		dec = DBLARR(nobj)		
		FOR i=0L, nobj-1 DO BEGIN
			obj_i = object[i]
			ra[i]  = obj_i.RA
			dec[i] = obj_i.dec
		END			
		
		; Convert RA/dec to x/y image coordinates
		radec2slcoord, ra, dec, sample, line, CORRECT_STELAB=vobs
		
		; Set "xcoord" and "ycoord" tag's value to object i structure.
		FOR i=0L, nobj-1 DO BEGIN
			obj_i = object[i]
			obj_i.xcoord = sample[i]
			obj_i.ycoord = line[i]
			object[i] = obj_i
		ENDFOR
		
	ENDIF ELSE BEGIN
		
		FOR i=0L, nobj-1 DO BEGIN
			obj_i = object[i]
			radec2slcoord, obj_i.ra, obj_i.dec, sample, line, CORRECT_STELAB=vobs
			obj_i.xcoord = sample
			obj_i.ycoord = line
			object[i] = obj_i
		ENDFOR
		
	ENDELSE
END	
