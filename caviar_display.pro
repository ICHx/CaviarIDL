;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
; NAME: CAVIAR_DISPLAY
; PURPOSE: Display the image and overlay information
;-------------------------------------------------------------------------------
PRO caviar_display
	
	COMMON CAVIAR_DATA, image, catstars, imgStars, planets
	COMMON CAVIAR_PARAMS, dispParams, imgDraw, dispProName, starsCatalogs, find_imgStars_params
	
	; Check that an image has been loaded
	IF ISA(image) EQ 0 THEN MESSAGE, "Please load an image first."
	
	; Save current window and set image window:
	old_win = !D.WINDOW
	WSET, image.window
	
	; Display the image:
	ns     = image.ns
	nl     = image.nl
	zf     = imgDraw.ZFACTOR
	interp = imgDraw.INTERP
	offset = imgDraw.OFFSET

	minim = min(image.raw)
	maxim = max(image.raw)
	img = (!D.TABLE_SIZE - 1) * (image.raw - minim) / (maxim - minim)
	
	IF zf NE 1 THEN img = REBIN(img, ns*zf, nl*zf, SAMPLE=1-interp)
	TV, img, 0-offset[0], 0-offset[1], /ORDER
	
	
	; Display data specific to current main tool:
	CASE dispProName OF
		'caviar_repoint_display': caviar_repoint_display
		'caviar_satPos_display': caviar_satpos_display
		ELSE:
	ENDCASE
	
	; Restore previous window:
	WSET, old_win
	
END
