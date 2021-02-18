;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
; NAME: CAVIAR_DISPLAY
; njc new routine
; PURPOSE: Re-display the image and overlay information
;-------------------------------------------------------------------------------
PRO caviar_redisplay
	
	COMMON CAVIAR_DATA, image, catstars, imgStars, planets, rings
	COMMON CAVIAR_PARAMS, dispParams, imgDraw, dispProName, starsCatalogs, find_imgStars_params
	
	; Test that an image have been loaded
	IF ISA(image) EQ 0 THEN MESSAGE, "You need to load an image first!"
	
	; Save current window and set image window:
	old_win = !D.WINDOW

        TVLCT, saved_colors, /GET

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
	
;njc from caviar_repoint_display (exclude the imgstars from FIND):
        caviar_catstars_display, catstars, dispParams.catstars, nl, imgDraw.OFFSET, ZOOMFACTOR=zf
        caviar_satellites_display, planets, dispParams.planets, nl, imgDraw.OFFSET, ZOOMFACTOR=zf
        caviar_rings_display, rings, dispParams.rings, nl, imgDraw.OFFSET, ZOOMFACTOR=zf

        TVLCT, saved_colors

	; Restore previous window:
	WSET, old_win
	
END
