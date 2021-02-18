;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;	List of procedure of this file:
;		- PRO		caviar_satPos_display
;		- PRO 		caviar_satPos_save
;		- PRO 		caviar_satPos_setResults
;-------------------------------------------------------------------------------

;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
; NAME: CAVIAR_SATPOS_DISPLAY
; PURPOSE: 
;-------------------------------------------------------------------------------
PRO caviar_satPos_display
	
	COMMON CAVIAR_DATA, image, catstars, imgStars, planets
	COMMON CAVIAR_PARAMS, dispParams, imgDraw, dispProName, starsCatalogs, find_imgStars_params
	COMMON CAVIAR_SATPOS, selSatIndex, wSatTblID, wSaveLbl, wSLFbaseYsize 
	COMMON CAVIAR_SATLIMBFIT, satModel, satLimb
	
	IF selSatIndex EQ !NULL THEN RETURN
	; If satellites just have been loaded, the 'selSatIndex' must be updated:
	IF selSatIndex GE N_ELEMENTS(planets) THEN selSatIndex = 0
	
	nl = image.nl
	zoomFactor   = imgDraw.ZFACTOR
	imgWinOffset = imgDraw.OFFSET
	sat = planets[selSatIndex]
	
	old_win = !D.WINDOW			; Save current window
	TVLCT, saved_colors, /GET	; Save colors
	
	;***************************************************************************
	; Display limb/terminator/equator:
	x=!NULL & y=!NULL & colors=!NULL
	colorsIndex = color(['CYAN','SPRING GREEN','ORANGE','MAGENTA'], /SILENT)
	IF haveTag(satModel, 'SLLIMB') THEN BEGIN
		nlimb = N_ELEMENTS(satModel.slLimb[*,0])
		x=[x, zoomFactor*satModel.slLimb[*,0]            - imgWinOffset[0]]
		y=[y, zoomFactor*((nl-1) - satModel.slLimb[*,1]) - imgWinOffset[1]]
		colors = [colors, REPLICATE(colorsIndex[0],nlimb)]
	ENDIF
	IF haveTag(satModel, 'SLTERM') THEN BEGIN
		nterm = N_ELEMENTS(satModel.slTerm[*,0])
		xt = zoomFactor * satModel.slTerm[*,0]            - imgWinOffset[0]
		yt = zoomFactor * ((nl-1) - satModel.slTerm[*,1]) - imgWinOffset[1]
		x=[x,xt]
		y=[y,yt]
		colors = [colors, REPLICATE(colorsIndex[1],nterm)]
	ENDIF		
	IF haveTag(satModel, 'SLEQUA') THEN BEGIN
		nequa = N_ELEMENTS(satModel.slEqua[*,0])
		xe = zoomFactor * satModel.slEqua[*,0]            - imgWinOffset[0]
		ye = zoomFactor * ((nl-1) - satModel.slEqua[*,1]) - imgWinOffset[1]
		x=[x,xe]
		y=[y,ye]
		colors = [colors, REPLICATE(colorsIndex[2],nequa)]
	ENDIF
	
	IF N_ELEMENTS(satLimb) NE 0 THEN BEGIN
		nedge = N_ELEMENTS(satLimb[*,0])
		xedge = zoomFactor * satLimb[*,0] - imgWinOffset[0]
		yedge = zoomFactor * ((nl-1) - satLimb[*,1]) - imgWinOffset[1]
		x=[x,xedge]
		y=[y,yedge]
		colors = [colors, REPLICATE(colorsIndex[3],nedge)]
	ENDIF	
	IF x NE !NULL && y NE !NULL $
	THEN PLOTS, x, y, /DEVICE, PSYM=3, SYMSIZE=10, COLOR=colors
	
	;***************************************************************************
	; Plot yellow square arround the satellite center from ephemerides:
	x = zoomFactor * sat.xcoord - imgWinOffset[0]
	y = zoomFactor * ((nl-1) - sat.ycoord) - imgWinOffset[1]
;	PLOTS, x, y, PSYM=6, SYMSIZE=zoomFactor*1.5, COLOR=color('DARK YELLOW 2', /SILENT), /DEVICE
;njc Don't zoom the square box:
        PLOTS, x, y, PSYM=6, SYMSIZE=1.5, COLOR=color('DARK YELLOW 2', /SILENT), /DEVICE
	
	; Plot a red cross on the computed satellite centroid:
	IF haveTag(sat, "XCENT") && haveTag(sat, "YCENT") $
	THEN BEGIN
		x = zoomFactor * sat.xcent - imgWinOffset[0]
		y = zoomFactor * ((nl-1) - sat.ycent) - imgWinOffset[1]
;		PLOTS, x, y, PSYM=1, SYMSIZE=zoomFactor*1, COLOR=color('RED', /SILENT), /DEVICE
;njc Don't zoom the symbol:
               PLOTS, x, y, PSYM=1, SYMSIZE=1, COLOR=color('RED', /SILENT), /DEVICE
	ENDIF
	
	;***************************************************************************
	; Restore colors and default window
	TVLCT, saved_colors
	WSET, old_win
END

;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
; NAME: CAVIAR_SATPOS_SAVE
; PURPOSE: Save astrometry results in *.QMPF and *.CSV files.
; INPUT:
;	format: Scalar string containing the saving file format ("QMPF").
;-------------------------------------------------------------------------------
PRO caviar_satPos_save, format
	
	COMMON CAVIAR_GUI, wCAVIARtlb
	COMMON CAVIAR_DATA, image, catstars, imgStars, planets
	COMMON CAVIAR_SATPOS, selSatIndex, wSatTblID, wSaveLbl, wSLFbaseYsize 
	
	; Save results in QMPF format:
	CASE format OF
		"QMPF": caviar_save2qmpf, FLAG=flag
	ENDCASE
	IF flag THEN BEGIN
		PRINT, "Results saved in ", format, " file."
		WIDGET_CONTROL, wSaveLbl, SET_VALUE=" Done!"
	ENDIF
	; Save results in CSV format:
	file = image.path+PATH_SEP()+"results.csv"
	msg = ["Would you like to save fitting results in CSV file?", file, "", $
		   "!! If the file already exist, the data will be append !!"]
	res = DIALOG_MESSAGE(msg, /QUESTION, DIALOG_PARENT=wCAVIARtlb)
	
	IF STRUPCASE(res) EQ "YES" THEN BEGIN
		p = (planets[selSatIndex])
		s = p.METHOD
		STRPUT, s, "_", STREGEX(s, " ")
		
		OPENW, lun, file, /GET_LUN, /APPEND
		PRINTF, lun, $
			FORMAT='(3(A, X),F0.2,X,2(F0.7,X),5(F0.4,X),2(F0.7,X),4(F0.4,X))', $
			image.NAME, p.NAME, s, p.PIXSIZE, $
			p.RA_CENT*180.0D/!DPI, p.DEC_CENT*180.0D/!DPI, $
			p.XCENT, p.YCENT, p.SIGMA, p.xoffset, p.yoffset, $
			p.RA*180.0D/!DPI, p.DEC*180.0D/!DPI, p.XCOORD, p.YCOORD, $
			p.XRES, p.YRES
		FREE_LUN, lun
		PRINT, FORMAT='(A,/,A)',"Results saved in CSV file: ", file
	ENDIF
END


;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
; NAME: CAVIAR_SATPOS_SETRESULTS
; PURPOSE: Set results values to the computed satellite structure.
; INPUTS:
;	xcent/ycent: Computed satellite centroid position (in pixels)
;	sigma: Computed satellite mean residual (in pixels)
; OPTIONAL INPUTS:
;	xoffset/yoffset: Center of light/center of figure offset for centroid 
;					 computation (unresolved satellite)
;-------------------------------------------------------------------------------
PRO caviar_satPos_setResults, method, xyCent, sigma, xoffset, yoffset
	
	COMMON CAVIAR_DATA, image, catstars, imgStars, planets
	COMMON CAVIAR_SATPOS, selSatIndex, wSatTblID, wSaveLbl, wSLFbaseYsize 
	
	IF N_ELEMENTS(xoffset) EQ 0 THEN xoffset=0D
	IF N_ELEMENTS(yoffset) EQ 0 THEN yoffset=0D
	
	; Compute RA/dec coordinates from x/y centroid:
	slcoord2radec, xyCent[0], xyCent[1], RA_cent, dec_cent, /ITERATE
	
	; Update planets[satellite_pos] structure with results:
	p = planets[selSatIndex]
	tags = ['METHOD', 'XCENT', 'YCENT', 'RA_CENT', 'DEC_CENT', $
			'XRES', 'YRES', 'SIGMA', 'XOFFSET', 'YOFFSET']
	values = LIST(method, xyCent[0], xyCent[1], RA_cent, dec_cent, $
				xyCent[0]-p.xcoord, xyCent[1]-p.ycoord, sigma, xoffset, yoffset)
	setStruct, p, tags, values
	planets[selSatIndex] = p
	
	; Print results:
	r2d = 180.0D/!DPI
	PRINT, FORMAT='(A,X,2A/, 2(2X,A,2(D12.8,X)/), 2(2X,A,2(D11.4,X)/),' $
				  +' 2X,A,2(D11.4,X),2X,G0/, 2X,A,2(D11.4)/, 2X,A,G0/ )', $
		p.NAME, p.method, ' RESULTS:', $
		"RA/dec Spice (degree) = ", r2d*p.RA     , r2d*p.dec, $
		"RA/dec Cent  (degree) = ", r2d*p.RA_cent, r2d*p.dec_cent, $
		"X/Y    Spice          = ", p.xcoord, p.ycoord, $
		"X/Y    Cent           = ", p.xcent , p.ycent, $
		"X/Y    Res    / Sigma = ", p.xres, p.yres, p.sigma, $
		"X/Y    Offset         = ", p.xoffset, p.yoffset, $
		"Pixel size       (km) = ", p.PIXSIZE
END
