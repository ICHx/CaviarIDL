@subimgxtract.pro

FUNCTION caviar_zoom_getInfo, xcursor, ycursor
	
	COMMON CAVIAR_DATA, image, catStars, imgStars, planets
	COMMON CAVIAR_PARAMS, dispParams, imgDraw, dispProName, starsCatalogs, find_imgStars_params

	info = STRARR(6)
	IF NOT haveTag(image, 'CMAT') THEN RETURN, ''
	
	; Convert cursor position from window to image:
	x = (xcursor+imgDraw.OFFSET[0])/imgDraw.ZFACTOR 
	y = (image.nl-1)-(ycursor+imgDraw.OFFSET[1])/imgDraw.ZFACTOR

	; Compute ra/dec coordinates of the cursor position in full image and overplot information
	slcoord2radec, x, y, RA, dec

	info[0] = STRING(x, y, FORMAT='("Cursor position (sample x line): ", X, I5, " x ", I5-)' )
	info[1] = "DN        = "+STRCOMPRESS(STRING(FIX(image.raw[x,y])), /REMOVE_ALL)
	info[2] = "RA  (deg) = "+STRCOMPRESS(STRING(RA*!RADEG), /REMOVE_ALL)
	info[3] = "Dec (deg) = "+STRCOMPRESS(STRING(dec*!RADEG),/REMOVE_ALL)
	
	IF N_ELEMENTS(planets) GT 0 THEN BEGIN
                radec2radlon, RA, dec, planets[0], image.et, image.SPC.ID, rad, lon
;		info[4] = (rad EQ -1) ? "Radius (km)          = NA" : STRING(rad, FORMAT='("Radius (km)          = ", I-12)')
;		info[5] = (lon EQ -1) ? "True Longitude (deg) = NA" : STRING(lon, FORMAT='("True Longitude (deg) = ", I-3)')
               info[4] = (rad EQ -1) ? "Radius (km)          = NA" : STRING(rad, FORMAT='("Radius (km)          = ", d12.2)') 
               info[5] = (lon EQ -1) ? "True Longitude (deg) = NA" : STRING(lon, FORMAT='("True Longitude (deg) = ", d12.6)')
	ENDIF

	RETURN, info
END


PRO caviar_zoom_display, xcursor, ycursor, zWinID, zWinXsize, zWinYsize, zFactor, $
						 INTERP=interp, $
						 FRAME=dispframe
	
	COMMON CAVIAR_SETTINGS, caviarSettings
;	COMMON CAVIAR_DATA, image
;njc added the stars, planets and rings for labelling in zoom window:
        COMMON CAVIAR_DATA, image, catstars, imgStars, planets, rings
	COMMON CAVIAR_PARAMS, dispParams, imgDraw, dispProName, starsCatalogs, find_imgStars_params
        COMMON CAVIAR_SATPOS, selSatIndex, wSatTblID, wSaveLbl, wSLFbaseYsize 
        COMMON CAVIAR_SATLIMBFIT, satModel, satLimb

;        IF selSatIndex EQ !NULL THEN RETURN
;        ; If satellites just have been loaded, the 'selSatIndex' must be updated:
;        IF selSatIndex GE N_ELEMENTS(planets) THEN selSatIndex = 0
		
	oldWin = !D.WINDOW
	TVLCT, saved_colors, /GET
	
        if imgDraw.ZFACTOR lt 1.0 then imgDraw.OFFSET=[0.0,0.0]

	; Convert cursor position from window to image:
	xc = LONG((xcursor+imgDraw.OFFSET[0])/imgDraw.ZFACTOR)
	yc = LONG((image.nl-1)-(ycursor+imgDraw.OFFSET[1])/imgDraw.ZFACTOR)

	; Size of the sub-image in full image
	subIMG_xsize = LONG(zWinXsize/(zFactor*imgDraw.ZFACTOR))
	subIMG_ysize = LONG(zWinYsize/(zFactor*imgDraw.ZFACTOR))
	
	; Extract the sub-image from the full-image and resize it to fit in zoom window
	subIMG = subimgxtract(image.raw, [xc, yc], subIMG_xsize, subIMG_ysize, GET_FRAME=frame)
	;zoomIMG = REBIN(subIMG, zWinXsize, zWinYsize, SAMPLE=1-interp)
	zoomIMG = CONGRID(subIMG, zWinXsize, zWinYsize, INTERP=interp)
	
	; Optionally, display a red square/rectangle in full-image window
	WSET, image.window
	IF KEYWORD_SET(dispframe) THEN BEGIN
		caviar_display
		PLOTS, frame.xmin-1, (image.nl)-frame.ymin-1, COLOR='0000FF'x, /DEVICE
		PLOTS, [frame.xmin-1, frame.xmax+1, frame.xmax+1, frame.xmin-1], $
			   (image.nl)-[frame.ymax+1, frame.ymax+1, frame.ymin-1, frame.ymin-1], $
			   /CONTINUE, /DEVICE, COLOR='0000FF'x
	ENDIF
	
	; 
	lczimg = zoomIMG[zWinXsize/2-1-20:zWinXsize/2-1+20,zWinYsize/2-1-20:zWinYsize/2-1+20]
	imax = MAX(lczimg)
	imin = MIN(lczimg)
	
	; Display zoom image in zoom window
	WSET, zWinID
	LOADCT, 0, /SILENT
;njc This is rescaling the pixel amplitudes in the zoom panel according to the position of the cursor:
	TV, zoomIMG * (255.0/(3*MEAN(zoomIMG))) < 255.0, /ORDER
	;TV, 0 > (zoomIMG-imin)/(imax-imin)*255 < 255, /ORDER
	
	; Plot a red cross in zoom window at the center
	PLOTS, zWinXsize/2, zWinYsize/2, /DEVICE, PSYM=1, SYMSIZE=2, COLOR=color('RED', /SILENT)
	
	; Display information about the image on zoom window:
	infos = caviar_zoom_getInfo(xcursor, ycursor)
	FOR i=0, N_ELEMENTS(infos)-1 $
	DO xyouts, 10, zWinYsize-20-i*15, infos[i], /DEVICE, COLOR=color('RED', /SILENT)

;njc ----------------------------------------------------------------------------------------------
;njc Added - plot the predicted positions of stars, satellites, rings and limbs on the zoom window:
;njc ----------------------------------------------------------------------------------------------
        nl = image.nl
        zoomFactor   = imgDraw.ZFACTOR
        imgWinOffset = imgDraw.OFFSET
        sat = planets[selSatIndex]

;Satellites:
        IF N_ELEMENTS(planets) GT 0 THEN BEGIN
          zplanets_x=dblarr(N_ELEMENTS(planets))
          zplanets_y=dblarr(N_ELEMENTS(planets))
          pnames=strarr(N_ELEMENTS(planets))
          FOR i=0, N_ELEMENTS(planets)-1 DO zplanets_x[i]=(planets[i].xcoord - xc)*zFactor*imgDraw.ZFACTOR + zWinXsize/2
          FOR i=0, N_ELEMENTS(planets)-1 DO zplanets_y[i]=(-planets[i].ycoord + yc)*zFactor*imgDraw.ZFACTOR + zWinYsize/2
          FOR i=0, N_ELEMENTS(planets)-1 DO pnames[i]=planets[i].name
          PLOTS, zplanets_x, zplanets_y, /DEVICE, PSYM=6, SYMSIZE=1.5, COLOR=color('DARK YELLOW 2', /SILENT)
          XYOUTS, zplanets_x+10, zplanets_y-3, pnames, /DEVICE, COLOR=color('DARK YELLOW 2', /SILENT)
        ENDIF

;Stars:
        IF N_ELEMENTS(catStars) GT 0 THEN BEGIN
          zstars_x=dblarr(N_ELEMENTS(catStars))
          zstars_y=dblarr(N_ELEMENTS(catStars))
          snames=strarr(N_ELEMENTS(catStars))
          FOR i=0, N_ELEMENTS(catStars)-1 DO zstars_x[i]=(catStars[i].xcoord - xc)*zFactor*imgDraw.ZFACTOR + zWinXsize/2
          FOR i=0, N_ELEMENTS(catStars)-1 DO zstars_y[i]=(-catStars[i].ycoord + yc)*zFactor*imgDraw.ZFACTOR + zWinYsize/2
          FOR i=0, N_ELEMENTS(catStars)-1 DO snames[i]=catStars[i].name
          PLOTS, zstars_x, zstars_y, /DEVICE, PSYM=4, SYMSIZE=3, COLOR=color('LIGHT BLUE', /SILENT)
          XYOUTS, zstars_x+10, zstars_y-3, snames, /DEVICE, COLOR=color('LIGHT BLUE', /SILENT)
        ENDIF

;Rings (note n_elements(rings) gives the number of rings not the no. of points per ring:
        IF N_ELEMENTS(rings) GT 0 THEN BEGIN
          ringi = rings[0]
          zrings_x=dblarr(N_ELEMENTS(ringi.xcoord))
          zrings_y=dblarr(N_ELEMENTS(ringi.ycoord))
          FOR i=0L, N_ELEMENTS(ringi.xcoord)-1L DO zrings_x[i]=(ringi.xcoord[i] - xc)*zFactor*imgDraw.ZFACTOR + zWinXsize/2
          FOR i=0L, N_ELEMENTS(ringi.ycoord)-1L DO zrings_y[i]=(-ringi.ycoord[i] + yc)*zFactor*imgDraw.ZFACTOR + zWinYsize/2
          PLOTS, zrings_x, zrings_y, /DEVICE, PSYM=1, SYMSIZE=2.0, COLOR=color('MAGENTA', /SILENT)
        ENDIF


;Limbs (from caviar_satpos_pkg.pro):
        ;***************************************************************************
        ; Display limb/terminator/equator:
        x=!NULL & y=!NULL & colors=!NULL
        colorsIndex = color(['CYAN','SPRING GREEN','ORANGE','MAGENTA'], /SILENT)

        IF N_ELEMENTS(satLimb) NE 0 THEN BEGIN
                nedge = N_ELEMENTS(satLimb[*,0])
;                xedge = zoomFactor * satLimb[*,0] - imgWinOffset[0]
;                yedge = zoomFactor * ((nl-1) - satLimb[*,1]) - imgWinOffset[1]
                xedge=[x, (satLimb[*,0]-xc)*zFactor*imgDraw.ZFACTOR + zWinXsize/2]
                yedge=[y, (-satLimb[*,1]+yc)*zFactor*imgDraw.ZFACTOR + zWinYsize/2]
                x=[x,xedge]
                y=[y,yedge]
                colors = [colors, REPLICATE(colorsIndex[3],nedge)]
        ENDIF
        IF haveTag(satModel, 'SLLIMB') THEN BEGIN
                nlimb = N_ELEMENTS(satModel.slLimb[*,0])
;                x=[x, zoomFactor*satModel.slLimb[*,0]            - imgWinOffset[0]]
;                y=[y, zoomFactor*((nl-1) - satModel.slLimb[*,1]) - imgWinOffset[1]]
                x=[x, (satModel.slLimb[*,0]-xc)*zFactor*imgDraw.ZFACTOR + zWinXsize/2]
                y=[y, (-satModel.slLimb[*,1]+yc)*zFactor*imgDraw.ZFACTOR + zWinYsize/2]
                colors = [colors, REPLICATE(colorsIndex[0],nlimb)]
        ENDIF
        IF haveTag(satModel, 'SLTERM') THEN BEGIN
                nterm = N_ELEMENTS(satModel.slTerm[*,0])
;                xt = zoomFactor * satModel.slTerm[*,0]            - imgWinOffset[0]
;                yt = zoomFactor * ((nl-1) - satModel.slTerm[*,1]) - imgWinOffset[1]
                xt=(satModel.slTerm[*,0]-xc)*zFactor*imgDraw.ZFACTOR + zWinXsize/2
                yt=(-satModel.slTerm[*,1]+yc)*zFactor*imgDraw.ZFACTOR + zWinYsize/2
                x=[x,xt]
                y=[y,yt]
                colors = [colors, REPLICATE(colorsIndex[1],nterm)]
        ENDIF           
        IF haveTag(satModel, 'SLEQUA') THEN BEGIN
                nequa = N_ELEMENTS(satModel.slEqua[*,0])
;                xe = zoomFactor * satModel.slEqua[*,0]            - imgWinOffset[0]
;                ye = zoomFactor * ((nl-1) - satModel.slEqua[*,1]) - imgWinOffset[1]
                xe=(satModel.slEqua[*,0]-xc)*zFactor*imgDraw.ZFACTOR + zWinXsize/2
                ye=(-satModel.slEqua[*,1]+yc)*zFactor*imgDraw.ZFACTOR + zWinYsize/2
                x=[x,xe]
                y=[y,ye]
                colors = [colors, REPLICATE(colorsIndex[2],nequa)]
        ENDIF
        
        IF x NE !NULL && y NE !NULL $
;njc
        THEN PLOTS, x, y, /DEVICE, PSYM=1, SYMSIZE=0.7, COLOR=colors

        ;***************************************************************************
        ; Plot a yellow square arround the predicted satellite center from the ephemeris:
;njc see above.
;        IF haveTag(sat, "XCOORD") && haveTag(sat, "YCOORD") $
;        THEN BEGIN
;                x = zoomFactor * sat.xcoord - imgWinOffset[0]
;                y = zoomFactor * ((nl-1) - sat.ycoord) - imgWinOffset[1]
;                x=(sat.xcoord-xc)*zFactor*imgDraw.ZFACTOR + zWinXsize/2
;                y=(-sat.ycoord+yc)*zFactor*imgDraw.ZFACTOR + zWinYsize/2
;                PLOTS, x, y, PSYM=6, SYMSIZE=0.75, COLOR=color('ORANGE', /SILENT), /DEVICE
;        ENDIF
        
        ; Plot a red cross at the computed satellite position:
        IF haveTag(sat, "XCENT") && haveTag(sat, "YCENT") $
        THEN BEGIN
;                x = zoomFactor * sat.xcent - imgWinOffset[0]
;                y = zoomFactor * ((nl-1) - sat.ycent) - imgWinOffset[1]
                x=(sat.xcent-xc)*zFactor*imgDraw.ZFACTOR + zWinXsize/2
                y=(-sat.ycent+yc)*zFactor*imgDraw.ZFACTOR + zWinYsize/2
                PLOTS, x, y, PSYM=1, SYMSIZE=3.0, COLOR=color('RED', /SILENT), /DEVICE
        ENDIF

;njc --------------------------------------------------------------------------------

	TVLCT, saved_colors
	WSET, oldWin
END



PRO caviar_zoom, zWinID, zWinXsize, zWinYsize, ZOOM_FACTOR=zFactor, $
				INTERPOLATE=interp, CONTINUOUS=continuous, FRAME=frame
		
	COMMON CAVIAR_DATA, image
		
	IF NOT KEYWORD_SET(zFactor)    THEN zFactor    = 4
	IF NOT KEYWORD_SET(interp)     THEN interp     = 0
	IF NOT KEYWORD_SET(continuous) THEN continuous = 1
	IF NOT KEYWORD_SET(frame)      THEN frame      = 0
	
	
	; Save current window
	oldWin = !D.WINDOW
	
		
	;########## Waiting for the cursor to be inside the full image window
	WSET, image.window
	REPEAT CURSOR, xcursor, ycursor, 1-continuous, /DATA $
	UNTIL (xcursor GT 0 && xcursor LT 1) && $
	 	  (ycursor GT 0 && ycursor LT 1)
	
	
	xcursor_old = -1
	ycursor_old = -1
	first_click = 1
	;########## Until the cursor moves out of the full image window...
	REPEAT BEGIN
		; ... and while the cursor is not static, ...
		WHILE (xcursor GT 0 && xcursor LT 1) && $
	 	  	  (ycursor GT 0 && ycursor LT 1) && $
	 	  	  (xcursor NE xcursor_old || ycursor NE ycursor_old) DO BEGIN

			; Extract sub-image, zoom it and display it
			caviar_zoom_display, xcursor*!D.X_VSIZE, ycursor*!D.Y_VSIZE, $
								 zWinID, zWinXsize, zWinYsize, $
								 zFactor, INTERP=interp, FRAME=frame
							
			; Save old cursor position
			xcursor_old = xcursor
			ycursor_old = ycursor
			
			; Get new cursor position within the full image window
			CURSOR, xcursor, ycursor, 1-continuous, /DATA
			
			; Click right to stop refresh zoom window
			IF !MOUSE.button EQ 4 THEN RETURN
		ENDWHILE
		
		; Save old cursor position
		xcursor_old = xcursor
		ycursor_old = ycursor
	
		; Get new cursor position within the full image window
		CURSOR, xcursor, ycursor, 1-continuous, /DATA
		
		IF !MOUSE.button EQ 1 && first_click EQ 1 THEN BEGIN
			infos = caviar_zoom_getInfo(xcursor*!D.X_VSIZE, ycursor*!D.Y_VSIZE)
			FOR i=0, N_ELEMENTS(infos)-1 DO PRINT, infos[i]
			PRINT, ""
			first_click = 0
		ENDIF 
		IF xcursor_old NE xcursor || ycursor_old NE ycursor $
		THEN first_click = 1		;Needed to avoid multi-output for one click
			
		
		; Click right to stop refresh zoom window
		IF !MOUSE.button EQ 4 THEN RETURN
		
	ENDREP UNTIL ~(xcursor GT 0 && xcursor LT 1) || $
	 	  		 ~(ycursor GT 0 && ycursor LT 1)
	
	WSET, oldWin
END
