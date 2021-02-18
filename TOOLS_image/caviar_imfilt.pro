FUNCTION caviar_imfilt, img_in, filters, DISPLAY=pwIMFDbase
	
	COMMON CAVIAR_GUI, wCAVIARtlb
	
	img = img_in
	imgSize = SIZE(img)
	ns = imgSize[1]
	nl = imgSize[2]
	
	; Get scale factor to display the filtered subimage with the best size:
	zf = 1
;njc Beware: this gives a zoom factor that can be way too big:
	FOREACH filter, filters DO BEGIN
		IF filter.NAME EQ "CUBIC INTERPOLATION" THEN zf = filter.factor
	ENDFOREACH
	factor = 0
	REPEAT dummy = ++factor*zf*ns/150 UNTIL dummy GE 1
	nns = factor*zf*ns
	nnl = factor*zf*nl
		
	;***************************************************************************
	; Prepare to draw widget to display intermediate filtered images:
	;***************************************************************************
	IF KEYWORD_SET(pwIMFDbase) THEN BEGIN
		
		; Save current window and color table and load new one:
		old_win = !D.WINDOW
		TVLCT, ct_saved, /GET
		LOADCT, 0, /SILENT
	
		; Get size of the top-level-base and draw widget to show filtering results:
		drawXsize = nns*(N_ELEMENTS(filters)+1)
		drawYsize = nnl+20
		
		; Get Monitor information and set widget screen dimensions:
		oInfo = OBJ_NEW('IDLsysMonitorInfo')
		rects = oInfo->GetRectangles(/EXCLUDE_TASKBAR)
		OBJ_DESTROY, oInfo
		maxHeight = MAX(rects[3,*], monIndex)
		scrXsize = (drawXsize+31) < (rects[2,monIndex]-50)
		scrYsize = (drawYsize+31) < (rects[3,monIndex]-50)
		
		; Get CaVIaR top-level-base id and widget base id:
		WIDGET_CONTROL, wCAVIARtlb, TLB_GET_OFFSET=tlbOffset
		wIMFDbase = *pwIMFDbase

		IF NOT WIDGET_INFO(wIMFDbase, /VALID_ID) THEN BEGIN
			; Define the limb fitting draw widget base and subwidgets tree:
			wIMFDbase = WIDGET_BASE(TITLE="Image filtering results", GROUP_LEADER=wCAVIARtlb)
			wIMFdraw = WIDGET_DRAW(wIMFDbase, /SCROLL, $
						X_SCROLL_SIZE=drawXsize, Y_SCROLL_SIZE=drawYsize, $
						SCR_XSIZE=scrXsize, SCR_YSIZE=scrYsize)
			WIDGET_CONTROL, wIMFDbase, /REALIZE
			*pwIMFDbase = wIMFDbase
		ENDIF
	
		WIDGET_CONTROL, wIMFDbase, /MAP
		
		; Set draw widget size and get window id:
		WIDGET_CONTROL, WIDGET_INFO(wIMFDbase, /CHILD), GET_VALUE=lfWinID, $
						SCR_XSIZE=scrXsize, SCR_YSIZE=scrYsize, $
						DRAW_XSIZE=drawXsize, DRAW_YSIZE=drawYsize
		
		; Set top-level-base size and position and make it visible, if not already:
		uvalue = {FACTOR:factor, NNS:nns, NNL:nnl}
		WIDGET_CONTROL, wIMFDbase, SET_UVALUE=PTR_NEW(uvalue, /NO_COPY), $
						SCR_XSIZE=scrXsize, SCR_YSIZE=scrYsize, MAP=1, $
						TLB_SET_XOFFSET=tlbOffset[0], TLB_SET_YOFFSET=tlbOffset[1]
	
		; Set active window & reset drawing area from previous data:
		WSET, lfWinID
		TV, INTARR(drawXsize, drawYsize)
	
		zscale = 255.0/(3*MEAN(img))
		TV, REBIN(img, nns, nnl, /SAMPLE) * zscale < 255.0, /ORDER
		XYOUTS, nns/2, nnl+5, 'ORIGINAL', ALIGNMENT=0.5, /DEVICE
	ENDIF
	
	;***************************************************************************
	; Image filtering
	;***************************************************************************
	err_msg=""
	CATCH, error_status
	IF error_status NE 0 THEN BEGIN
		CATCH, /CANCEL
		MESSAGE, err_msg+!ERROR_STATE.MSG, /CONTINUE
	ENDIF
					
	k=0			
	FOREACH filter, filters DO BEGIN
		k++
		CASE STRUPCASE(filter.NAME) OF
			
			; Morphological filter:
			"MORPH CLOSE": BEGIN
				IF haveTag(filter, "R") && filter.R GT 0 THEN BEGIN
					r = filter.R
					elmt = SHIFT(DIST(2*r+1), r, r) LE r
					img_ext = img_extend(img, r, /MIRROR)
					img_ext = MORPH_CLOSE(img_ext, elmt, /GRAY)
					img = img_ext[r:r+ns-1, r:r+nl-1]
				ENDIF ELSE MESSAGE, /INF, $
					"WARNING! Cannot find Morph Close element radius. 'Radius' must be greater than 0."
		 	END
	  	
		  	; Band pass filter:
		  	"BANDPASS BUTTERWORTH": BEGIN
		  		r=2
		  		img_ext = img_extend(img, r, /MIRROR)
		  		img_ext = BANDPASS_FILTER(img_ext, 0, 0.5, BUTTERWORTH=5)
				img = img_ext[r:r+ns-1, r:r+nl-1]
			END
			
			; Interpolation:
			"CUBIC INTERPOLATION": BEGIN
				err_msg = "Error in cubic interpolation. "
				
				IF haveTag(filter, "FACTOR") && filter.FACTOR GT 0 THEN BEGIN
					f = filter.FACTOR
					img = CONGRID(img, f*ns, f*nl, CUBIC=-0.5)
				ENDIF ELSE MESSAGE, /INF, $
					"WARNING! Cannot find interpolation factor. 'Factor' must be greater than 0."
			END
			
			; Median filter:
			"MEDIAN": BEGIN
				err_msg = "Error in median filtering. "
			   	IF haveTag(filter, "WIDTH") && filter.WIDTH GE 3 THEN BEGIN
			   		w = filter.WIDTH
			   		img = ESTIMATOR_FILTER(img, w, /MEDIAN)
				ENDIF ELSE MESSAGE, /INF, $
					"WARNING! Cannot find median width. 'Width' must be greater or equal to 3."
			END
			
			ELSE:
		ENDCASE
		
		IF KEYWORD_SET(pwIMFDbase) THEN BEGIN
			zscale = 255.0/(3*MEAN(img))
			TV, REBIN(img, nns, nnl, /SAMPLE) * zscale < 255.0, k*nnl, 0, /ORDER
		  	XYOUTS, (k+0.5)*nns, nnl+5, $
				STRING(k, '(I0)')+'. '+STRUPCASE(filter.NAME), ALIGNMENT=0.5, /DEVICE
		ENDIF
		
	ENDFOREACH
	
	RETURN, img
END
