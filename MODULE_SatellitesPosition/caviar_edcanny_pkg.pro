;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
; NAME: CAVIAR_EDCANNY
; PURPOSE: Detect edges in an image by using Canny algorithm.
; INPUTS:
;-------------------------------------------------------------------------------
PRO caviar_edcanny, cedp, filters, imfdii, pwIMFDbase
	
	COMMON CAVIAR_DATA, image, catstars, imgStars, planets
	COMMON CAVIAR_SATPOS, pSelIndex
	COMMON CAVIAR_SATLIMBFIT, satModel, satLimb
	
	IF N_ELEMENTS(image) EQ 0 THEN MESSAGE, "You need to load an image first!"
	IF pSelIndex EQ !NULL THEN MESSAGE, "You need to select an object first!"
	
	;***************************************************************************
	; Extract region of interest in input image, containing the object limb:
	;***************************************************************************
	sat = planets[pSelIndex]
	
	; Get size, in pixels, of the radius of the satellite on the sensor:
	cspice_spkez, sat.ID, image.ET, 'J2000', 'CN+S', image.SPC.ID, satState, ltime
	satPixRadius = ATAN(MAX(sat.RADII), NORM(satState[0:2])) / (image.FOVPIX*image.BINNING)
	
	hbox = satPixRadius + (5 > 0.5*satPixRadius < 100)
	smin = ( ROUND( sat.xcoord - hbox ) > 0 )[0]
	smax = ( ROUND( sat.xcoord + hbox ) < (image.ns-1) )[0]
	lmin = ( ROUND( sat.ycoord - hbox ) > 0 )[0]
	lmax = ( ROUND( sat.ycoord + hbox ) < (image.nl-1) )[0]
	img = image.raw[smin:smax, lmin:lmax]
	
	;***************************************************************************
	; Image filtering:
	;***************************************************************************
	display = imfdii ? pwIMFDbase : 0
	img = caviar_imfilt(img, filters, DISPLAY=display)
	
	;***************************************************************************
	; Edge detection:
	;***************************************************************************
	img_canny = CANNY(img, HIGH=cedp[0], LOW=cedp[1], SIGMA=cedp[2])
	index = WHERE(img_canny NE 0, count)
	IF count EQ 0 THEN MESSAGE, "Cannot detect edges with these parameters."
	edge = TRANSPOSE( ARRAY_INDICES(img_canny, index) )
	
	
	; Get coordinates of the limb in the original image:
	zf = 1
	FOREACH filter, filters DO IF filter.NAME EQ "CUBIC INTERPOLATION" THEN zf = filter.factor
	satLimb = [ [(1.0D/zf)*edge[*,0]+smin], [(1.0D/zf)*edge[*,1]+lmin] ]
	help, satlimb

	; Plot edge in the image filtering result window:
	IF imfdii THEN BEGIN
		;Save current window and color table and load new one:
		old_win = !D.WINDOW
		TVLCT, ct_saved, /GET
		LOADCT, 0, /SILENT
		
		WIDGET_CONTROL, *pwIMFDbase, GET_UVALUE=puval
		factor = (*puval).factor
		nns    = (*puval).nns
		nnl    = (*puval).nnl
		
		; Plot the edge points in the result window:
		col = color("MAGENTA", /SILENT)
		nfilt = N_ELEMENTS(filters)
		PLOTS, factor*edge[*,0], (nnl-1)-factor*edge[*,1], /DEVICE, PSYM=3, COLOR=col
		PLOTS, factor*edge[*,0]+nfilt*nns, (nnl-1)-factor*edge[*,1], /DEVICE, PSYM=3, COLOR=col
		
		;Reset previous window and color table
		WSET, old_win
		TVLCT, ct_saved
	ENDIF

	caviar_display
END

;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;	                  Manage events of the widget 		     				   
;-------------------------------------------------------------------------------
PRO caviar_edcanny_gui_event, event
	
	COMMON CAVIAR_DATA, image, stars, imgStars, planets
	
;	CATCH, error
;	IF error NE 0 THEN BEGIN
;		res = DIALOG_MESSAGE(!ERROR_STATE.MSG, DIALOG_PARENT=event.HANDLER, /CENTER)
;		RETURN
;	ENDIF
	
	; Get state from the first child of the compound widget root:
	child = WIDGET_INFO(event.HANDLER, /CHILD)
	WIDGET_CONTROL, child, GET_UVALUE=pstate
	
	WIDGET_CONTROL, /HOURGLASS

	update = (*pstate).autodetect
	
	uname = WIDGET_INFO(event.id, /UNAME)
	CASE uname OF
		
		;***********************************************************************
		; Image filtering parameters:
		;***********************************************************************
		"IMF DISP INTER-IMAGES": BEGIN
			oldVal = (*pstate).imfdii
			(*pstate).imfdii = event.SELECT
			IF event.SELECT EQ oldVal THEN update = 0
		END
		
		"IMF BANDPASS FILTER": BEGIN
			oldVal = (*pstate).imfbpp
			(*pstate).imfbpp = event.SELECT
			IF event.SELECT EQ oldVal THEN update = 0
		END
		
		"IMF PARAMS": BEGIN
			oldVal = (*pstate).imfp
			(*pstate).imfp = event.value
			IF ARRAY_EQUAL(event.value, oldVal) EQ 1 THEN update = 0
		END
		;***********************************************************************

		"CANNY PARAMS": BEGIN
			oldVal = (*pstate).cedp
			(*pstate).cedp = event.value
			IF ARRAY_EQUAL(event.value, oldVal) EQ 1 THEN update = 0
		END
		
		"AUTO DETECT": BEGIN
			(*pstate).autodetect = event.select
			update = 0
		END
		
		"EDGE DETECT": update = 1
		
		ELSE:
	ENDCASE
	
	IF NOT update THEN RETURN
	;***************************************************************************
	; Prepare filters:
	filters = LIST()
	IF (*pstate).imfp[0] GT 0 THEN filters.add, {NAME:"MORPH CLOSE", R:(*pstate).imfp[0]}
	IF (*pstate).imfbpp THEN filters.add, {NAME:"BANDPASS BUTTERWORTH"}
	IF (*pstate).imfp[1] GT 1 THEN filters.add, {NAME:"CUBIC INTERPOLATION", FACTOR:(*pstate).imfp[1]}
	IF (*pstate).imfp[2] GE 3 THEN filters.add, {NAME:"MEDIAN", WIDTH:(*pstate).imfp[2]}
	
	; Detect edge:
	caviar_edcanny, (*pstate).cedp, filters, (*pstate).imfdii, (*pstate).pwIMFDbase
	
END

;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
; NAME: 
;		CAVIAR_EDCANNY_GUI
;
; PURPOSE: Display a graphical interface to set parameters for a edge detection
;		algorithm that use canny method.
;
; CATEGORY:
;		Widgets
;
; INPUTS:
; 		None.
; KEYWORDS:
;		
; OUTPUTS:
;       None.
; COMMON BLOCKS:
;       
; SIDE EFFECTS:
;       A widget window is created.
; RESTRICTIONS:
;       
; MODIFICATION HISTORY:
;		2014, december			MEUNIER L-E		OBSPM>IMCCE
;			- Written
;-------------------------------------------------------------------------------
PRO caviar_edcanny_gui, PARENT=wMAINbase, GROUP_LEADER=groupLeader, $
							XOFFSET=xoffset, YOFFSET=yoffset, TITLE=title, $
							FRAME=frame, XPAD=xpad, YPAD=ypad, SPACE=space
	
	COMMON CAVIAR_GUI, wCAVIARtlb

	; Test if widget window is already launched
	IF(XRegistered('caviar_edcanny_gui') NE 0) THEN RETURN
	
	;***************************************************************************
	; Set Parameters 
	;***************************************************************************
	; Set initial state for displaying intermediate filtered images:
	imfdii = 0
	
	; Bandpass image filter parameters:
	imfbpp = 0	;LOWFREQ:0.0, HIGHFREQ:0.5, BUTTERWORTH:5.0}
	
	; Others image filters parameters:
	imfp = [0,1,1]
	imfp_format = ['(I2)','(I2)','(I2)']
	imfp_min = [0,1,1]
	imfp_max = [30,30,31]
	imfp_step = [1,1,2]
        imfp_text = ["Morph close r:  ","Interp factor:  ","Median width:   "]
	
	; Canny edge detection parameters:
	cedp = [0.6,0.95,0.4]
	cedp_format = ['(G0)','(G0)','(G0)']
	cedp_min = [0.1,0.01,0.01]
	cedp_max = [3,1,1]
	cedp_step = [0.1,0.01,0.01]
	cedp_text = ["Convol. sigma: ","High thresh.:  ","Low thresh.:   "]
	
	; Auto Detect setting:
	autodetect = 1
	
	
	;*************************************************************************** 
	; Define the widget base and subwidgets tree 
	;***************************************************************************
	IF NOT KEYWORD_SET(title) THEN title = "Canny Edge Detection"
	IF NOT KEYWORD_SET(space) THEN space = 10
	
	IF KEYWORD_SET(wMAINbase) THEN BEGIN
		IF NOT WIDGET_INFO(wMAINbase, /VALID_ID) $
		THEN res=DIALOG_MESSAGE('Invalid widget identifier.',/ERROR)
	ENDIF ELSE BEGIN
		IF NOT KEYWORD_SET(groupLeader) $
		THEN wMAINbase = WIDGET_BASE(TITLE=title, XOFFSET=xoffset, YOFFSET=yoffset) $
		ELSE BEGIN
			IF WIDGET_INFO(groupLeader, /VALID_ID) $
			THEN wMAINbase = WIDGET_BASE(TITLE=title, XOFFSET=xoffset, YOFFSET=yoffset, $
										GROUP_LEADER=groupLeader, /FLOATING) $
			ELSE res=DIALOG_MESSAGE('Invalid group leader widget identifier.',/ERROR)
		ENDELSE
	ENDELSE
	
	;***************************************************************************
	; Define subwidgets tree:
	;***************************************************************************
	extra = WIDGET_BASE(wMAINbase, /COLUMN, XPAD=xpad, YPAD=ypad)
	
	wbase = WIDGET_BASE(extra, /COLUMN, FRAME=frame, XPAD=0, YPAD=0, SPACE=space, $
									/BASE_ALIGN_LEFT, /ALIGN_CENTER)
		
		; Widgets for edge detection option and button:
		wsbase = WIDGET_BASE(wbase, /ROW, YPAD=0, SPACE=120)
			wADbgrp = CW_BGROUP(wsbase, /NONEXCLUSIVE, UNAME='AUTO DETECT', $
								"Auto-update", SET_VALUE=autodetect)
			wEDbttn = WIDGET_BUTTON(wsbase, UNAME="EDGE DETECT", $
									VALUE="Detect", XSIZE=60, YSIZE=29)
		
		; Widgets for image filtering:
		wIMFbase = WIDGET_BASE(wbase, /COL, XPAD=0, YPAD=0)
			wIMFlbl = WIDGET_LABEL(wIMFbase, /ALIGN_LEFT, VALUE="Image pre-filtering:")
			wIMFsbase = WIDGET_BASE(wIMFbase, /COL, /FRAME, XPAD=3, YPAD=3, SPACE=0)
				wIMFBPFILTbgrp = CW_BGROUP(wIMFsbase, /NONEXCLUSIVE, UNAME="IMF BANDPASS FILTER", $
					"Bandpass filter (Butterworth 0-0.5, 5)", SET_VALUE=imfbpp)
				wIMFsgroup = CW_SGROUP(wIMFsbase, 3, UNAME="IMF PARAMS", $
					TEXT=imfp_TEXT, FORMAT=imfp_FORMAT, VAL=imfp, $
					MIN=imfp_MIN, MAX=imfp_MAX, STEP=imfp_step, $
					X_SLIDER_SIZE=137, X_LABEL_SIZE=116, Y_LABEL_SIZE=16, $
					XPAD=0, YPAD=0, SPACE=0)
				wIMFDISPIIbgrp = CW_BGROUP(wIMFsbase, /NONEXCLUSIVE, UNAME="IMF DISP INTER-IMAGES", $
					"Display intermediate filtered images", SET_VALUE=imfdii)
			
		; Widgets for gradient parameters:
		wCPbase = WIDGET_BASE(wbase, /COL, XPAD=0, YPAD=0)
			wCPlbl = WIDGET_LABEL(wCPbase, /ALIGN_LEFT, VALUE="Edge detection parameters:")
			wCPsbase  = WIDGET_BASE(wCPbase, /COL, /FRAME, XPAD=3, YPAD=3, SPACE=0)
				wSGroup = CW_SGROUP(wCPsbase, 3, UNAME="CANNY PARAMS", $
				TEXT=cedp_text, FORMAT=cedp_format, VAL=cedp, $
				MIN=cedp_min, MAX=cedp_max, STEP=cedp_step, $
				X_SLIDER_SIZE=137, X_LABEL_SIZE=116, Y_LABEL_SIZE=16, $
				XPAD=0, YPAD=0, SPACE=0)

	;***************************************************************************
	; Realize main base, control subwidgets and initialize:
	;***************************************************************************
	WIDGET_CONTROL, wMAINbase, /REALIZE
	
	; Initialize image filtering draw widget:
	wIMFDbase = WIDGET_BASE(TITLE="Image filtering results", GROUP_LEADER=wCAVIARtlb)
		wIMFdraw = WIDGET_DRAW(wIMFDbase)
	WIDGET_CONTROL, wIMFDbase, /REALIZE, MAP=0
	pwIMFDbase = PTR_NEW(wIMFDbase, /NO_COPY)
	
	;***************************************************************************
	; Set 'state' structure with some parameters and copy it in 'extra' widget uvalue:
	;***************************************************************************
	state = {IMFDII:imfdii, IMFBPP:imfbpp, IMFP:imfp, CEDP:cedp, $
			 AUTODETECT:autodetect, PWIMFDBASE:pwIMFDbase}
	WIDGET_CONTROL, extra, SET_UVALUE=PTR_NEW(state, /NO_COPY)
	
	XMANAGER, 'caviar_edcanny_gui', wMAINbase, /JUST_REG
END
