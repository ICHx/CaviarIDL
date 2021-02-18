;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
; NAME: caviar_EDGRAD
; PURPOSE: Detect edges in an image by using a gradient method.
;       
; CALLING SEQUENCE:
;
;
; INPUTS:
;
; PROCEDURE:
;	An edge point is a point were the gradient is maximum locally. 
;	First we compute the gradient of the image in either sample and/or line 
;	and/or diagonals directions. Then we search for local maxima in the gradient 
;	image, again, in sample and/or line and/or diagonals directions, 
;	Local maxima are searched in the range 1:ns-2 and 1:nl-2 of the image to 
;	avoid border effects. A maximum is found if the gradient value is greater
;	than the defined threshold and also greater than the neighbor points in any
;	direction. The neighbor points are defined by a unidimensionnal sliding 
;	box of size (2*hbox+1), centered on the point to compute.
;	
;
; MODIFICATION HISTORY:
;	August, 13 2014			LE. Meunier, IMCCE
;		- Written
;-------------------------------------------------------------------------------
PRO caviar_edgrad, gcd, msd, gedp, filters, imfdii, pwIMFDbase
	
	COMMON CAVIAR_DATA, image, stars, imgStars, planets
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
	; Edges detection:
	;***************************************************************************
	sgrad = gcd[0]
	lgrad = gcd[1]
	dgrad = gcd[2]
	IF sgrad EQ 0 && lgrad EQ 0 && dgrad EQ 0 THEN dgrad = 1
			
	activityThreshold = gedp[0]
	hbox=(gedp[1]-1)>1
	w = hbox-1
	
	imgSize = SIZE(img)
	ns = imgSize[1]
	nl = imgSize[2]
	
	;***************************************************************************
	; Compute gradient of the image in sample, line and/or diagonals directions:
	
	; 1- Extend the image dimension by adding 1 pixel around all edges:
	; With the keyword 'MIRROR', the added pixel is a copy of the image edge.
	imgex = DOUBLE(img_extend(img,1,/MIRROR))
	
	; 2- Put bidimensionnal coordinates of the image into two unidimensionnal 
	; arrays for quick computation:
	imgcoords = LINDGEN(ns*nl)
	l = imgcoords[*] /ns 	;line
	s = imgcoords[*] -ns*l	;sample
	
	gradient = DBLARR(ns*nl)
	; 3- Compute gradient of the image in each directions:
	; (The +1 is because img data start at (1,1) of the extended image)
	IF sgrad THEN gradient += ABS(imgex[s,l+1]-imgex[s+2,l+1])
	IF lgrad THEN gradient += ABS(imgex[s+1,l]-imgex[s+1,l+2])
	IF dgrad THEN gradient += ABS(imgex[s,l]-imgex[s+2,l+2]) + ABS(imgex[s+2,l]-imgex[s,l+2])
	imgrad = DBLARR(ns,nl)
	imgrad[s,l] = gradient[s+(ns*l)]		; Parenthesis are important!!!
	
	; Set the gradient threshold:
	gradThreshold = activityThreshold * MAX(imgrad) / 100.0
	
	;***************************************************************************
	; Search for local maxima of the image gradient:

	; Extend the imgrad dimensions by adding (w=hbox-1) pixels around all edges:
	imgradex = DOUBLE(img_extend(imgrad,w))
	
	limb = {xcoords:LIST(), ycoords:LIST()}
	goodindex = WHERE(imgradex GT gradthreshold)
	IF goodindex[0] EQ -1 THEN MESSAGE, "Cannot detect edges with these parameters."
	goodindex = ARRAY_INDICES(imgradex,goodindex)
	FOR k=0, N_ELEMENTS(goodindex[0,*])-1 DO BEGIN
		i = goodindex[0,k]
		j = goodindex[1,k]
		IF i LT hbox OR i GT ns+2*(hbox-1)-hbox-1 THEN CONTINUE
		IF j LT hbox OR j GT nl+2*(hbox-1)-hbox-1 THEN CONTINUE
		sfoundone = (lfoundone = 0)
		IF msd[0] THEN BEGIN	;Search maxima along sample direction
			smax = MAX(imgradex[i-hbox:i+hbox,j], smaxpos)
			smaxpos = ARRAY_INDICES(imgradex[i-hbox:i+hbox,j],smaxpos)
			sfoundone = smaxpos[0] EQ hbox ? 1 : 0
		ENDIF
		IF msd[1] THEN BEGIN	;Search maxima along line direction
			lmax = MAX(imgradex[i,j-hbox:j+hbox], lmaxpos)
			lmaxpos = ARRAY_INDICES(imgradex[i,j-hbox:j+hbox],lmaxpos)
			lfoundone = lmaxpos[1] EQ hbox ? 1 : 0
		ENDIF
		IF sfoundone || lfoundone THEN BEGIN
			limb.xcoords.add, i-w
			limb.ycoords.add, j-w
		ENDIF
	ENDFOR
	edge = [[limb.xcoords.ToArray()],[limb.ycoords.ToArray()]]
	
	IF (SIZE(edge))[0] EQ 0 THEN MESSAGE, "Cannot detect edges with these parameters."
	
	; Get coordinates of the limb in the original image:
	zf = 1
	FOREACH filter, filters DO IF filter.NAME EQ "CUBIC INTERPOLATION" THEN zf = filter.factor
	satLimb = [ [(1.0D/zf)*edge[*,0]+smin], [(1.0D/zf)*edge[*,1]+lmin] ]
	
	
	
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
; Manage events of the widget
;-------------------------------------------------------------------------------
PRO caviar_edgrad_gui_event, event
	
	COMMON CAVIAR_DATA, image, stars, imgStars, planets
	
	CATCH, error
	IF error NE 0 THEN BEGIN
		res = DIALOG_MESSAGE(!ERROR_STATE.MSG, DIALOG_PARENT=event.HANDLER, /CENTER)
		RETURN
	ENDIF
	
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
		; Gradient Computation Direction
		"GCD": (*pstate).gcd[event.VALUE] = event.SELECT
		
		; Maxima Search Direction
		"MSD": (*pstate).msd[event.VALUE] = event.SELECT
		
		"GRADIENT PARAMS": BEGIN
			oldVal = (*pstate).gedp
			(*pstate).gedp = event.value
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
	caviar_edgrad, (*pstate).gcd, (*pstate).msd, (*pstate).gedp, filters, $
					(*pstate).imfdii, (*pstate).pwIMFDbase
					
	
END

;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
; NAME: 
;		CAVIAR_EDGRAD_GUI
;
; PURPOSE: Display a graphical interface to set parameters for a edge detection
;		algorithm that use gradient method.
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
PRO caviar_edgrad_gui, PARENT=wMAINbase, GROUP_LEADER=groupLeader, $
							XOFFSET=xoffset, YOFFSET=yoffset, TITLE=title, $
							FRAME=frame, XPAD=xpad, YPAD=ypad, SPACE=space
	
	COMMON CAVIAR_GUI, wCAVIARtlb

	; Test if widget window is already launched
	IF(XRegistered('caviar_edgrad_gui') NE 0) THEN RETURN
	
	;***************************************************************************
	; Set Parameters 
	;***************************************************************************
	; Set initial state for displaying intermediate filtered images:
	imfdii = 0
	
	; Bandpass image filter parameters:
	imfbpp = 0 ;LOWFREQ:0.0, HIGHFREQ:0.5, BUTTERWORTH:5.0
	
	; Others image filters parameters:
	imfp = [0,1,1]
	imfp_format = ['(I2)','(I2)','(I2)']
	imfp_min = [0,1,1]
	imfp_max = [30,30,31]
	imfp_step = [1,1,2]
        imfp_text = ["Morph close r:  ","Interp factor:  ","Median width:   "]

	filters = LIST()

	IF imfp[0] GT 0 THEN filters.add, {NAME:"MORPH CLOSE", R:imfp[0]}
	IF imfbpp THEN filters.add, {NAME:"BANDPASS BUTTERWORTH"}
	IF imfp[1] GT 1 THEN filters.add, {NAME:"CUBIC INTERPOLATION", FACTOR:imfp[1]}
	IF imfp[2] GE 3 THEN filters.add, {NAME:"MEDIAN", WIDTH:imfp[2]}
	
	; Gradient Computation Direction setting:
	gcd = [1,1,1]
	
	; Maxima Search Direction setting:
	msd = [1,1]
	
	; Gradient edge detection parameters:
	gedp = [30,3]
	gedp_step = [1,1]
	gedp_min = [1,0]
	gedp_max = [100,40]
	gedp_text = ["  Activity:  ","  Separation: "]
	gedp_format = ['(I3)','(I2," px")']

	; Auto Detect setting:
	autodetect = 1
	

	
	;*************************************************************************** 
	; Define the widget base and subwidgets tree 
	;***************************************************************************
	IF NOT KEYWORD_SET(title) THEN title = "Gradient Edge Detection"
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
					"Bandpass filter (Butterworth 0-0.5, 5)", SET_VALUE=imfbpp, UVALUE=imfbpp)
				wIMFsgroup = CW_SGROUP(wIMFsbase, 3, UNAME="IMF PARAMS", UVALUE=imfp, $
					TEXT=imfp_text, FORMAT=imfp_format, VAL=imfp, $
					MIN=imfp_min, MAX=imfp_max, STEP=imfp_step, $
					X_SLIDER_SIZE=137, X_LABEL_SIZE=116, Y_LABEL_SIZE=16, $
					XPAD=0, YPAD=0, SPACE=0)
				wIMFDISPIIbgrp = CW_BGROUP(wIMFsbase, /NONEXCLUSIVE, UNAME="IMF DISP INTER-IMAGES", $
					"Display intermediate filtered images", SET_VALUE=imfdii, UVALUE=imfdii)
			
		; Widgets for gradient parameters:
		wGPbase = WIDGET_BASE(wbase, /COL, XPAD=0, YPAD=0)
			wGPlbl = WIDGET_LABEL(wGPbase, /ALIGN_LEFT, VALUE="Edge detection parameters:")
			wGPsbase  = WIDGET_BASE(wGPbase, /COL, /FRAME, XPAD=3, YPAD=3, SPACE=0)
				wGCDbgroup = CW_BGROUP(wGPsbase, ['Sample','Line','Diagonals'], $
					SET_VALUE=gcd, UNAME='GCD', XPAD=0, YPAD=0, SPACE=0, $
					LABEL_TOP="Gradient computation direction:", /ROW, /NONEXCLUSIVE)
				wMSDbgroup = CW_BGROUP(wGPsbase, ['Sample','Line'], $
					SET_VALUE=msd, UNAME='MSD', XPAD=0, YPAD=0, SPACE=0, $
					LABEL_TOP="Maxima search direction:", /ROW, /NONEXCLUSIVE)
				wlabel = WIDGET_LABEL(wGPsbase, VALUE="Thresholds:", /ALIGN_LEFT, XOFFSET=10)
				wSGroup = CW_SGROUP(wGPsbase, 2, UNAME="GRADIENT PARAMS", $
					TEXT=gedp_text, FORMAT=gedp_format, VAL=gedp, $
					MIN=gedp_min, MAX=gedp_max, STEP=gedp_step, $
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
	state = {FILTERS:filters, IMFDII:imfdii, IMFBPP:imfbpp, IMFP:imfp, GCD:gcd, MSD:msd, GEDP:gedp, $
			 AUTODETECT:autodetect, PWIMFDBASE:pwIMFDbase}
	WIDGET_CONTROL, extra, SET_UVALUE=PTR_NEW(state, /NO_COPY)
	
	XMANAGER, 'caviar_edgrad_gui', wMAINbase, /JUST_REG
END
