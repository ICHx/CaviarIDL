;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;	Procedures/functions :
;		- PRO			caviar_rings_display
;		- FUNCTION 		caviar_rings_loadSaturnFring
;		- FUNCTION		caviar_rings_load
;		- FUNCTION		caviar_rings_gui_getparams
;		- FUNCTION		caviar_rings_gui_deftblvalues
;		- PRO			caviar_rings_gui_event
;		- PRO			caviar_rings_gui
;-------------------------------------------------------------------------------

;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
; NAME: CAVIAR_RINGS_DISPLAY
; PURPOSE: Plot ring points
;-------------------------------------------------------------------------------
PRO caviar_rings_display, rings, dispParams, nl, offset, ZOOMFACTOR=zf
	
	n = N_ELEMENTS(rings)
	IF dispParams.state NE 1 || n EQ 0 THEN RETURN
	
	IF NOT KEYWORD_SET(zf) THEN zf=1
			
	; Get display parameters
	color   = color(dispParams.color, /SILENT)	;Be careful, color function changes the color table! 
	psym    = dispParams.psym
	symsize = dispParams.symsize
	
	; Display planets and rings
	xr = DBLARR(n)
	yr = DBLARR(n)
	FOR i=0,n-1 DO BEGIN
		ringi = rings[i]
		xr = zf * ( ringi.xcoord )          - offset[0]
		yr = zf * ( (nl-1) - ringi.ycoord ) - offset[1]
;		PLOTS, xr, yr, /DEVICE, COLOR=color, PSYM=psym, SYMSIZE=symsize
;njc:
               PLOTS, xr, yr, /DEVICE, COLOR=color, PSYM=1, SYMSIZE=0.8*zf*symsize
	ENDFOR

END	

FUNCTION caviar_rings_loadSaturnFring, startlon, stoplon, n_points, et, sc

	IF n_points LE 1 THEN MESSAGE, "'N_POINTS' variable must be greater than 1!"
	
	;***************************************************************************
	; Compute rotation matrix from local to J2000 referential:
	;***************************************************************************
	; Convert F-ring epoch from characters to seconds past J2000
	cspice_str2et, '1980 NOV 12 23:46:00.0 UTC', frbepochet
	deltat = (et-frbepochet)/(86400.0d0*365.25d0)	;in decimal years
	
	; Compute F-ring pole direction (RA/dec):
	; The values for the pole direction of Saturn are taken from Bosh et al. 
	; (2002) and are required for correct implementation of the model.
	frpra  = 40.59287d0 + (-0.00061772d0)*deltat
	frpdec = 83.53833d0 + (-0.00006420d0)*deltat
	capN = (frpra+90)  * !DTOR
	capJ = (90-frpdec) * !DTOR
	
	rot = [ [cos(capN), -sin(capN)*cos(capJ),  sin(capN)*sin(capJ)], $
			[sin(capN),  cos(capN)*cos(capJ), -cos(capN)*sin(capJ)], $
			[	0.0d0 ,  		   sin(capJ), 		     cos(capJ)] ]
			
	;***************************************************************************
	; Set F-ring parameters:
	;***************************************************************************
	; Convert F-ring epoch from characters to seconds past J2000
	cspice_str2et, '2000 JAN 01 12:00:00.0 TDB', f_ring_epoch
	
;Bosh et al (2002):
;	a = 140223.7d0
;	e = 0.00254d0
;	inc = 0.0065d0 * !DTOR
;	curly_pi0 = 24.1d0 * !DTOR
;	big_omega0 = 16.1d0 * !DTOR
;	beta  =  2.7001d0 * !DTOR/86400.0d0
;	gamma = -2.6876d0 * !DTOR/86400.0d0

;Cooper et al (2013) Fit 11:
        a = 140223.92d0 
        e = 2.3636d-3       
        inc = 0.00568d0 * !DTOR 
        curly_pi0 =  8.8d0 * !DTOR
        big_omega0 = 5.3d0 * !DTOR
        beta  =  2.7052d0 * !DTOR/86400.0d0
        gamma = -2.6859d0 * !DTOR/86400.0d0

	startlon = FLOAT(startlon)
	stoplon  = FLOAT(stoplon)	
	tstoplon = stoplon
	IF tstoplon LT startlon THEN tstoplon+=360.0d0
	lonstep = (tstoplon-startlon)/n_points
	
	;***************************************************************************
	; Initialize variables:
	ras = DBLARR(n_points) & decs = DBLARR(n_points)
	ltime = 0.0d0
	
	xyz=dblarr(3)
	J2000_xyz=dblarr(3)
	pf_xyz=dblarr(3)
	
	cspice_spkez, 699L, et, 'J2000', 'NONE', sc, state, ltime_dummy
	rho_planet = state[0:2]
	
	
	FOR i=0L, n_points-1L DO BEGIN
		curly_pi = curly_pi0 + beta*(et-ltime-f_ring_epoch)
		curly_pi = curly_pi - 2*!dpi*FIX(curly_pi/(2*!dpi))
		IF curly_pi LT 0 THEN curly_pi+=2*!dpi
		
		big_omega = big_omega0 + gamma*(et-ltime-f_ring_epoch)
		big_omega = big_omega - 2*!dpi*FIX(big_omega/(2*!dpi))
		IF big_omega LT 0 THEN big_omega+=(2*!dpi)
		
		little_omega = curly_pi-big_omega
		little_omega = little_omega - 2*!dpi*FIX(little_omega/(2*!dpi))
		IF little_omega LT 0 THEN little_omega+=2*!dpi
		
		
		rot1=[ [cos(little_omega), -sin(little_omega), 	   0.0d0], $
			   [sin(little_omega),  cos(little_omega),     0.0d0], $
			   [			0.0d0,				0.0d0,     1.0d0] ]
		rot2=[ [1.0d0, 	  0.0d0,     0.0d0], $
			   [0.0d0, cos(inc), -sin(inc)], $
			   [0.0d0, sin(inc),  cos(inc)] ]
		rot3=[ [cos(big_omega), -sin(big_omega), 0.0d0], $
			   [sin(big_omega),  cos(big_omega), 0.0d0], $
			   [		 0.0d0,			  0.0d0, 1.0d0] ]

		f = (startlon + i*lonstep)*!DTOR - curly_pi
		xyz = [ a*(1-e^2)*cos(f)/(1+e*cos(f)), a*(1-e^2)*sin(f)/(1+e*cos(f)), 0.0d0 ]		
		
		pf_xyz = rot3##rot2##rot1##xyz
		J2000_xyz = rot##pf_xyz
		rho_ring = rho_planet+J2000_xyz
		ltime = NORM(rho_ring)/299792.458d0
		;not iterating for light time. For any reasonable light time the location of a particular
		;longitude in the F-ring w.r.t. the planet will not change.

		cspice_spkez, 699L, et-ltime, 'J2000', 'NONE', sc, state, ltime_dummy
		rho_planet = state[0:2]
		
		rho_ring = rho_planet+J2000_xyz
		; This corrects for aberration assuming the F ring is a non-rotating 
		; solid wire in a fixed position w.r.t. the planet.
	
    	cspice_recrad, rho_ring, range, RA, dec
    	ras[i] = RA
    	decs[i] = dec
	ENDFOR
	
	; Get ring points image coordinates:
	radec2slcoord, ras, decs, samples, lines
	
	; Create ring structure:
	sFring = {PLANETID: 699, RA: ras, dec: decs, xcoord: samples, ycoord: lines}
	
	RETURN, sFring
END


FUNCTION caviar_rings_load, radius, startlon, stoplon, n_points, $
						 planet_id, planet_polera, planet_poledec, et, sc
	
	IF n_points LE 1 THEN MESSAGE, "'N_POINTS' variable must be greater than 1!"
	
	tstoplon = stoplon
	IF tstoplon LT startlon THEN tstoplon+=360.0
	lonstep = FLOAT(tstoplon-startlon)/n_points
	
	ras = DBLARR(n_points) & decs = DBLARR(n_points)
	
	capN = (planet_polera+90)*!DTOR
	capJ = (90-planet_poledec)*!DTOR
	rot = [ [ cos(capN), -sin(capN)*cos(capJ),  sin(capN)*sin(capJ)], $
			[ sin(capN),  cos(capN)*cos(capJ), -cos(capN)*sin(capJ)], $
			[ 	    0.0, 			sin(capJ), 			  cos(capJ)] ]
		
	cspice_spkez, planet_id, et, 'J2000', 'CN+S', sc, state, ltime_dummy

	FOR i=0, n_points-1 DO BEGIN
		pntLongitude = ( startlon + i*lonstep )*!DTOR
		
		; Compute cartesian coordinates (x,y,z) of a circle with the input radius
		; in J2000 reference frame centered on the planet.
		xyz = [ radius*cos(pntLongitude), radius*sin(pntLongitude), 0.0d0 ]
		J2000_xyz = TRANSPOSE(rot##xyz)
		
		; Get cartesian coordinates (x,y,z) centered on the spacecraft
		rho_planet = state[0:2]
		rho_ring = rho_planet+J2000_xyz
	
		; Correct for light-time between the ring points and the spacecraft
		; No iteration here to find light time because the position of a 
		; particular inertiallongitude in a circular ring is constant w.r.t. the 
		; centre of the planet. This allows for aberration, modelling the ring 
		; as a non-rotating solid disc centred on the planet.
		ltime = NORM(rho_ring)/299792.458d0
		cspice_spkez, planet_id, et-ltime, 'J2000', 'CN+S', sc, state, ltime_dummy
		rho_planet = state[0:2]
		rho_ring = rho_planet+J2000_xyz
		
		; Convert cartesian coordinates to RA/dec
    	cspice_recrad, rho_ring, range, RA, dec
    	ras[i] = RA
    	decs[i] = dec
	ENDFOR
	
	; Get ring points image coordinates:
	radec2slcoord, ras, decs, samples, lines

	; Create ring structure:
	sRing = {PLANETID: planet_id, RADIUS:radius, NPOINTS:n_points, $
		STARTLON:startlon, STOPLON:stoplon, $
		RA: ras, DEC: decs, XCOORD: samples, YCOORD: lines}
												 
	RETURN, sRing
END

;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
; NAME: CAVIAR_RINGS_GUI_DEFTBLVALUES
; PURPOSE: 
;-------------------------------------------------------------------------------
FUNCTION caviar_rings_gui_deftblvalues
	
	COMMON CAVIAR_DATA, image, stars, imgStars, planets, rings
	
	IF haveTag(image, 'PLANET_ID') THEN BEGIN
		pid = image.PLANET_ID
		CASE pid OF
			599:  radius = [128940.0D, 122500.0D]
			699:  radius = [136744.4D, 133423.53D, 136488.2D]
			ELSE: RETURN, tblVali
		ENDCASE
	ENDIF
	nrings = N_ELEMENTS(radius)
	
	tblVali = {PID:'', RADIUS:'', NPTS:'3000', RSTART:'0', RSTOP:'360'}
	tblValues = REPLICATE(tblVali, nrings > 10)
	
	FOR i=0, nrings-1 DO BEGIN
		tblVali.PID = STRING(pid, '(I3)')
		tblVali.RADIUS = STRING(radius[i], '(F0.3)')
		tblvalues[i] = tblVali
	ENDFOR
	
	RETURN, tblValues
END

;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
; NAME: CAVIAR_RINGS_GUI_GETPARAMS
; PURPOSE: Get rings parameters list of structures
;-------------------------------------------------------------------------------
FUNCTION caviar_rings_gui_getparams, tblValues
	
	ringi = {PID:0L, RADIUS:0D, NPTS:0, RSTART:0., RSTOP:0.}
	
	IF N_ELEMENTS(tblValues) EQ 0 THEN RETURN, ringi
	
	ringsParams = LIST()
	i = 0
	FOREACH tblVal, tblValues DO BEGIN
		
		IF tblVal.PID    NE '' THEN ringi.PID    = LONG(tblVal.PID)		 ELSE CONTINUE
		IF tblVal.RADIUS NE '' THEN ringi.RADIUS = DOUBLE(tblVal.RADIUS) ELSE CONTINUE
		IF tblVal.NPTS   NE '' THEN ringi.NPTS   = FIX(tblVal.NPTS)      ELSE CONTINUE
		IF tblVal.RSTART NE '' THEN ringi.RSTART = FLOAT(tblVal.RSTART)  ELSE CONTINUE
		IF tblVal.RSTOP  NE '' THEN ringi.RSTOP  = FLOAT(tblVal.RSTOP)   ELSE CONTINUE
		
		ringsParams.ADD, ringi
		i++
	ENDFOREACH
	
	RETURN, ringsParams
END


;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
; NAME: CAVIAR_GETRING_GUI_EVENT
; PURPOSE: Manage events from the widget
;-------------------------------------------------------------------------------
PRO caviar_rings_gui_event, event
	
	COMMON CAVIAR_GUI, wCAVIARtlb, wFITSATLIMBbase, wLOADRINGSbase
	COMMON CAVIAR_DATA, image, stars, imgStars, planets, rings
	
	; Get state from the first child of the compound widget root:
	WIDGET_CONTROL, event.HANDLER, GET_UVALUE=pstate
	WIDGET_CONTROL, /HOURGLASS
	
	CATCH, error_status
	IF error_status NE 0 THEN BEGIN
		res = DIALOG_MESSAGE(!ERROR_STATE.MSG, DIALOG_PARENT=event.handler, /CENTER)
		RETURN	
	ENDIF
	
	uname = WIDGET_INFO(event.id, /UNAME)
	CASE uname OF
						
		; Add field value to radius list when press button
		"ADD": WIDGET_CONTROL, (*pstate).wRPtbl, /INSERT_ROWS
		
		; Remove selected radius from the list
		"REMOVE": BEGIN
			WIDGET_CONTROL, (*pstate).wRPtbl, GET_VALUE=tblValues
			nlines = N_ELEMENTS(tblValues)
			cellsel = WIDGET_INFO((*pstate).wRPtbl, /TABLE_SELECT)
			ncellsel = (cellsel[1] EQ -1) ? nlines : (cellsel[3]-cellsel[1])+1
			
			IF nlines EQ ncellsel $
			THEN WIDGET_CONTROL, (*pstate).wRPtbl, $
					SET_VALUE={PID:'', RADIUS:'', NPTS:'3000', RSTART:'0', RSTOP:'360'} $
			ELSE WIDGET_CONTROL, (*pstate).wRPtbl, /DELETE_ROWS
			
			WIDGET_CONTROL, (*pstate).wRPtbl, GET_VALUE=tblValues
			n = N_ELEMENTS(tblValues)
			WIDGET_CONTROL, (*pstate).wRPtbl, SET_TABLE_SELECT=cellsel<(n-1)>0
		END
		
		; Restore defaults radius
		"RESTORE": BEGIN
			ringsTblValues = caviar_rings_gui_deftblvalues()
			WIDGET_CONTROL, (*pstate).wRPtbl, TABLE_YSIZE=N_ELEMENTS(ringsTblValues)
			WIDGET_CONTROL, (*pstate).wRPtbl, SET_VALUE=ringsTblValues, ALIGNMENT=1
		END
				
		; Start getting image rings 
		"LOAD": BEGIN
			WIDGET_CONTROL, (*pstate).wRMbgroup, GET_VALUE=ringsModels
			WIDGET_CONTROL, (*pstate).wRPtbl, GET_VALUE=ringsTblValues
			rp = caviar_rings_gui_getparams(ringsTblValues)
			
			; Get circular rings:
			rings = LIST()
			FOR i=0, N_ELEMENTS(rp)-1 DO BEGIN
				rpi = rp[i]
				FOREACH planet, planets DO IF planet.ID EQ rpi.PID THEN p = planet
				IF p EQ !NULL THEN MESSAGE, "'PID' must refer to an available parent."
				ringi = caviar_rings_load(rpi.RADIUS, rpi.RSTART, rpi.RSTOP, rpi.NPTS, $
								p.ID, p.POLERA, p.POLEDEC, image.ET, image.SPC.ID)
				rings.ADD, ringi
			ENDFOR
			
			; Get rings models:
			IF ringsModels EQ 1 THEN BEGIN
				ringi = caviar_rings_loadSaturnFring(0, 360, 3000, image.ET, image.SPC.ID)
				rings.ADD, ringi
			ENDIF
			
			; Display rings
			caviar_display
		END
	
		"CLOSE": WIDGET_CONTROL, event.TOP, /DESTROY
		
		ELSE:
	ENDCASE
END

;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
; NAME:
;	caviar_getRing_gui
;
; PURPOSE:
;	Create a widget or compound widget (depend if the "PARENT" keyword is set) ...
;
; CATEGORY:
;
; CALLING SEQUENCE:
;	caviar_getRing_gui[, PARENT=base]
;
; INPUTS:
;	All input parameters are passed as keywords or in common block
;
; KEYWORDS:
;	PARENT: Set this keyword to the parent widget id in witch you want to place 
;		this compound widget.
; OUTPUTS:
;	No explicit outputs.
;
; COMMON BLOCKS:
;
; SIDE EFFECTS:
;	A window is created if no "PARENT" keyword have been set.
;
; MODIFICATION HISTORY:
;	2012, May			Louis-Etienne MEUNIER 		IMCCE>OBSPM
;		- Written
;	2014, January			Louis-Etienne MEUNIER 		IMCCE>OBSPM
;		- 		
;-------------------------------------------------------------------------------
PRO caviar_rings_gui, PARENT=wMAINbase, GROUP_LEADER=groupLeader, $
						XOFFSET=xoffset, YOFFSET=yoffset
	
	COMMON CAVIAR_DATA, image, stars, imgStars, planets, rings
		
	; Test if the widget has already been launched
	IF(XRegistered('caviar_getRing_gui') NE 0) THEN RETURN
	
	IF N_ELEMENTS(image) EQ 0 $
	THEN MESSAGE, "Please load an image first!"

	IF N_ELEMENTS(planets) EQ 0 $
	THEN MESSAGE, "Please load planet and satellites first!"
		
	;***************************************************************************
	; Set parameters:
	;***************************************************************************
	; Set rings table parameters:
	tblValues = caviar_rings_gui_deftblvalues()
	tblLabels = ["PID", "Radius (km)", "# points", "Start (deg)", "Stop (deg)"]
	tblColWidth = [30, 75, 60, 75, 75]
	
	; If planets have already been found, set the list. Otherwise, define empty list:
	pList = LIST()
	FOREACH p, planets DO pList.ADD, STRING(p.ID,'(I3)')+' - '+p.NAME
	pList = pList.TOARRAY()
	
	; Set rings model parameters:
	ringsModels = 0
	
	;***************************************************************************
	; Define the widget base:
	;***************************************************************************
	IF NOT KEYWORD_SET(title) THEN title = "Rings"
	
	IF KEYWORD_SET(wMAINbase) THEN BEGIN
		IF NOT WIDGET_INFO(wMAINbase, /VALID_ID) $
		THEN res=DIALOG_MESSAGE('Invalid parent widget identifier.',/ERROR)
		isTopBase = 0
	ENDIF ELSE BEGIN
		isTopBase = 1
		IF KEYWORD_SET(groupLeader) THEN BEGIN
			IF NOT WIDGET_INFO(groupLeader, /VALID_ID) $
			THEN res=DIALOG_MESSAGE('Invalid group leader widget identifier.',/ERROR)
			wMAINbase = WIDGET_BASE(TITLE=title, XOFFSET=xoffset, YOFFSET=yoffset, $
								GROUP_LEADER=groupLeader, /FLOATING)
		ENDIF ELSE wMAINbase = WIDGET_BASE(TITLE=title,	XOFFSET=xoffset, YOFFSET=yoffset)
	ENDELSE
	
	;***************************************************************************
	; Define subwidgets tree:
	;***************************************************************************				
	wEXTRAbase = WIDGET_BASE(wMAINbase, /COLUMN)
	
	wbase = WIDGET_BASE(wExtraBase, /COL, XPAD=0, YPAD=0, SPACE=0)
		wsbase1 = WIDGET_BASE(wbase, /ROW, XPAD=0, YPAD=0)
			wRPbase = WIDGET_BASE(wsbase1, /COL)
			
				wRPsbase = WIDGET_BASE(wRPbase, /ROW, /ALIGN_CENTER)
					wRPlbl = WIDGET_LABEL(wRPsbase, VALUE="Circular rings:  ", /ALIGN_LEFT)
					wRPADDbttn = WIDGET_BUTTON(wRPsbase, XSIZE=50, $
									UNAME="ADD", VALUE="Add")
					wRPRMVbttn = WIDGET_BUTTON(wRPsbase, XSIZE=55, $
									UNAME="REMOVE", 	VALUE="Remove")
					wRPRSTbttn = WIDGET_BUTTON(wRPsbase, XSIZE=115, $
									UNAME="RESTORE", VALUE="Restore defaults")
								
				wRPtbl = WIDGET_TABLE(wRPbase, /ROW_MAJOR, /NO_ROW_HEADERS, $
							ALIGNMENT=1, Y_SCROLL_SIZE=10, YSIZE=10, $
							COLUMN_LABELS=tblLabels, COLUMN_WIDTHS=tblColWidth, $
							VALUE=tblValues, EDITABLE=1, BACKGROUND_COLOR=!COLOR.white)	
			
			wPLbase = WIDGET_BASE(wsbase1, /COL, /ALIGN_BOTTOM, /BASE_ALIGN_CENTER, SPACE=6)
				wPLlbl = WIDGET_LABEL(wPLbase, VALUE="Available parents (PID)")
				wPLlist = WIDGET_LIST(wPLbase, VALUE=pList, XSIZE=18, YSIZE=15)
			
		
		wRMbase = WIDGET_BASE(wbase, /ROW, SCR_YSIZE=(planets[0].ID EQ 699) ? 29 : 1)
			wRMlbl = WIDGET_LABEL(wRMbase, VALUE="Ring model:")
			wRMbgroup = CW_BGROUP(wRMbase, ['Saturn F-ring'], UNAME="MODELS", $
					/NONEXCLUSIVE, /ROW, /NO_RELEASE, SET_VALUE=ringsModels)
		

	wGRbase = WIDGET_BASE(wEXTRAbase, /ROW, /ALIGN_CENTER, SPACE=50)
		wLOADbttn = WIDGET_BUTTON(wGRbase, VALUE="LOAD", UNAME="LOAD", XSIZE=70, YSIZE=30)	
		IF isTopBase THEN $
		wCLOSEbttn = WIDGET_BUTTON(wGRbase, VALUE="CLOSE", UNAME="CLOSE", XSIZE=70)

	;***************************************************************************
	; Create the widget (i.e. display it) and save variables into 'extra' base:
	;***************************************************************************
	WIDGET_CONTROL, wMAINbase, /REALIZE							
	
	state = {wRPtbl:wRPtbl, wRMbgroup:wRMbgroup}
	WIDGET_CONTROL, wExtraBase, SET_UVALUE=PTR_NEW(state, /NO_COPY)
		
		
	; Let XMANAGER takes control of widgets event processing
	XMANAGER, 'caviar_rings_gui', wEXTRAbase, /JUST_REG, GROUP_LEADER=groupLeader
END
