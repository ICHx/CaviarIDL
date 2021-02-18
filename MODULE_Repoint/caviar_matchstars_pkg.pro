;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
; List of procedures/functions of this file:
;	PRO caviar_unmatchStars
;	PRO caviar_matchStars
;	PRO caviar_matchStars_movepntg
;	PRO caviar_matchStars_mouseMove
;	PRO caviar_matchStars_gui_init
;	PRO caviar_matchStars_gui_event
;	PRO caviar_matchStars_gui
;-------------------------------------------------------------------------------


;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
; NAME:
;   CAVIAR_UNMATCHSTARS
; PURPOSE:
;	Allow the user to remove some of the located stars list (match betwwen 
;	catalogs and image stars) by clicking on them in the image window.
;		
; CATEGORY:
;       
; CALLING SEQUENCE:
;   caviar_unmatchStars(stars, nl, winID [, THRESHOLD=threshold])
; INPUTS:
;   stars: list of structures containing "xcent" and "ycent" tags.
;	nl: number of line of the image to take into account the /ORDER options 
;		used to display this image.
;
; OUTPUTS:
;   stars: list of structures with unmatched stars.
;
; KEYWORDS:
;	THRESHOLD: Minimal cursor distance from the object to remove it.
; 
; COMMON BLOCKS:
;   None.
; SIDE EFFECTS:
;   None.
;
; MODIFICATION HISTORY:
;	2012, january		MEUNIER L-E		OBSPM>IMCCE
;		- Written
;	2012, August		MEUNIER L-E		OBSPM>IMCCE
;		- Updated to follow CaVIaR modifications
;	2013, December		MEUNIER L-E		OBSPM>IMCCE
;		- Now manage offset of the image drawing viewport.
;-------------------------------------------------------------------------------
PRO caviar_unmatchStars, stars, nl, winID, wlblID, THRESHOLD=threshold
	
	COMMON CAVIAR_PARAMS, dispParams, imgDraw, dispProName, starsCatalogs, find_imgStars_params
	
	nstars = N_ELEMENTS(stars)
	
	; Extract located stars
	nmatch = 0
	FOR i=0, nstars-1 DO IF (stars[i]).matched EQ 1 THEN nmatch++
	IF nmatch EQ 0 THEN MESSAGE, "No stars have been matched!"
					
	IF ~KEYWORD_SET(threshold) THEN threshold=10
	zf = FLOAT(imgDraw.ZFACTOR)
	xoffset = imgDraw.OFFSET[0]
	yoffset = imgDraw.OFFSET[1]
		
	; Extraction of image coordinates from the object's structures list
	xcoords = DBLARR(nstars)
	ycoords = DBLARR(nstars)
	FOR i=0, nstars-1 DO BEGIN
		IF (stars[i]).MATCHED EQ 1 THEN BEGIN
			xcoords[i] = (stars[i]).xcent
			ycoords[i] = (stars[i]).ycent
		ENDIF ELSE BEGIN
			xcoords[i] = -99
			ycoords[i] = -99
		ENDELSE
	ENDFOR
	
	PRINT, FORMAT='(A,I0,A,/,A)', $
			'Click on objects to unmatch them, (',threshold,' pixels around)...', $
			'Leave image window or right-click to return.
	
	; Set from which window to get the cursor position and set cursor shape:
	WSET, winID
	DEVICE, CURSOR_STANDARD=34
	
	WIDGET_CONTROL, wlblID, SET_VALUE="Move to image window..."
	
	count = 0
	REPEAT BEGIN
		CURSOR, xcursor, ycursor, /NOWAIT, /DEVICE
		
		; Staying outside the window for more than 5 seconde stops the procedure
		IF xcursor EQ -1 OR ycursor EQ -1 THEN count++ ELSE count = 0
		IF count GT 5000 THEN BREAK
	ENDREP UNTIL xcursor NE -1 AND ycursor NE -1
	
	WIDGET_CONTROL, wlblID, SET_VALUE="Click on objects..."
	
	count = 0
	REPEAT BEGIN
		
		CURSOR, xcursor, ycursor, /NOWAIT, /DEVICE

		; Leaving the window for more than 500 milliseconde stops the procedure
		IF xcursor EQ -1 OR ycursor EQ -1 THEN count++ ELSE count = 0
		IF count GT 300 THEN BREAK
		
		; A right-click on the mouse stops the procedure
		IF !mouse.button EQ 4 THEN BREAK
		
		; Compute distance of the cursor from stars objects:
		xcursor = (xcursor+xoffset)/zf
		ycursor = (nl-1) - (ycursor+yoffset)/zf
		dist2 = (xcoords-xcursor)^2 + (ycoords-ycursor)^2
		min_dist = SQRT(MIN(dist2, min_index))
		
		IF min_dist GT threshold THEN BEGIN
			DEVICE, CURSOR_STANDARD=34
			CONTINUE
		ENDIF ELSE DEVICE, CURSOR_STANDARD=58
		
		; A left-click on the mouse allows a star to be unmatched:
		IF !mouse.button EQ 1 THEN BEGIN
			tmpStar = stars[min_index]
			tmpStar.MATCHED = 0
			stars[min_index] = tmpStar
			PRINT, 'Star '+(stars[min_index]).name+' has been unmatched!
			caviar_display
		ENDIF
	ENDREP UNTIL !mouse.button EQ 4
	
	DEVICE, CURSOR_STANDARD=34
	
	RETURN
END

;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
; NAME:
;		MATCHSTARS
;
; PURPOSE:
;		Compare distance between x/y coordinates of stars found in catalogs 
;		(catStars) with those found in image (imgStars). If this distance is 
;		lower than the threshold (default is 4 pixels), the flag "MATCHED" of
;		the stars structure is set to 1.
;
; CALLING SEQUENCE:
;	matchStars, catStars, imgStars[, THRESHOLD=threshold]
;
; INPUTS:
;	catStars: List of structures as define in "caviar_getCatStars".
;	imgStars: List of structures as define in "caviar_getImgStars".
;
; OPTIONAL KEYWORDS:
;	THRESHOLD: Maximum distance in pixels for matching catalogs star with 
;			image stars. Default value is 4 pixels.
;
; OUTPUT:
;	catStars: List of structure containing these tags:
; 		- Tags from input "catStars": 'name', 'mag', 'RA', 'dec', 'Xcoord', 'Ycoord'
;		- 'located': Integer scalar equal to 0 (no match with image stars) or 1 (match succed)
;		- If match succed, the "imgStars"'s tags: 'Xcent', 'Ycent', 'Flux', 'Sharpness', 'Roundness'
; MODIFICATIONS:
;	2012, March			MEUNIER L-E				OBSPM>IMCCE
;		- Written
;---------------------------------------------------------------------------------------------------
PRO caviar_matchStars, catStars, imgStars, THRESHOLD=threshold
	
	If ~KEYWORD_SET(threshold) THEN threshold=4
	
	n_catStars = N_ELEMENTS(catStars)
	n_imgStars = N_ELEMENTS(imgStars)
	
	
	IF n_imgStars EQ 0 || n_catStars EQ 0 THEN BEGIN
		MESSAGE, "No image and/or catalogs stars have been found! Procedure will return."
		RETURN
	ENDIF
			
	imgStars_xcoords = FLTARR(n_imgStars)
	imgStars_ycoords = FLTARR(n_imgStars)
	FOR i=0,n_imgStars-1 DO BEGIN
		imgStars_xcoords[i] = (imgStars[i]).xcent
		imgStars_ycoords[i] = (imgStars[i]).ycent
	ENDFOR	
	
	
	FOR i=0,n_catStars-1 DO BEGIN
		catStar_i = catStars[i]
		
		; Compute distances between catalogue star i and all image stars
 		dx = REPLICATE(catStar_i.xcoord, n_imgStars)-imgStars_xcoords
	   	dy = REPLICATE(catStar_i.ycoord, n_imgStars)-imgStars_ycoords
	   	d = SQRT(dx^2 + dy^2)
	   	dmin = MIN(d, pos)
		IF dmin GT threshold THEN catStar_i.MATCHED = 0 $
		ELSE BEGIN
			catStar_i.MATCHED = 1
			IF haveTag(catStar_i, (TAG_NAMES(imgStars[pos]))[0]) EQ 0 $
			THEN catStar_i = CREATE_STRUCT(catStar_i, imgStars[pos]) $
			ELSE STRUCT_ASSIGN, imgStars[pos], catStar_i, /NOZERO
		ENDELSE
		
		catStars[i] = catStar_i
	ENDFOR
END 

;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
; NAME: caviar_matchStars_movepntg
;
; PURPOSE: 
;	Given a shifting vector (in pixel) or an angle (in radian), the procedure...
;		- compute the translated/rotated pointing vector coordinates (in ra/dec), 
;		- update the pointing matrix (cmat),
;		- update the display.
;
; INPUT KEYWORDS: 
;	shift: 2-Elements array with x and y translation distance in image referential (in pixel)
;	angle: Scalar representing the angle to rotate the camera pointing vector (in radian)
;
; OUTPUT:
;	None
;
; SIDE EFFECTS:
;	- Change pointing matrix (cmat)
;	- Change objects coordinates
;	- Update display
;
; MODIFICATIONS:
;	2012 October 9			MEUNIER L-E				OBSPM>IMCCE
;		- Add a verification that the camera pointing matrix (cmat) has been loaded
;		- Add reset (i.e. set to 0) of stars with 'located' tag set to 1
;		- Rename from 'update_pointing' to 'manual_repoint'
;-------------------------------------------------------------------------------
PRO caviar_matchStars_movepntg, TRANSLATE=shift, ROTATE=angle
	
	COMMON CAVIAR_DATA, image, catstars, imgstars, satellites, rings
	
	IF NOT haveTag(image,'CMAT') THEN $
		MESSAGE, "You need to get the camera pointing matrix first!"
		
	cspice_m2eul, image.cmat, 3, 1, 3, ang3, ang2, ang1
	twist = ang3
	dec = 0.5*!dpi - ang2
	ra = ang1 - 0.5*!dpi
	IF ra	 LT 0 THEN ra    += 2*!dpi
	IF twist LT 0 THEN twist += 2*!dpi

	
	IF ISA(shift, /ARRAY) EQ 1 THEN BEGIN					
		; Definition of rotation matrix between image (x/y) and equatorial (ra/dec) referential
		theta = -(twist + 0.5*!dpi)
		rot = [ [ cos(theta), -sin(theta)], $
				[ sin(theta),  cos(theta)] ]
	
		; Convert shift vector in image referential from pixel to radian
		shift *= image.fovpix
	
		; Transformation of shifting vector from image referential to equatorial
		shiftJ2000 = rot##shift
	
		; Compute new equatorial coordinates
		dec +=  shiftJ2000[0]
		ra  += (shiftJ2000[1]/cos(dec))
	ENDIF
	
	IF ISA(angle, /SCALAR) NE 0 THEN twist += angle

	; Reset the 'MATCHED' flag of catstars to "0" because previous "match" 
	; operation is no longer relevant:
	FOR i=0, N_ELEMENTS(catstars)-1 DO BEGIN
		stari = catstars[i]
		stari.MATCHED = 0
		catstars[i] = stari
	ENDFOR
	
	; Update cmat, objects coordinates and display:
	cspice_eul2m, twist, 0.5*!dpi-dec, 0.5*!dpi+ra, 3, 1, 3, cmat
	image.CMAT = cmat
	
	caviar_updtCoords, catstars, image.vobs_stars
	caviar_updtCoords, satellites
	caviar_updtCoords, rings
	caviar_display	
	RETURN
END

;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
; NAME: CAVIAR_MATCHSTARS_MOUSEMOVE
; PURPOSE: Allow user to translate or rotate graphical objects in a window by 
;		moving the mouse in it. This routine calls an external routine with a  
;		translation vector or a rotation angle as parameter allowing the user to 
;		manage object(s) motion.
;
;		Translation vector: 2-Elements array of double value for x and y translation.
;		Rotation angle: Scalar double representing an angle in radian.
;
; KNOWN ISSUES:
;		To use this routine on Mac OSX, the X11 preferences have to be modified.  
;		In the "windows" tab, check all the 3 options (don't know which 
;		particular option cause the problem !).
; MODIFICATIONS:
;	2012 February, 14		MEUNIER L-E				OBSPM>IMCCE
;		- Add header and comments
;		- Cosmetics, variables name
;	2012 June				MEUNIER L-E				OBSPM>IMCCE
;		- Move computation block to external routine to be used by an other user  
;		  interactive procedure.
;		- Change algorithme to get better user experience
;	2012 October, 9			MEUNIER L-E				OBSPM>IMCCE
;		- Make this routine independant from final purpose. Now, just compute 
;		  shift and angle values, and give them to the procedure caller.
;		- Add 'RCENTER' to set his own center.
;-------------------------------------------------------------------------------	
PRO caviar_matchStars_mouseMove, wlblID, TFACTOR=tFactor, RFACTOR=rFactor, RCENTER=centerVec
    
    COMMON CAVIAR_DATA, image
    
    ; Save current window and set new:
	old_win = !D.WINDOW
	WSET, image.window
    
    ; Print user informations:
    PRINT, "Press a button and move the mouse over the window:"            	
	IF KEYWORD_SET(tFactor) THEN PRINT, "    Left button   => translate
	IF KEYWORD_SET(rFactor) THEN PRINT, "    Middle button => quit 
	PRINT, "    Right button  => rotate"
		
	
	IF NOT KEYWORD_SET(centerVec) THEN centerVec=[!D.X_SIZE/2.0D, !D.Y_SIZE/2.0D]
	
	WIDGET_CONTROL, wlblID, SET_VALUE="Move to image window..."
	
	count = 0
	REPEAT BEGIN
		CURSOR, xcursor, ycursor, /NOWAIT, /DEVICE
		
		; Staying outside the window for more than 5 seconde stops the procedure
		IF xcursor EQ -1 OR ycursor EQ -1 THEN count++ ELSE count = 0
		IF count GT 5000 THEN BEGIN
			WSET, old_win
			RETURN
		ENDIF
	ENDREP UNTIL xcursor NE -1 AND ycursor NE -1
	
	WIDGET_CONTROL, wlblID, SET_VALUE="Press left/right button to translate/rotate..."
	
	
	CURSOR, xmA, ymA, /NOWAIT, /DEVICE
	ptA = [xmA, ymA]
	
	count = 0
	working=1
	WHILE working EQ 1 DO BEGIN
		
		CURSOR, xmB, ymB, /NOWAIT, /DEVICE
		ptB = [xmB, ymB]
		
		; Leaving the window for more than 300 milliseconde stops the procedure
		IF xmB EQ -1 OR ymB EQ -1 THEN count++ ELSE count = 0
		IF count GT 300 THEN BREAK
		
		CASE !mouse.button OF
			1: BEGIN	; Left button -> compute translation vector:
				DEVICE, CURSOR_STANDARD=52
				IF ~ARRAY_EQUAL(ptA,ptB) && KEYWORD_SET(tFactor) $
				THEN caviar_matchStars_movepntg, TRANSLATE = tFactor * [ptB[0]-ptA[0], ptB[1]-ptA[1]]
			END
			4: BEGIN	; Right button -> compute rotation angle
				IF ~ARRAY_EQUAL(ptA,ptB) && KEYWORD_SET(rFactor) THEN BEGIN
					ptA_c = ptA - centerVec
					ptB_c = ptB - centerVec
					
					angle = atan(ptB_c[1]/ptB_c[0])-atan(ptA_c[1]/ptA_c[0])
					IF angle LT -!DPI/2 THEN angle += !DPI
					IF angle GT  !DPI/2 THEN angle -= !DPI
					
					caviar_matchStars_movepntg, ROTATE=rFactor*angle
				ENDIF
			END
			2: working=0	;Right click -> quit the loop and stop the procedure
			ELSE: DEVICE, CURSOR_STANDARD=34
		ENDCASE
		
		; Mouse position B become mouse position A
		ptA = ptB
	ENDWHILE
	
	; Set old window:
	WSET, old_win
	
	RETURN
END

;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
; NAME: CAVIAR_MATCHSTARS_GUI_INIT
; PURPOSE: Set default values/labels/parameters/etc. of the GUI.
;-------------------------------------------------------------------------------
PRO caviar_matchStars_gui_init
	COMMON CAVIAR_MATCHSTARS, wMATCHlbl, wUNMATCHlbl
	
	WIDGET_CONTROL, wMATCHlbl,  SET_VALUE='0 found'
	WIDGET_CONTROL, wUNMATCHlbl, SET_VALUE='0 kept'
END

;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
; NAME: CAVIAR_MATCHSTARS_GUI_EVENT
; PURPOSE: Manage events of the widgets
;-------------------------------------------------------------------------------
PRO caviar_matchStars_gui_event, event
	
	COMMON CAVIAR_DATA, image, catstars, imgStars, planets
	COMMON CAVIAR_MATCHSTARS, wMATCHlbl, wUNMATCHlbl
	
	IF N_ELEMENTS(image) EQ 0 THEN BEGIN
		msg = ["% CALCSATPOS_GUI:", "You need to load an image first!"]
		res = DIALOG_MESSAGE(msg, /CENTER)
		RETURN
	ENDIF
	
	CATCH, errStatus
	IF errStatus NE 0 THEN BEGIN
		res = DIALOG_MESSAGE(!ERROR_STATE.MSG, DIALOG_PARENT=event.HANDLER, /CENTER)
		
		WIDGET_CONTROL, (*pstate).wsbase[0], MAP=1
		WIDGET_CONTROL, (*pstate).wsbase[1], MAP=0
		RETURN	
	ENDIF
	
	
	; Get state from the first child of the compound widget root:
	WIDGET_CONTROL, event.HANDLER, GET_UVALUE=pstate
	
	uname = WIDGET_INFO(event.id, /UNAME)
	CASE uname OF
		
		;***********************************************************************
		"MOVE": BEGIN
			CASE event.type OF
				1:  caviar_matchStars_movepntg, TRANSLATE = event.value
				2:  caviar_matchStars_movepntg, TRANSLATE = event.value
				3:  caviar_matchStars_movepntg, TRANSLATE = event.value
				4:  caviar_matchStars_movepntg, TRANSLATE = event.value
				5:  BEGIN
						WIDGET_CONTROL, (*pstate).wsbase[0], MAP=0
						WIDGET_CONTROL, (*pstate).wsbase[1], MAP=1
			
						caviar_matchStars_mouseMove, (*pstate).wHELPtxt, $
							TFACTOR=event.value.tFactor, RFACTOR=event.value.rFactor
							
						WIDGET_CONTROL, (*pstate).wsbase[0], MAP=1
						WIDGET_CONTROL, (*pstate).wsbase[1], MAP=0
					END
				6:  caviar_matchStars_movepntg, TRANSLATE = event.value
				7:  caviar_matchStars_movepntg, TRANSLATE = event.value
				8:  caviar_matchStars_movepntg, TRANSLATE = event.value
				9:  caviar_matchStars_movepntg, TRANSLATE = event.value
				11: caviar_matchStars_movepntg, ROTATE = event.value*!DTOR
				12: caviar_matchStars_movepntg, ROTATE = event.value*!DTOR
				ELSE:
			ENDCASE
		END
		
		;***********************************************************************
		"RESET POINTING": BEGIN
			WIDGET_CONTROL, /HOURGLASS
			FOR i=0, N_ELEMENTS(catstars)-1 DO BEGIN 
				stari = catstars[i]
				stari.MATCHED = 0
				catstars[i] = stari
			ENDFOR
			image.cmat = image.cmat_ini
			caviar_updtCoords, catstars, image.vobs_stars
			caviar_updtCoords, satellites
			caviar_updtCoords, rings
			caviar_display
		END
		
		;***********************************************************************
		"MATCH_THRESHOLD":   (*pstate).matchThresh = event.value
		"UNMATCH_THRESHOLD": (*pstate).unmatchThresh = event.value
				
		;***********************************************************************
		;* Matching image stars with catalogue stars
		"MATCH": BEGIN
			WIDGET_CONTROL, /HOURGLASS
			; Verify that stars to match exists
			IF N_ELEMENTS(imgStars) EQ 0 || N_ELEMENTS(catstars) EQ 0 THEN BEGIN
				msg = "You need to get stars from image and/or catalogs before matching them!"
				res = DIALOG_MESSAGE(msg, /CENTER)
				RETURN
			ENDIF
			
			; Match the stars and update image display		
			caviar_matchStars, catstars, imgStars, THRESHOLD=(*pstate).matchThresh
			caviar_display
			
			; Set number of matched stars
			n_matchStars = 0
			FOR i=0, N_ELEMENTS(catstars)-1 DO IF (catstars[i]).matched EQ 1 THEN n_matchStars++
			sn_matchStars = STRING(n_matchStars, '(I0)')
			WIDGET_CONTROL, wMATCHlbl,  SET_VALUE=sn_matchStars+' found'
			WIDGET_CONTROL, wUNMATCHlbl, SET_VALUE=sn_matchStars+' kept'
		END
		
		
		;***********************************************************************
		;* Removing unwanted stars by clicking on it on the the image window
		"UNMATCH": BEGIN
			WIDGET_CONTROL, (*pstate).wsbase[0], MAP=0
			WIDGET_CONTROL, (*pstate).wsbase[1], MAP=1
			
			; Remove manualy unwwanted stars	
			caviar_unmatchStars, catstars, image.nl, image.window, $
					(*pstate).wHELPtxt , THRESHOLD=(*pstate).unmatchThresh
			
			WIDGET_CONTROL, (*pstate).wsbase[0], MAP=1
			WIDGET_CONTROL, (*pstate).wsbase[1], MAP=0
			
			; Set number of located stars
			n_selStars = 0
			FOR i=0, N_ELEMENTS(catstars)-1 DO IF (catstars[i]).MATCHED EQ 1 THEN n_selStars++
			WIDGET_CONTROL, wUNMATCHlbl, SET_VALUE=STRING(n_selStars, '(I0)')+' kept'
		END			
		
		;***********************************************************************
		"CLOSE": BEGIN
			PTR_FREE, pstate
			WIDGET_CONTROL, event.TOP, /DESTROY
		END
		
		ELSE: RETURN
	ENDCASE
	
END


;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
; NAME:	CAVIAR_MATCHSTARS_GUI
; PURPOSE: Create a graphical interface to search for stars in the image. 
;		   This interface allows changes to FWHM, HMIN, ROUNDNESS and SHARPNESS 
;		   parameters.
;		
; CALLING SEQUENCE:
;		caviar_matchStars_gui[, PARENT=wMAINbase]
;
; INPUTS: None.
; OUTPUT: None.
; OPTIONAL KEYWORDS:
;	wMAINbase: Set this keyword to the parent widget id in which you want to  
;			place this compound widget.
;	
; COMMON BLOCKS:
;	CAVIAR_DATA
;	CAVIAR_MATCHSTARS
;
; PROCEDURE CALLS:
;	caviar_display				in caviar_display.pro
;		
; MODIFICATIONS:
;	2013			MEUNIER L-E				OBSPM>IMCCE
;		- Written based on the 'caviar_repoint_gui'
;-------------------------------------------------------------------------------
PRO caviar_matchstars_gui, PARENT=wMAINbase, GROUP_LEADER=groupLeader, $
		XOFFSET=xoffset, YOFFSET=yoffset, TITLE=title, $
		FRAME=frame, XPAD=xpad, YPAD=ypad, SPACE=space

	COMMON CAVIAR_MATCHSTARS, wMATCHlbl, wUNMATCHlbl
	
	; Test if widget window is already launched
	IF(XRegistered('caviar_matchStars_gui') NE 0) THEN RETURN
	
	; Parameters
	matchThreshold = 4
	unmatchThreshold = 10
	
	;***************************************************************************
	; Define the widget base:
	;***************************************************************************
	IF NOT KEYWORD_SET(title) THEN title = '3. Move, match or unmatch stars'
	
	IF KEYWORD_SET(wMAINbase) THEN BEGIN
		IF WIDGET_INFO(wMAINbase, /VALID_ID) THEN isTopBase = 0 $
		ELSE res=DIALOG_MESSAGE('Invalid parent widget identifier.',/ERROR)
	ENDIF ELSE BEGIN
		isTopBase = 1
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
	
	wEXTRAbase = WIDGET_BASE(wMainBase, /COL, XPAD=xpad, YPAD=ypad)
	
	IF NOT isTopBase THEN wLbl = WIDGET_LABEL(wEXTRAbase, VALUE=title, /ALIGN_CENTER)
	
	wbase = WIDGET_BASE(wEXTRAbase, /COL, XPAD=0, YPAD=0, SPACE=space, FRAME=frame, $
				/BASE_ALIGN_CENTER, /ALIGN_CENTER)
		
		; Move subwidgets:
		wMOVEbase = WIDGET_BASE(wbase, /COL, /BASE_ALIGN_CENTER, /ALIGN_CENTER, SPACE=0)
			wManRepMove = CW_MOVE(wMOVEbase, UNAME="MOVE", /ROW, YPAD=0)
			
		; Match subwidgets:
		wMATCHbase = WIDGET_BASE(wbase, /BASE_ALIGN_CENTER, /ROW, YPAD=0, SPACE=6)
			wMATCHbttn = WIDGET_BUTTON(wMATCHbase, VALUE="Match", $
				XSIZE=60, YSIZE=28, UNAME="MATCH")
			wMATCHlbl = WIDGET_LABEL(wMATCHbase, VALUE='0 found', XSIZE=63, /ALIGN_LEFT)
			wMATCHsgroup = CW_SGROUP(wMATCHbase, 1, UNAME="MATCH_THRESHOLD", $
				/LABEL_TOP, /ROW, FORMAT=['("Threshold: ", I2, " pixels")'], $
				MINIMUM=1, MAXIMUM=20, STEP=1, VALUE=matchThreshold, $
				X_SLIDER_SIZE=135, Y_LABEL_SIZE=14, XPAD=0, YPAD=0)
			
		; Unmatch subwidgets:
		wUNMATCHbase = WIDGET_BASE(wbase, /BASE_ALIGN_CENTER, /ROW, YPAD=0, SPACE=6)
			wUnmatchBttn = WIDGET_BUTTON(wUNMATCHbase, VALUE="UNmatch", $
					XSIZE=60, YSIZE=28, UNAME="UNMATCH")
			wUNMATCHlbl  = WIDGET_LABEL(wUNMATCHbase, VALUE='0 kept', XSIZE=63, /ALIGN_LEFT)
			wUMATCHsgroup = CW_SGROUP(wUNMATCHbase, 1, UNAME="UNMATCH_THRESHOLD", $
					/LABEL_TOP, /ROW, FORMAT='("Threshold: ", I2, " pixels")',$
					MINIMUM=1, MAXIMUM=20, STEP=1,  VALUE=unmatchThreshold, $
					X_SLIDER_SIZE=135, Y_LABEL_SIZE=14, XPAD=0, YPAD=0)
		
		; Help & restore subwidgets:
		wsbase = WIDGET_BASE(wbase)
			wsbase1 = WIDGET_BASE(wsbase, /COL, /BASE_ALIGN_CENTER, XSIZE=250, XPAD=0, YPAD=6, MAP=1)
				wResetPntgBttn = WIDGET_BUTTON(wsbase1, VALUE="Reset Pointing State", $
					UNAME="RESET POINTING", TOOLTIP=resetPntg_tooltip, XSIZE=160, YSIZE=28)
			wsbase2 = WIDGET_BASE(wsbase, /COL, XSIZE=250, XPAD=0, YPAD=0, MAP=0)
				wHELPtxt = WIDGET_TEXT(wsbase2, VALUE="", $
					XSIZE=23, YSIZE=2, /WRAP)
		
		
	IF isTopBase THEN wCLOSEbttn = WIDGET_BUTTON(wEXTRAbase, VALUE="CLOSE", UNAME="CLOSE")
	
	; Create the widget (i.e. display it)
	WIDGET_CONTROL, wMAINbase, /REALIZE
	
	state = {wsbase:[wsbase1,wsbase2], $
			wHELPtxt:wHELPtxt, $
			matchThresh: matchThreshold, $
			unmatchThresh: unmatchThreshold}
	WIDGET_CONTROL, wEXTRAbase, SET_UVALUE=PTR_NEW(state, /NO_COPY)
	
		
	; Let XMANAGER takes control of widgets event processing
	XMANAGER, 'caviar_matchStars_gui', wEXTRAbase, /JUST_REG, GROUP_LEADER=groupLeader	
END
