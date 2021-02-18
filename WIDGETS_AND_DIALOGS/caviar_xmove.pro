;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
; NAME: CAVIAR_XMOVE_CALLBACK
; PURPOSE: 
;-------------------------------------------------------------------------------
FUNCTION caviar_xmove_callback, TRANSLATE=shift, ROTATE=angle
	COMPILE_OPT HIDDEN

	COMMON CAVIAR_XMOVE, cbName, cbData
	
	errStatus = 0
    CATCH, errStatus
    IF (errStatus NE 0) THEN BEGIN
        CATCH, /CANCEL
        v = DIALOG_MESSAGE(['Unexpected error in caviar_xmove:', !ERROR_STATE.msg], /ERROR)
       RETURN, 0
    ENDIF
    
     IF (STRLEN(cbName) GT 0) THEN BEGIN
        IF N_ELEMENTS(p_cbData) NE 0 $
        THEN CALL_PROCEDURE, cbName, TRANSLATE=shift, ROTATE=angle, DATA=cbData $
        ELSE CALL_PROCEDURE, cbName, TRANSLATE=shift, ROTATE=angle
    ENDIF
    
    RETURN, 1
END

;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
; NAME: CAVIAR_XMOVE_MOUSE
; PURPOSE: Allow user to translate or rotate graphical objects in a window by moving the mouse in it.
;		This routine call an external routine with a translation vector or a rotation angle as 
;		parameter allowing the user to manage object(s) motion.
;
;			Translation vector: 2-Elements array of double value for x and y translation.
;			Rotation angle: Scalar double representing an angle in radian.
;
; KNOWN ISSUES:
;		To use this routine on Mac OSX, the X11 preferences have to be modified. In the "windows" 
;		tab, check all the 3 options (don't know witch particular option cause the problem !).
; MODIFICATIONS:
;		2012 February, 14		MEUNIER L-E				OBSPM>IMCCE
;			- Add header and comments
;			- Cosmetics, variables name
;		2012 June				MEUNIER L-E				OBSPM>IMCCE
;			- Move computation block to external routine to be used by an other 
;			  user interactive procedure.
;			- Change algorithme to get better user experience
;		2012 October, 9			MEUNIER L-E				OBSPM>IMCCE
;			- Make this routine independant from final purpose. Now, just 
;			  compute shift and angle values, and give it to the procedure caller.
;			- Add 'ROTATION_CENTER' to set his own center.
;-------------------------------------------------------------------------------	
PRO caviar_xmove_mouse, winID, $
						TFACTOR=tFactor, $
						RFACTOR=rFactor, $
						RCENTER=centerVec
    
    PRINT, "Press a button and move the mouse over the window:"            	
	IF KEYWORD_SET(tFactor) THEN PRINT, "    Left button   => translate
	IF KEYWORD_SET(rFactor) THEN PRINT, "    Middle button => rotate 
	PRINT, "    Right button  => quit"
	
	WSET, winID
	
	IF NOT KEYWORD_SET(centerVec) THEN centerVec=[!D.X_SIZE/2.0D, !D.Y_SIZE/2.0D]
	CURSOR, xmA, ymA, /DEVICE
	ptA = [xmA, ymA]
	
	working=1
	WHILE working EQ 1 DO BEGIN
		
		CURSOR, xmB, ymB, /NOWAIT, /DEVICE
		ptB = [xmB, ymB]
		
		; Left click -> compute translation vector:
		IF !mouse.button EQ	1 && ~ARRAY_EQUAL(ptA,ptB) && KEYWORD_SET(tFactor) $
		THEN working = caviar_xmove_callback(TRANSLATE = tFactor * [ptB[0]-ptA[0], ptB[1]-ptA[1]])
		
		; Left click -> compute rotation angle
		IF !mouse.button EQ	2 && ~ARRAY_EQUAL(ptA,ptB) && KEYWORD_SET(rFactor) THEN BEGIN
			ptA_c = ptA - centerVec
			ptB_c = ptB - centerVec
			
			angle = atan(ptB_c[1]/ptB_c[0])-atan(ptA_c[1]/ptA_c[0])
			IF angle LT -!DPI/2 THEN angle += !DPI
			IF angle GT  !DPI/2 THEN angle -= !DPI
			
			working = caviar_xmove_callback(ROTATE=rFactor*angle)
		ENDIF
		
		;Right click -> quit the loop and stop the procedure
		IF !mouse.button EQ 4 THEN working=0	
		
		; Mouse position B become mouse position A
		ptA = ptB
	ENDWHILE
	
	RETURN
END

;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
; NAME: CAVIAR_XMOVE_EVENT
; PURPOSE: Manage events from the widget
;-------------------------------------------------------------------------------		  
PRO caviar_xmove_event, event
	
	; Get state from the first child of the compound widget root:
	WIDGET_CONTROL, event.HANDLER, GET_UVALUE=pstate
	
	uname = WIDGET_INFO(event.id, /UNAME)
	CASE uname OF
		
		"MOVE": BEGIN
			CASE event.type OF
				1:  res = caviar_xmove_callback(TRANSLATE = event.value)
				2:  res = caviar_xmove_callback(TRANSLATE = event.value)
				3:  res = caviar_xmove_callback(TRANSLATE = event.value)
				4:  res = caviar_xmove_callback(TRANSLATE = event.value)
				5:  caviar_xmove_mouse, (*pstate).winID, $
										TFACTOR=event.value.tFactor, $
							  			RFACTOR=event.value.rFactor	
				6:  res = caviar_xmove_callback(TRANSLATE = event.value)
				7:  res = caviar_xmove_callback(TRANSLATE = event.value)
				8:  res = caviar_xmove_callback(TRANSLATE = event.value)
				9:  res = caviar_xmove_callback(TRANSLATE = event.value)
				11: res = caviar_xmove_callback(ROTATE = event.value*!DTOR)
				12: res = caviar_xmove_callback(ROTATE = event.value*!DTOR)
				ELSE:
			ENDCASE
		END
				
		ELSE:
	ENDCASE
	
END

;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;
;
;-------------------------------------------------------------------------------
PRO caviar_xmove, winID, TRANSLATE=translate, ROTATE=rotate, $
				  PARENT=wMainBase, GROUP_LEADER=groupLeader, $
				  PROC2CALL_NAME=proc2call_name, PROC2CALL_DATA=proc2call_data, $
				  COLUMN=column, ROW=row, FRAME=frame, TITLE=title, $
				  XOFFSET = xoffset, XPAD = xpad, XSIZE = xsize, X_SCROLL_SIZE = x_scroll_size, $
    			  YOFFSET = yoffset, YPAD = ypad, YSIZE = ysize, Y_SCROLL_SIZE = y_scroll_size 

	COMMON CAVIAR_XMOVE, cbName, cbData
	
	; Test if widget window is already launched
	IF XRegistered('caviar_xmove') NE 0 THEN RETURN
	
	;********************************* Keywords ********************************
	IF ~KEYWORD_SET(translate) && ~KEYWORD_SET(rotate) THEN translate=(rotate=1) $
	ELSE BEGIN
		translate = KEYWORD_SET(translate)
		rotate    = KEYWORD_SET(rotate)
	ENDELSE
	col=0 & row=0
	IF translate && rotate THEN row=1 ELSE col=1
	IF ~KEYWORD_SET(frame) THEN frame=0
	
	IF KEYWORD_SET(proc2call_name) THEN cbName=proc2call_name ELSE cbName=''
	IF KEYWORD_SET(proc2call_data) THEN cbData=proc2call_data
	
	version = WIDGET_INFO(/version)
  	IF version.toolkit EQ 'OLIT' THEN def_space_pad=4 ELSE def_space_pad=3
	IF N_ELEMENTS(space) EQ 0         THEN space = def_space_pad
	IF N_ELEMENTS(xoffset) EQ 0       THEN xoffset=0
	IF N_ELEMENTS(xpad) EQ 0          THEN xpad = def_space_pad
	IF N_ELEMENTS(xsize) EQ 0         THEN xsize = 0
	IF N_ELEMENTS(x_scroll_size) EQ 0 THEN x_scroll_size = 0
	IF N_ELEMENTS(yoffset) EQ 0       THEN yoffset=0
	IF N_ELEMENTS(ypad) EQ 0          THEN ypad = def_space_pad
	IF N_ELEMENTS(ysize) EQ 0         THEN ysize = 0	
	IF N_ELEMENTS(title) EQ 0		  THEN title="XMOVE"
	
	;***************************************************************************
	; Define the widget base and subwidgets tree
	;***************************************************************************
	IF KEYWORD_SET(wMainBase) THEN BEGIN
		IF NOT WIDGET_INFO(wMainBase, /VALID_ID) $
		THEN res=DIALOG_MESSAGE('Invalid parent widget identifier.',/ERROR)
		isTopBase = 0
	ENDIF ELSE BEGIN
		isTopBase = 1
		IF KEYWORD_SET(groupLeader) THEN BEGIN
			IF NOT WIDGET_INFO(groupLeader, /VALID_ID) $
			THEN res=DIALOG_MESSAGE('Invalid group leader widget identifier.',/ERROR)
			wMainBase = WIDGET_BASE(TITLE=title, XOFFSET=xoffset, YOFFSET=yoffset, $
								GROUP_LEADER=groupLeader, /FLOATING)
		ENDIF ELSE $
			wMainBase = WIDGET_BASE(TITLE=title, XOFFSET=xoffset, YOFFSET=yoffset)
	ENDELSE
	
	
	wExtraBase = WIDGET_BASE(wMainBase, /COLUMN, /ALIGN_CENTER, /BASE_ALIGN_CENTER, $
					FRAME=frame, XPAD=xpad, YPAD=ypad, SPACE=space)
		
		
		wMove = CW_MOVE(wExtraBase, UNAME="MOVE", FRAME=frame, COLUMN=col, ROW=row)
	
	; Create the widget (i.e. display it)
	WIDGET_CONTROL, wMainBase, /REALIZE

	
	; Set 'extra' base value as 'state' structure with all needed widgets parameters:
	state = {winID:winID}
	
	; Copy 'state' structure in extra widget uvalue
	WIDGET_CONTROL, wExtraBase, SET_UVALUE=PTR_NEW(state, /NO_COPY)
	
	; Let XMANAGER takes control of widgets event processing
	XMANAGER, 'caviar_xmove', wExtraBase, /JUST_REG
END			
